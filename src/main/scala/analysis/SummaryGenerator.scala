package analysis

import util.Logger

import analysis.*
import ir.*
import boogie.*
import boogie.SpecGlobal
import ir.transforms.{AbstractDomain, reversePostOrder, worklistSolver}

/** A temporary copy of RNA analysis which works on Taintables.
  */
import analysis.solvers.SimpleWorklistFixpointSolver

private trait RNATaintableAnalysis(
  program: Program,
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
) {
  val lattice: MapLattice[CFGPosition, Set[Taintable], PowersetLattice[Taintable]] = MapLattice(PowersetLattice())

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Variable] = Set(linkRegister, framePointer, stackPointer)

  def eval(cmd: Command, s: Set[Taintable]): Set[Taintable] = {
    cmd match {
      case assume: Assume =>
        s ++ assume.body.variables -- ignoreRegions
      case assert: Assert =>
        s ++ assert.body.variables -- ignoreRegions
      case memoryStore: MemoryStore =>
        val m = s -- getMemoryVariable(cmd, memoryStore.mem, memoryStore.index, memoryStore.size, constProp, globals)
        m ++ memoryStore.index.variables ++ memoryStore.value.variables -- ignoreRegions
      case indirectCall: IndirectCall =>
        if (ignoreRegions.contains(indirectCall.target)) {
          s
        } else {
          s + indirectCall.target -- ignoreRegions
        }
      case assign: LocalAssign =>
        val m = s - assign.lhs
        m ++ assign.rhs.variables -- ignoreRegions
      case memoryLoad: MemoryLoad =>
        val m = s - memoryLoad.lhs
        val memvar = getMemoryVariable(cmd, memoryLoad.mem, memoryLoad.index, memoryLoad.size, constProp, globals)
        m ++ memvar ++ memoryLoad.index.variables -- ignoreRegions
      case _ => s
    }
  }

  def transfer(n: CFGPosition, s: Set[Taintable]): Set[Taintable] = n match {
    case cmd: Command =>
      eval(cmd, s)
    case _ => s // ignore other kinds of nodes
  }
}

private class RNATaintableSolver(
  program: Program,
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
) extends RNATaintableAnalysis(program, globals, constProp)
    with IRIntraproceduralBackwardDependencies
    with Analysis[Map[CFGPosition, Set[Taintable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Taintable], PowersetLattice[Taintable]]

/** Generates summaries (requires and ensures clauses) for procedures that necessarily must hold (assuming a correct
  * implementation). This helps because the verifier cannot make any assumptions about procedures with no summaries.
  */
class SummaryGenerator(
  program: Program,
  specGlobals: Set[SpecGlobal],
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]]
) {
  private val rnaResults = RNATaintableSolver(program, globals, constProp).analyze()

  // registers R19 to R29 and R31 must be preserved by a procedure call, so we only need summaries for R0-R18 and R30
  // TODO support R30? the analysis currently ignores it?
  val variables: Set[Taintable] = 0
    .to(18)
    .map { n =>
      Register(s"R$n", 64)
    }
    .toSet ++ specGlobals.map { g =>
    analysis.GlobalVariable(dsl.mem, BitVecLiteral(g.address, 64), g.size, g.name)
  }

  private def toGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: Register => Some(variable.toGamma)
      case variable: LocalVar => Some(variable.toGamma)
      case variable: GlobalVariable => Some(variable.toGamma)
      // case variable: LocalStackVariable => None
      case _: UnknownMemory => Some(FalseBLiteral)
    }
  }

  private def toExprGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: GlobalVariable => Some(BinaryBExpr(BoolOR, variable.toGamma, variable.L))
      case _ => toGamma(variable)
    }
  }

  /** Get a map of variables to variables which have tainted it in the procedure.
    */
  private def getTainters(procedure: Procedure, variables: Set[Taintable]): Map[Taintable, Set[Taintable]] = {
    varDepsSummaries.getOrElse(procedure, Map())
  }

  /**
   * Gets the set of gammas stored in a VarGammaMap, if possible
   */
  private def relevantGammas(gammaMap: VarGammaMap, v: Taintable): Option[Set[Taintable]] = {
    gammaMap(v) match {
      case LatticeSet.Top() => None // We can't know all of the variables, so we soundly say nothing
      case LatticeSet.Bottom() => Some(Set())
      case LatticeSet.FiniteSet(s) => Some(s)
      case LatticeSet.DiffSet(_) => None
    }
  }

  /** Generate requires clauses for a procedure. Currently we can generate the requirement that the variables that
    * influence the gamma of the first branch condition must be low.
    */
  def generateRequires(procedure: Procedure): List[BExpr] = {
    if procedure.blocks.isEmpty then return List()

    val mustGammaDomain = MustGammaDomain(globals, constProp)
    val predDomain = PredicateDomain()
    val initialState = LatticeMap.TopMap((variables ++ procedure.formalInParam).map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    reversePostOrder(procedure)
    val (_, mustGammaResults) = worklistSolver(MustGammaDomain(globals, constProp)).solveProc(procedure, false)
    val (before, after) = worklistSolver(ReachabilityConditions()).solveProc(procedure, false)
    val (predDomainResults, _) = worklistSolver(predDomain).solveProc(procedure, true)
    Logger.debug(predDomainResults.map((a, b) => (a, eval.simplifyExprFixpoint(b.simplify.toBasil.get)._1.toBoogie)))

    val mustGammasWithConditions = procedure.blocks.flatMap(b => {
      b.statements.flatMap(s => {
        s match {
          case a: Assume if a.checkSecurity => {
            val condition = a.parent.prevBlocks.foldLeft(TrueBLiteral: BExpr)((p, b) =>
                BinaryBExpr(BoolAND, p, ReachabilityConditions().toPred(before(b)).toBoogie.simplify))
            a.body.variables.foldLeft(Some(Set()): Option[Set[BExpr]]) {
              (s, v) => {
                relevantGammas(mustGammaResults(a.parent), v).flatMap(r => s.map(s => s ++ r.flatMap(toGamma)))
              }
            }.flatMap {
              gammas => {
                (if (gammas.size > 1) {
                  Some(gammas.tail.foldLeft(gammas.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next)))
                } else if (gammas.size == 1) {
                  Some(gammas.head)
                } else {
                  None
                }).map(prop => BinaryBExpr(BoolIMPLIES, condition, prop))
              }
            }
          }
          case _ => List()
        }
      })
    }).flatMap(p => {
      Logger.debug("Simplified " + p.toString() + " into " + p.simplify.toString())
      p.simplify match {
        case TrueBLiteral => None
        case p => Some[BExpr](p)
      }
    }).toSet.toList

    val wpThing = procedure.entryBlock.flatMap(b => predDomainResults.get(b).flatMap(p => p.toBasil).map(p => eval.simplifyExprFixpoint(p)._1.toBoogie)).toList

    mustGammasWithConditions ++ wpThing
  }

  /** Generate ensures clauses for a procedure. Currently, all generated ensures clauses are of the form (for example)
    * ensures Gamma_R2 || (Gamma_R2 == old(Gamma_y)) || (Gamma_R2 == old(Gamma_R1)) || (Gamma_R2 == old(Gamma_R2));
    * whenever this can be done soundly.
    */
  def generateEnsures(procedure: Procedure): List[BExpr] = {
    if procedure.blocks.isEmpty then return List()

    // We only need to make ensures clauses about the gammas of modified variables.
    val relevantVars = (variables ++ procedure.formalOutParam).filter { v =>
      v match {
        case v: Global => procedure.modifies.contains(v)
        case v: LocalVar => procedure.formalOutParam.contains(v)
        case _ => true
      }
    }

    // TODO further explanation of this would help
    // Use rnaResults to find stack function arguments
    val tainters = relevantVars.map {
      v => (v, Set())
    }.toMap ++ getTainters(procedure, variables ++ procedure.formalInParam ++ rnaResults(IRWalk.firstInProc(procedure).get) + UnknownMemory()).filter { (variable, taints) =>
      relevantVars.contains(variable)
    }

    Logger.debug("For " + procedure.toString)
    Logger.debug(relevantVars)
    Logger.debug(tainters)

    tainters.toList.flatMap {
      (variable, taints) => {
        // If our variable was tainted by memory that we know nothing about, it is sound to assume that we
        // know nothing about its gamma in the post state.
        if taints.contains(UnknownMemory()) then None
        else
          toGamma(variable).flatMap { varGamma =>
            {
              if taints.isEmpty then {
                // If the variable is tainted by nothing, it must have been set to some constant value, meaning
                // its gamma is low. Note that if a variable was unchanged, it would be tainted by itself.
                Some(varGamma)
              } else {
                // Else, we must consider the variables which have tainted this variable. We can soundly assert
                // that this variable's gamma must be equal to one of its tainter's gammas, or it is set to
                // low.
                //
                // In a correct procedure, we never have L(x) = true and Gamma_x = false, so we can safely
                // assume L(x) || Gamma_x == Gamma_x. This means that the gamma of any expression is equal
                // to the conjunction of the gammas of the variables and the loads in the expression.
                val equations = taints.flatMap { tainter =>
                  toGamma(tainter).map { tainterGamma =>
                    BinaryBExpr(BoolEQ, varGamma, Old(tainterGamma))
                  }
                }

                if equations.nonEmpty then {
                  Some(equations.foldLeft(varGamma) { (expr, eq) =>
                    BinaryBExpr(BoolOR, expr, eq)
                  })
                } else {
                  None
                }
              }
            }
          }
      }
    }
  }
}
