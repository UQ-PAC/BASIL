package analysis

import analysis.*
import ir.*
import boogie.*
import util.Logger
import specification.SpecGlobal

/**
 * A temporary copy of RNA analysis which works on Taintables.
 */
import analysis.solvers.SimpleWorklistFixpointSolver

private trait RNATaintableAnalysis(
  program: Program,
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
) {
  val lattice: MapLattice[ir.CFGPosition, Set[Taintable], PowersetLattice[Taintable]] = MapLattice(PowersetLattice())

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer, stackPointer)

  def eval(cmd: Command, s: Set[Taintable]): Set[Taintable] = {
    var m = s
    val exprs = cmd match {
      case assume: Assume =>
        Set(assume.body)
      case assert: Assert =>
        Set(assert.body)
      case memoryAssign: MemoryAssign =>
        m = m -- getMemoryVariable(cmd, memoryAssign.mem, memoryAssign.index, memoryAssign.size, constProp, globals)
        Set(memoryAssign.index, memoryAssign.value)
      case indirectCall: IndirectCall =>
        if (ignoreRegions.contains(indirectCall.target)) return m
        Set(indirectCall.target)
      case assign: Assign =>
        m = m - assign.lhs
        Set(assign.rhs)
      case _ => return m
    }

    exprs.foldLeft(m) {
      (m, expr) => {
        val vars = expr.variables.filter(!ignoreRegions.contains(_)).map { v => v: Taintable }
        val memvars: Set[Taintable] = expr.loads.flatMap {
          l => getMemoryVariable(cmd, l.mem, l.index, l.size, constProp, globals)
        }
        m.union(vars).union(memvars)
      }
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
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
) extends RNATaintableAnalysis(program, globals, mmm, constProp)
    with IRIntraproceduralBackwardDependencies
    with Analysis[Map[CFGPosition, Set[Taintable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Taintable], PowersetLattice[Taintable]]

/**
 * Generates summaries (requires and ensures clauses) for procedures that necessarily must hold (assuming a correct implementation).
 * This helps because the verifier cannot make any assumptions about procedures with no summaries.
 */
class SummaryGenerator(
    program: Program,
    specGlobals: Set[SpecGlobal],
    globals: Map[BigInt, String],
    mmm: MemoryModelMap,
    constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
) {
  val rnaResults = RNATaintableSolver(program, globals, mmm, constProp).analyze()

  // TODO should the stack/link/frame pointers propagate taint?
  val variables: Set[analysis.Taintable] = (0 to 28).map { n =>
    Register(s"R$n", 64)
  }.toSet ++ specGlobals.map { g =>
    analysis.GlobalVariable(dsl.mem, BitVecLiteral(g.address, 64), g.size, g.name)
  }

  private def toGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: Register => Some(variable.toGamma)
      case variable: LocalVar => None
      case variable: GlobalVariable => Some(variable.toGamma)
      case variable: LocalStackVariable => None
      case variable: UnknownMemory => Some(FalseBLiteral)
    }
  }

  private def toExprGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: GlobalVariable => Some(BinaryBExpr(BoolOR, variable.toGamma, variable.L))
      case _ => toGamma(variable)
    }
  }

  /**
   * Get a map of variables to variables which have tainted it in the procedure.
   */
  private def getTainters(procedure: Procedure, variables: Set[Taintable]): Map[Taintable, Set[Taintable]] = {
    VariableDependencyAnalysis(program, variables, globals, mmm, constProp, procedure).analyze().getOrElse(procedure.end, Map())
  }

  /**
   * Generate requires clauses for a procedure. Currently this does nothing.
   */
  def generateRequires(procedure: Procedure): List[BExpr] = List()

  /**
   * Generate ensures clauses for a procedure. Currently, all generated ensures clauses are of the form (for example)
   *   ensures Gamma_R2 || (Gamma_R2 == old(Gamma_y)) || (Gamma_R2 == old(Gamma_R1)) || (Gamma_R2 == old(Gamma_R2));
   * whenever this can be done soundly.
   */
  def generateEnsures(procedure: Procedure): List[BExpr] = {
    if procedure.blocks.isEmpty then return List()

    // We only need to make ensures clauses about the gammas of modified variables.
    val relevantVars = variables.filter { v =>
      v match {
        case v: Global => procedure.modifies.contains(v)
        case _ => true
      }
    }

    // Use rnaResults to find stack function arguments
    val tainters = getTainters(procedure, variables ++ rnaResults(procedure.begin) + UnknownMemory()).filter { (variable, taints) =>
      relevantVars.contains(variable)
    }

    tainters.toList
      .flatMap { (variable, taints) =>
      {
        // If our variable was tainted by memory that we know nothing about, it is sound to assume that we
        // know nothing about its gamma in the post state.
        if taints.contains(UnknownMemory()) then None
        else toGamma(variable).flatMap
          { varGamma => {
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

              if equations.size > 0 then {
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
