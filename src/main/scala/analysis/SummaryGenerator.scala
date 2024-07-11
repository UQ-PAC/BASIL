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

class SummaryGenerator(
    program: Program,
    specGlobals: Set[SpecGlobal],
    globals: Map[BigInt, String],
    mmm: MemoryModelMap,
    constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]
) {
  val rnaResults = RNATaintableSolver(program, globals, mmm, constProp).analyze()

  // TODO should the stack/return/frame pointers propagate taint?
  val variables: Set[analysis.Taintable] = (0 to 28).map { n =>
    Register(s"R$n", 64)
  }.toSet ++ specGlobals.map { g =>
    analysis.GlobalVariable(dsl.mem, BitVecLiteral(g.address, 64), g.size, g.name)
  }

  private def toGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: Variable       => Some(variable.toGamma)
      case variable: GlobalVariable => Some(variable.toGamma)
      // TODO It is not the case that stack variables are necessarily local variables, since they could also
      //      be (for examples) function arguments. How should these variables be detected? (RNA analysis ?)
      case variable: LocalStackVariable => None
      case variable: UnknownMemory      => Some(FalseBLiteral)
    }
  }

  private def toExprGamma(variable: Taintable): Option[BExpr] = {
    variable match {
      case variable: GlobalVariable => Some(BinaryBExpr(BoolOR, variable.toGamma, variable.L))
      case _                        => toGamma(variable)
    }
  }

  private def getTaints(procedure: Procedure, variables: Set[Taintable]): Map[Taintable, Set[Taintable]] = {
    variables.flatMap { variable =>
      {
        val tainted: Map[CFGPosition, Set[Taintable]] = Map(procedure -> Set(variable))
        val taintResults = TaintAnalysis(program, globals, mmm, constProp, tainted).analyze()
        taintResults.get(procedure.end).map { m =>
          {
            val taintedVars: Set[Taintable] = m.flatMap { (v, t) =>
              v match
                case v: LocalVar => None
                case _           => if t == TwoElementTop then Some(v) else None
            }.toSet
            (variable, taintedVars)
          }
        }
      }
    }.toMap
  }

  private def getTainters(procedure: Procedure, variables: Set[Taintable]): Map[Taintable, Set[Taintable]] = {
    getTaints(procedure, variables).foldLeft(variables.map { v => (v, Set[Taintable]()) }.toMap) { (m, taints) =>
      {
        val (variable, tainted) = taints
        tainted.foldLeft(m) { (m, t) =>
          {
            m + (t -> (m.getOrElse(t, Set()) + variable))
          }
        }
      }
    }
  }

  def generateRequires(procedure: Procedure): List[BExpr] = List()

  def generateEnsures(procedure: Procedure): List[BExpr] = {
    // We only need to make postconditions about the gammas of modified variables.
    val relevantVars = variables.filter { v =>
      v match {
        case v: Global => procedure.modifies.contains(v)
        case _         => true
      }
    }

    // `tainters` is a mapping from a relevant variable to all input variables (or globals) that have
    // affected (tainted) it in the procedure. We need the rnaResults to find stack function arguments
    val tainters = getTainters(procedure, variables ++ rnaResults(procedure.begin) + UnknownMemory()).filter { (variable, taints) =>
      relevantVars.contains(variable)
    }

    tainters.toList
      .flatMap { (variable, taints) =>
        {
          Logger.debug(variable)
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
                  // assume L(x) || Gamma_x == Gamma_x. Thus, the gamma of any memory load in an expression is
                  // just the gamma of the loaded variable.
                  //
                  // Our postconditions are sound because any taint propagation from an assignment comes with a
                  // gamma assignment, where we take the conjunction of all variables in the expression
                  // (including memory variables!), and it is true that the conjunction of a set of booleans is
                  // equal to one of the booleans.
                  //
                  // Note that we cannot assert that the gamma of this variable is equal to the disjunction of
                  // all taint gammas, because of conditionals! If we had the following:
                  // ```c
                  // int x; L(x) = false
                  // int y; L(y) = (z == 0)
                  // int z; L(z) = true
                  //
                  // int fn() {
                  //     if (z) {
                  //         return x;
                  //     } else {
                  //         return y;
                  //     }
                  // }
                  // ```
                  // then the tainters of the return value of `fn` will be `{x, y}`, however the gamma of the
                  // return value is not necessarily equal to the disjunction of Gamma_x and Gamma_y: if we take
                  // Gamma_x == false, Gamma_y == true, z == 0 at the start of the procedure, the return gamma
                  // will be true.
                  //
                  // TODO we an probably make stronger postconditions than this.
                  // We could maybe gather expressions that the final value of a variable was assigned from, and
                  // work backwards from there?
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
      .map { expr =>
        {
          Logger.debug(s"Ensures DIRECT: \"$expr\"")
          expr
        }
      }
  }
}
