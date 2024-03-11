package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, Block, CFGPosition, Call, DirectCall, GoTo, IndirectCall, Jump, LocalAssign, MemoryAssign, NOP, Procedure, Program, Statement, Variable, Return}

abstract class LivenessAnalysis(program: Program) extends Analysis[Any]:
  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(PowersetLattice())
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    n match {
      case p: Procedure => s
      case b: Block => s
      case LocalAssign(variable, expr, _) => (s - variable) ++ expr.variables
      case MemoryAssign(_, store, _) => s ++ store.index.variables ++ store.value.variables
      case Assume(expr, _, _, _) => s ++ expr.variables
      case Assert(expr, _, _) => s ++ expr.variables
      case IndirectCall(variable, _, _) => s + variable
      case c: DirectCall => s
      case g: GoTo => s
      case r: Return => s
      case _ => s
    }
  }

class IntraLiveVarsAnalysis(program: Program)
  extends LivenessAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
    with IRIntraproceduralBackwardDependencies