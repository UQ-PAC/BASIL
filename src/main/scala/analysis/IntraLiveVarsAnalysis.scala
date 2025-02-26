package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{
  Assert,
  Assume,
  Block,
  CFGPosition,
  Call,
  DirectCall,
  GoTo,
  IndirectCall,
  Jump,
  LocalAssign,
  MemoryLoad,
  MemoryStore,
  Procedure,
  Program,
  Statement,
  Variable,
  Return,
  Unreachable
}

abstract class LivenessAnalysis(program: Program) extends Analysis[Any] {
  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(PowersetLattice())
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    n match {
      case _: Procedure => s
      case _: Block => s
      case LocalAssign(variable, expr, _) => (s - variable) ++ expr.variables
      case MemoryStore(_, index, value, _, _, _) => s ++ index.variables ++ value.variables
      case MemoryLoad(lhs, _, index, _, _, _) => (s - lhs) ++ index.variables
      case Assume(expr, _, _, _) => s ++ expr.variables
      case Assert(expr, _, _) => s ++ expr.variables
      case IndirectCall(variable, _) => s + variable
      case _: DirectCall => s
      case _: GoTo => s
      case _: Return => s
      case _: Unreachable => s
      case _ => ???
    }
  }
}

class IntraLiveVarsAnalysis(program: Program)
    extends LivenessAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
    with IRIntraproceduralBackwardDependencies
