package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*

abstract class LivenessAnalysis(program: Program) extends Analysis[Any] {
  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(PowersetLattice())
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    n match {
      case p: Procedure    => s
      case b: Block        => s
      case a: LocalAssign  => (s - a.lhs) ++ a.rhs.variables
      case m: MemoryStore  => s ++ m.index.variables ++ m.value.variables
      case m: MemoryLoad   => (s - m.lhs) ++ m.index.variables
      case a: Assume       => s ++ a.body.variables
      case a: Assert       => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall   => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo         => s
      case r: Return       => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable  => s
      case n: NOP          => s
    }
  }
}

class IntraLiveVarsAnalysis(program: Program)
    extends LivenessAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
    with IRIntraproceduralBackwardDependencies
