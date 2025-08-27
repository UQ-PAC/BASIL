package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*

abstract class LivenessAnalysis(program: Program, addExternals: Boolean = true) extends Analysis[Any] {
  val lattice: MapLattice[CFGPosition, Set[Variable]] = MapLattice()
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    n match {
      case p: Procedure => s
      case b: Block => s
      case SimulAssign(assignments, _) =>
        (s -- assignments.map(_._1).toSet) ++ assignments.flatMap(_._2.variables).toSet
      case a: MemoryAssign => (s - a.lhs) ++ a.rhs.variables
      case m: MemoryStore => s ++ m.index.variables ++ m.value.variables
      case m: MemoryLoad => (s - m.lhs) ++ m.index.variables
      case a: Assume => s ++ a.body.variables
      case a: Assert => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall if addExternals && (c.target.isExternal.contains(true) || c.target.blocks.isEmpty) => {
        val writes = ir.transforms.externalCallWrites(c.target.procName).toSet[Variable]
        val reads = ir.transforms.externalCallReads(c.target.procName).toSet[Variable]
        s -- writes ++ reads
      }
      case c: DirectCall => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo => s
      case r: Return => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable => s
      case n: NOP => s
    }
  }
}

class IntraLiveVarsAnalysis(program: Program)
    extends LivenessAnalysis(program, false)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable]]
    with IRIntraproceduralBackwardDependencies
