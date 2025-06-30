package analysis

import analysis.solvers.SimplePushDownWorklistFixpointSolver
import ir.{
  CFGPosition,
  DirectCall,
  GlobalVar,
  IntraProcIRCursor,
  LocalAssign,
  MemoryLoad,
  Procedure,
  Program,
  Register,
  Variable,
  computeDomain
}

abstract class ReachingDefs(program: Program, writesTo: Map[Procedure, Set[GlobalVar]])
    extends Analysis[Map[CFGPosition, Map[Variable, Set[CFGPosition]]]] {

  val mallocRegister = Register("R0", 64)
  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, program.procedures).toSet
  val lattice: MapLattice[CFGPosition, Map[Variable, Set[CFGPosition]], MapLattice[Variable, Set[
    CFGPosition
  ], PowersetLattice[CFGPosition]]] = MapLattice(MapLattice(PowersetLattice[CFGPosition]()))

  def transfer(n: CFGPosition, s: Map[Variable, Set[CFGPosition]]): Map[Variable, Set[CFGPosition]] = {
    n match {
      case loc: LocalAssign =>
        s + (loc.lhs -> Set(n))
      case load: MemoryLoad =>
        s + (load.lhs -> Set(n))
      case DirectCall(target, _, _, _) if target.procName == "malloc" =>
        s + (mallocRegister -> Set(n))
      case DirectCall(target, _, _, _) if writesTo.contains(target) =>
        val result: Map[Variable, Set[CFGPosition]] = writesTo(target).foldLeft(Map[Variable, Set[CFGPosition]]()) {
          (m, register) =>
            m + (register -> Set(n))
        }
        s ++ result
      case _ => s
    }
  }

}

class ReachingDefsAnalysis(program: Program, writesTo: Map[Procedure, Set[GlobalVar]])
    extends ReachingDefs(program, writesTo),
      IRIntraproceduralForwardDependencies,
      SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, Set[CFGPosition]], MapLattice[Variable, Set[
        CFGPosition
      ], PowersetLattice[CFGPosition]]]
