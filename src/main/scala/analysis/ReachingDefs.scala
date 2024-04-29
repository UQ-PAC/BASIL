package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, BitVecType, CFGPosition, Call, DirectCall, Expr, GoTo, IndirectCall, InterProcIRCursor, IntraProcIRCursor, LocalAssign, MemoryAssign, NOP, Procedure, Program, Register, Variable, computeDomain}

abstract class ReachingDefs(program: Program, writesTo: Map[Procedure, Set[Register]]) extends Analysis[Map[CFGPosition, Map[Variable, Set[CFGPosition]]]] {

  val mallocRegister = Register("R0", BitVecType(64))
  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, program.procedures).toSet
  val lattice: MapLattice[CFGPosition, Map[Variable, Set[CFGPosition]], MapLattice[Variable, Set[CFGPosition], PowersetLattice[CFGPosition]]] = new MapLattice(new MapLattice(new PowersetLattice[CFGPosition]()))

  def transfer(n: CFGPosition, s: Map[Variable, Set[CFGPosition]]): Map[Variable, Set[CFGPosition]] =
    if n.isInstanceOf[LocalAssign] && n.asInstanceOf[LocalAssign].label.get.startsWith("%000004f4") then
      print("")
    n match
      case loc:LocalAssign =>
        s + (loc.lhs -> Set(n))
      case DirectCall(proc, target, label) if proc.name == "malloc" =>
        s + (mallocRegister -> Set(n))
      case DirectCall(proc, target, label) if writesTo.contains(proc) =>
        val result: Map[Variable, Set[CFGPosition]] = writesTo(proc).foldLeft(Map[Variable, Set[CFGPosition]]()){
          (m, register) =>
            m + (register -> Set(n))
        }
        s ++ result
      case _ => s

}

class ReachingDefsAnalysis(program: Program, writesTo:  Map[Procedure, Set[Register]]) extends ReachingDefs(program, writesTo),  IRIntraproceduralForwardDependencies,
  SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Set[CFGPosition]], MapLattice[Variable, Set[CFGPosition], PowersetLattice[CFGPosition]]]

