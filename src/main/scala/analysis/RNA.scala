package analysis

import analysis.solvers.*
import ir.*

import scala.collection.immutable

/**
 * Calculates the set of variables that are read but not written in a given program.
 * This helps to identify the set of variables that are read from memory before they have been initialised.
 * This could be used on callee side to identify what parameters where passed to the function.
 */
trait RNAAnalysis(program: Program, ignoreStackPtrs: Boolean = true) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] = if (ignoreStackPtrs) then Set(linkRegister, framePointer, stackPointer) else Set()

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Set[Variable]): Set[Variable] = {
    var m = s
    cmd match {
      case assume: Assume =>
        m.union(assume.body.variables.filter(!ignoreRegions.contains(_)))
      case assert: Assert =>
        m.union(assert.body.variables.filter(!ignoreRegions.contains(_)))
      case memoryAssign: MemoryAssign =>
        m.union((memoryAssign.index.variables ++ memoryAssign.value.variables).filter(!ignoreRegions.contains(_)))
      case indirectCall: IndirectCall =>
        if (ignoreRegions.contains(indirectCall.target)) return m
        m + indirectCall.target
      case assign: Assign =>
        m = m - assign.lhs
        m.union(assign.rhs.variables.filter(!ignoreRegions.contains(_)))
      case _ =>
        m
    }
  }

  /** Transfer function for state lattice elements.
    */
  def localTransfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = n match {
    case cmd: Command =>
      eval(cmd, s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
      */
  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = localTransfer(n, s)
}

class RNAAnalysisSolver(
    program: Program,
    ignoreStackPtrs: Boolean = true,
) extends RNAAnalysis(program, ignoreStackPtrs)
    with IRIntraproceduralBackwardDependencies
    with Analysis[Map[CFGPosition, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]] {
}
