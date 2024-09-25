package analysis

import ir.*
import analysis.solvers._

import scala.collection.immutable

/**
 * Calculates the set of variables that are not read after being written up to that point in the program.
 * Useful for detecting dead stores, constants and if what variables are passed as parameters in a function call.
 */
trait ANRAnalysis(program: Program) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer, stackPointer)

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Set[Variable]): Set[Variable] = {
    var m = s
    cmd match {
      case assume: Assume =>
        m.diff(assume.body.variables)
      case assert: Assert =>
        m.diff(assert.body.variables)
      case memoryAssign: MemoryAssign =>
        m.diff(memoryAssign.index.variables)
      case indirectCall: IndirectCall =>
        m - indirectCall.target
      case assign: Assign =>
        m = m.diff(assign.rhs.variables)
        if ignoreRegions.contains(assign.lhs) then m else m + assign.lhs
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

class ANRAnalysisSolver(program: Program) extends ANRAnalysis(program)
    with IRIntraproceduralForwardDependencies
    with Analysis[Map[CFGPosition, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]] {
}
