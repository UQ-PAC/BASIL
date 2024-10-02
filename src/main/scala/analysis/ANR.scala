package analysis

import ir.*
import analysis.solvers._

import scala.collection.immutable

/**
 * Calculates the set of variables that are not read after being written up to that point in the program.
 * Useful for detecting dead stores, constants and if what variables are passed as parameters in a function call.
 */
trait ANRAnalysis(program: Program, ignoreStackPtrs: Boolean = false) {

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
        m.diff(assume.body.variables)
      case assert: Assert =>
        m.diff(assert.body.variables)
      case memoryAssign: MemoryAssign =>
        m.diff(memoryAssign.index.variables ++ memoryAssign.value.variables)
      case indirectCall: IndirectCall =>
        m - indirectCall.target
      case call: DirectCall =>
        m.diff(call.actualParams.flatMap(_._2.variables).toSet.filterNot(ignoreRegions.contains(_))) 
        ++ call.outParams.map(_._2).toSet
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

class ANRAnalysisSolver(program: Program, ignoreStack : Boolean = true) extends ANRAnalysis(program, ignoreStack)
    with IRIntraproceduralForwardDependencies
    with Analysis[Map[CFGPosition, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]] {
}
