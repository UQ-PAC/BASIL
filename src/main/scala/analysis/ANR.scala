package analysis

import analysis.solvers.*
import ir.*

import scala.collection.immutable

/** Calculates the set of variables that are not read after being written up to that point in the program. Useful for
  * detecting dead stores, constants and which variables are passed as parameters in a function call.
  */
trait ANRAnalysis(program: Program, ignoreStackPtrs: Boolean = false) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Expr] =
    if (ignoreStackPtrs) then Set(linkRegister, framePointer, stackPointer) else Set()

  /** Default implementation of eval.
    */
  def eval(cmd: Command, s: Set[Variable]): Set[Variable] = {
    cmd match {
      case assume: Assume =>
        s.diff(assume.body.variables)
      case assert: Assert =>
        s.diff(assert.body.variables)
      case memoryStore: MemoryStore =>
        s.diff(memoryStore.index.variables ++ memoryStore.value.variables)
      case indirectCall: IndirectCall =>
        s - indirectCall.target
      case call: DirectCall =>
        s.diff(call.actualParams.flatMap(_._2.variables).toSet.filterNot(ignoreRegions.contains(_)))
          ++ call.outParams.map(_._2).toSet
      case assign: LocalAssign =>
        val m = s.diff(assign.rhs.variables)
        if (ignoreRegions.contains(assign.lhs)) {
          m
        } else {
          m + assign.lhs
        }
      case memoryLoad: MemoryLoad =>
        val m = s.diff(memoryLoad.index.variables)
        if (ignoreRegions.contains(memoryLoad.lhs)) {
          m
        } else {
          m + memoryLoad.lhs
        }
      case _ =>
        s
    }
  }

  /** Transfer function for state lattice elements.
    */
  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = n match {
    case cmd: Command =>
      eval(cmd, s)
    case _ => s // ignore other kinds of nodes
  }
}

class ANRAnalysisSolver(program: Program, ignoreStack: Boolean = true)
    extends ANRAnalysis(program, ignoreStack)
    with IRIntraproceduralForwardDependencies
    with Analysis[Map[CFGPosition, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]] {}
