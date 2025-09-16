package analysis

import analysis.solvers.*
import ir.*

/** Calculates the set of variables that are read but not written in a given program. This helps to identify the set of
  * variables that are read from memory before they have been initialised. This could be used on callee side to identify
  * what parameters where passed to the function.
  */
trait RNAAnalysis(program: Program, ignoreStack: Boolean = true) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegions: Set[Variable] =
    if ignoreStack then Set(linkRegister, framePointer, stackPointer) else Set()

  def eval(cmd: Command, s: Set[Variable]): Set[Variable] = {
    cmd match {
      case assume: Assume =>
        s ++ (assume.body.variables -- ignoreRegions)
      case assert: Assert =>
        s ++ (assert.body.variables -- ignoreRegions)
      case memoryStore: MemoryStore =>
        s ++ ((memoryStore.index.variables ++ memoryStore.value.variables) -- ignoreRegions)
      case call: DirectCall =>
        (s ++ call.actualParams.flatMap(_._2.variables).toSet.filterNot(ignoreRegions.contains(_)))
          .diff(call.outParams.map(_._2).toSet)
      case indirectCall: IndirectCall =>
        if (ignoreRegions.contains(indirectCall.target)) {
          s
        } else {
          s + indirectCall.target
        }
      case assign: LocalAssign =>
        val m = s - assign.lhs
        m ++ (assign.rhs.variables -- ignoreRegions)
      case memoryLoad: MemoryLoad =>
        val m = s - memoryLoad.lhs
        m ++ (memoryLoad.index.variables -- ignoreRegions)
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

class RNAAnalysisSolver(program: Program, ignoreStack: Boolean = true)
    extends RNAAnalysis(program, ignoreStack)
    with IRIntraproceduralBackwardDependencies
    with Analysis[Map[CFGPosition, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
