package analysis

import ir.*
import analysis.solvers._

import scala.collection.immutable

/**
 * Calculates the set of variables that are not read after being written up to that point in the program.
 * Useful for detecting dead stores, constants and if what variables are passed as parameters in a function call.
 */
trait ANRAnalysis(cfg: ProgramCfg) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CfgNode, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

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
  def localTransfer(n: CfgNode, s: Set[Variable]): Set[Variable] = n match {
    case cmd: CfgCommandNode =>
      eval(cmd.data, s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
      */
  def transfer(n: CfgNode, s: Set[Variable]): Set[Variable] = localTransfer(n, s)
}

class ANRAnalysisSolver(cfg: ProgramCfg) extends ANRAnalysis(cfg)
    with IntraproceduralForwardDependencies
    with Analysis[Map[CfgNode, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CfgNode, Set[Variable], PowersetLattice[Variable]] {
}