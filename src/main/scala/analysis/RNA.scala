package analysis

import analysis.solvers.*
import ir.*

import scala.collection.immutable

/**
 * Calculates the set of variables that are read but not written in a given program.
 * This helps to identify the set of variables that are read from memory before they have been initialised.
 * This could be used on callee side to identify what parameters where passed to the function.
 */
trait RNAAnalysis(cfg: ProgramCfg) {

  val powersetLattice: PowersetLattice[Variable] = PowersetLattice()

  val lattice: MapLattice[CfgNode, Set[Variable], PowersetLattice[Variable]] = MapLattice(powersetLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))

  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer, stackPointer)

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
        m.union((memoryAssign.lhs.variables ++ memoryAssign.rhs.variables).filter(!ignoreRegions.contains(_)))
      case indirectCall: IndirectCall =>
        if (ignoreRegions.contains(indirectCall.target)) return m
        m + indirectCall.target
      case localAssign: LocalAssign =>
        m = m - localAssign.lhs
        m.union(localAssign.rhs.variables.filter(!ignoreRegions.contains(_)))
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

class RNAAnalysisSolver(
    cfg: ProgramCfg,
) extends RNAAnalysis(cfg)
    with IntraproceduralBackwardDependencies
    with Analysis[Map[CfgNode, Set[Variable]]]
    with SimpleWorklistFixpointSolver[CfgNode, Set[Variable], PowersetLattice[Variable]] {
}