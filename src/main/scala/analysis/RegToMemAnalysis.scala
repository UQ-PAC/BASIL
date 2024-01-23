package analysis

import ir.{MemoryLoad, *}
import analysis.solvers.*

import scala.collection.immutable

/**
 * Calculates the set of variables that are not read after being written up to that point in the program.
 * Useful for detecting dead stores, constants and if what variables are passed as parameters in a function call.
 */
trait RegToMemAnalysis(cfg: ProgramCfg) {

  val mapLattice: MapLattice[RegisterVariableWrapper, FlatElement[MemoryLoad], FlatLattice[MemoryLoad]] = MapLattice(FlatLattice[_root_.ir.MemoryLoad]())

  val lattice: MapLattice[CfgNode, Map[RegisterVariableWrapper, FlatElement[MemoryLoad]], MapLattice[RegisterVariableWrapper, FlatElement[MemoryLoad], FlatLattice[MemoryLoad]]] = MapLattice(mapLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  /** Default implementation of eval.
   */
  def eval(cmd: Command, s: Map[RegisterVariableWrapper, FlatElement[MemoryLoad]]): Map[RegisterVariableWrapper, FlatElement[MemoryLoad]] = {
    var m = s
    cmd match {
      case localAssign: LocalAssign =>
        localAssign.rhs match {
          case memoryLoad: MemoryLoad =>
            m + (RegisterVariableWrapper(localAssign.lhs) -> FlatEl(memoryLoad).asInstanceOf[FlatElement[MemoryLoad]])
          case _ => m
        }
      case _ =>
        m
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: Map[RegisterVariableWrapper, FlatElement[MemoryLoad]]): Map[RegisterVariableWrapper, FlatElement[MemoryLoad]] = n match {
    case cmd: CfgCommandNode =>
      eval(cmd.data, s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CfgNode, s: Map[RegisterVariableWrapper, FlatElement[MemoryLoad]]): Map[RegisterVariableWrapper, FlatElement[MemoryLoad]] = localTransfer(n, s)
}

class RegToMemAnalysisSolver(
                         cfg: ProgramCfg,
                       ) extends RegToMemAnalysis(cfg)
  with InterproceduralForwardDependencies
  with Analysis[Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[MemoryLoad]]]]
  with SimpleWorklistFixpointSolver[CfgNode, Map[RegisterVariableWrapper, FlatElement[MemoryLoad]], MapLattice[RegisterVariableWrapper, FlatElement[MemoryLoad], FlatLattice[MemoryLoad]]] {
}