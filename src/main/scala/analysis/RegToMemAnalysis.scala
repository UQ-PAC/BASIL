package analysis

import ir.{MemoryLoad, *}
import analysis.solvers.*

import scala.collection.immutable

/**
 * Collects all the memory loads and the expressions that are assigned to a register but cannot be evaluated.
 *
 * Tracks:
 * R_x = MemoryLoad[Base + Offset]
 * R_x = Base + Offset
 *
 * Both in which constant propagation mark as TOP which is not useful.
 */
trait RegionAccessesAnalysis(cfg: ProgramCfg, constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], reachingDefs: Map[CFGPosition, (Map[Variable, Set[LocalAssign]], Map[Variable, Set[LocalAssign]])]) {

  val mapLattice: MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]] = MapLattice(FlatLattice[_root_.ir.Expr]())

  val lattice: MapLattice[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]], MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]]] = MapLattice(mapLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  /** Default implementation of eval.
   */
  def eval(cmd: CfgCommandNode, constants: Map[Variable, FlatElement[BitVecLiteral]], s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = {
    cmd.data match {
      case localAssign: LocalAssign =>
        localAssign.rhs match {
          case memoryLoad: MemoryLoad =>
            s + (RegisterWrapperPartialEquality(localAssign.lhs, getDefinition(localAssign.lhs, cmd.data, reachingDefs)) -> FlatEl(memoryLoad))
          case binaryExpr: BinaryExpr =>
            if (evaluateExpression(binaryExpr.arg1, constants).isEmpty) { // approximates Base + Offset
              s + (RegisterWrapperPartialEquality(localAssign.lhs, getDefinition(localAssign.lhs, cmd.data, reachingDefs)) -> FlatEl(binaryExpr))
            } else {
              s
            }
          case _ => s
        }
      case _ =>
        s
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = n match {
    case cmd: CfgCommandNode =>
      eval(cmd, constantProp(cmd.data), s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CfgNode, s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = localTransfer(n, s)
}

class RegionAccessesAnalysisSolver(
                         cfg: ProgramCfg,
                         constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                         reachingDefs: Map[CFGPosition, (Map[Variable, Set[LocalAssign]], Map[Variable, Set[LocalAssign]])],
                       ) extends RegionAccessesAnalysis(cfg, constantProp, reachingDefs)
  with InterproceduralForwardDependencies
  with Analysis[Map[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]]]]
  with SimpleWorklistFixpointSolver[CfgNode, Map[RegisterWrapperPartialEquality, FlatElement[Expr]], MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]]] {
}