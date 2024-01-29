package analysis

import ir.{MemoryLoad, *}
import analysis.solvers.*

import scala.collection.immutable

/**
 * Calculates the set of variables that are not read after being written up to that point in the program.
 * Useful for detecting dead stores, constants and if what variables are passed as parameters in a function call.
 *
 * Tracks
 * R_x = MemoryLoad[Base + Offset]
 * R_x = Base + Offset
 *
 * Both in which constant propagation mark as TOP which is not useful.
 */
trait RegToMemAnalysis(cfg: ProgramCfg, constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]) {

  val mapLattice: MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]] = MapLattice(FlatLattice[_root_.ir.Expr]())

  val lattice: MapLattice[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] = MapLattice(mapLattice)

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  /** Default implementation of eval.
   */
  def eval(cmd: Command, constants:  Map[Variable, FlatElement[BitVecLiteral]], s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = {
    var m = s
    cmd match {
      case localAssign: LocalAssign =>
        localAssign.rhs match {
          case memoryLoad: MemoryLoad =>
            m + (RegisterVariableWrapper(localAssign.lhs) -> FlatEl(memoryLoad).asInstanceOf[FlatElement[Expr]])
          case binaryExpr: BinaryExpr =>
            if (evaluateExpression(binaryExpr.arg1, constants).isEmpty) {
              return m + (RegisterVariableWrapper(localAssign.lhs) -> FlatEl(binaryExpr).asInstanceOf[FlatElement[Expr]])
            }
            m
          case _ => m
        }
      case _ =>
        m
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CfgNode, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = n match {
    case cmd: CfgCommandNode =>
      eval(cmd.data, constantProp(n), s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CfgNode, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = localTransfer(n, s)
}

class RegToMemAnalysisSolver(
                         cfg: ProgramCfg,
                         constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]],
                       ) extends RegToMemAnalysis(cfg, constantProp)
  with InterproceduralForwardDependencies
  with Analysis[Map[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]]]]
  with SimpleWorklistFixpointSolver[CfgNode, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] {
}