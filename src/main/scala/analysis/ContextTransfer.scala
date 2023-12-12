package analysis
import ir.*
import util.Logger

import scala.collection.mutable

/** Steensgaard-style pointer analysis. The analysis associates an [[UnifiableTerm]] with each variable declaration and
  * expression node in the AST. It is implemented using [[tip.solvers.UnionFindSolver]].
  */
class ContextTransfer(
      cfg: ProgramCfg,
      constantProp: Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[mutable.Map[Procedure, mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]]]] {

  val functionMergedCtx = mutable.Map[Procedure, mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]]]()


  def mergeContexts(ctx1: Map[Variable, FlatElement[BitVecLiteral]],
                    ctx2: mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]] = mutable.Map.empty): mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]] = {
    val mergedCtx = mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]]()
    ctx1.foreach { case (v, e) => {
      val set = mutable.Set[FlatElement[BitVecLiteral]]()
      set += e
      mergedCtx(v) = set
    }}
    if (ctx2.isEmpty) return mergedCtx

    ctx2.foreach { case (v, e) => {
      val set = mergedCtx.getOrElse(v, mutable.Set[FlatElement[BitVecLiteral]]())
      set ++= e
      mergedCtx(v) = set
    }}
    mergedCtx
  }

  /** @inheritdoc
   */
  def analyze(): mutable.Map[Procedure, mutable.Map[Variable, mutable.Set[FlatElement[BitVecLiteral]]]] =
    // generate the constraints by traversing the AST and solve them on-the-fly
    cfg.nodes.foreach(n => visit(n, ()))
    functionMergedCtx

  /** Generates the constraints for the given sub-AST.
   *
   * @param node
   * the node for which it generates the constraints
   * @param arg
   * unused for this visitor
   */
  def visit(n: CfgNode, arg: Unit): Unit = {

    n match {
      case cfgJumpNode: CfgJumpNode => {
        cfgJumpNode.data match {
          case directCall: DirectCall => {
            val currentCtx = constantProp(n)
            val procedure = directCall.target
            val procedureCtx = functionMergedCtx.get(procedure)
            procedureCtx match {
              case Some(ctx) => {
                val mergedCtx = mergeContexts(currentCtx, ctx)
                functionMergedCtx(procedure) = mergedCtx
              }
              case None => {
                val mergedCtx = mergeContexts(currentCtx)
                functionMergedCtx(procedure) = mergedCtx
              }
            }
          }
        }
      }
      case _ => // do nothing
    }
  }
}