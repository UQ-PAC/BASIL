package analysis
import ir.IntraProcIRCursor

/** Dependency methods for worklist-based analyses.
  */
trait Dependencies[N](val intra: Boolean):

  /** Outgoing dependencies. Used when propagating dataflow to successors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that depend on the given element
    */
  def outdep(n: N): Set[N]

  /** Incoming dependencies. Used when computing the join from predecessors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that the given element depends on
    */
  def indep(n: N): Set[N]

/** Dependency methods for forward analyses.
  */
trait ForwardDependencies extends Dependencies[CfgNode]:

  /* TODO: add functionality here for distinguishing between Intra / Inter */

  // Also add support for getting edges / conditions here?
  override def outdep(n: CfgNode): Set[CfgNode] =
    if intra then n.succ(intra).toSet else n.succ(intra).toSet.union(n.succ(!intra).toSet)

  override def indep(n: CfgNode): Set[CfgNode] =
    if intra then n.pred(intra).toSet else n.pred(intra).toSet.union(n.pred(!intra).toSet)



trait IntraProcDependencies extends Dependencies[IntraProcIRCursor.Node]:
  override def outdep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcDependencies.outdep(n)
  override def indep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcDependencies.indep(n)

/** Dependency methods for forward analyses.
  */
object IntraProcDependencies extends Dependencies[IntraProcIRCursor.Node](true):
  override def outdep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcIRCursor.succ(n)
  override def indep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcIRCursor.pred(n)
