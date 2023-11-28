package analysis
import ir.IntraProcIRCursor

/** Dependency methods for worklist-based analyses.
  */
trait Dependencies[N]:

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

trait InterproceduralForwardDependencies extends Dependencies[CfgNode] {
  override def outdep(n: CfgNode): Set[CfgNode] = n.succInter.toSet
  override def indep(n: CfgNode): Set[CfgNode] = n.predInter.toSet
}

trait IntraproceduralForwardDependencies extends Dependencies[CfgNode] {
  override def outdep(n: CfgNode): Set[CfgNode] = n.succIntra.toSet
  override def indep(n: CfgNode): Set[CfgNode] = n.predIntra.toSet
}


trait IntraProcDependencies extends Dependencies[IntraProcIRCursor.Node]:
  override def outdep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcDependencies.outdep(n)
  override def indep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcDependencies.indep(n)

/** Dependency methods for forward analyses.
  */
object IntraProcDependencies extends Dependencies[IntraProcIRCursor.Node]:
  override def outdep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcIRCursor.succ(n)
  override def indep(n: IntraProcIRCursor.Node): Set[IntraProcIRCursor.Node] = IntraProcIRCursor.pred(n)
