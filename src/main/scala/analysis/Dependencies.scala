package analysis

import ir.{CFGPosition, InterProcIRCursor, IntraProcIRCursor}

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

trait IntraproceduralBackwardDependencies extends Dependencies[CfgNode] {
  def outdep(n: CfgNode): Set[CfgNode] = n.predIntra.toSet
  def indep(n: CfgNode): Set[CfgNode] = n.succIntra.toSet
}


trait IRInterproceduralForwardDependencies extends Dependencies[CFGPosition] {
  override def outdep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.succ(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.pred(n)
}

trait IRIntraproceduralForwardDependencies extends Dependencies[CFGPosition] {
  override def outdep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.succ(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.pred(n)
}

trait IRInterproceduralBackwardDependencies extends  IRInterproceduralForwardDependencies {
  override def outdep(n: CFGPosition): Set[CFGPosition] =  super.indep(n)
  override def indep(n: CFGPosition): Set[CFGPosition] =  super.outdep(n)
}

trait IRIntraproceduralBackwardDependencies extends IRIntraproceduralForwardDependencies {
  override def outdep(n: CFGPosition): Set[CFGPosition] = super.indep(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = super.outdep(n)
}
