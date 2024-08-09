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

trait IRInterproceduralForwardDependencies extends Dependencies[CFGPosition] {
  override def outdep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.succ(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.pred(n)
}

trait IRIntraproceduralForwardDependencies extends Dependencies[CFGPosition] {
  override def outdep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.succ(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.pred(n)
}

trait IRInterproceduralBackwardDependencies extends Dependencies[CFGPosition]  {
  override def outdep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.pred(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = InterProcIRCursor.succ(n)
}

trait IRIntraproceduralBackwardDependencies extends Dependencies[CFGPosition]  {
  override def outdep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.pred(n)
  override def indep(n: CFGPosition): Set[CFGPosition] = IntraProcIRCursor.succ(n)
}
