package analysis
import ir.{IRWalk, IntraProcIRCursor, InterProcIRCursor, CFGPosition}

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


class IRDependencies[NT <: CFGPosition](val walker: IRWalk[NT]) {
  trait ForwardDependencies extends  Dependencies[NT] {
    override def outdep(n: NT): Set[NT] = walker.succ(n)

    override def indep(n: NT): Set[NT] = walker.pred(n)
  }

  trait BackwardDependencies extends Dependencies[NT] {
    override def outdep(n: NT): Set[NT] = walker.pred(n)

    override def indep(n: NT): Set[NT] = walker.succ(n)
  }
}

object IRIntraproceduralDependencies extends IRDependencies[CFGPosition](IntraProcIRCursor)

object IRInterproceduralDependencies extends IRDependencies[CFGPosition](InterProcIRCursor)
