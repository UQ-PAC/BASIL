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
  def outdep(n: N): Iterable[N]

  /** Incoming dependencies. Used when computing the join from predecessors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that the given element depends on
    */
  def indep(n: N): Iterable[N]

trait InterproceduralForwardDependencies extends Dependencies[CfgNode] {
  override def outdep(n: CfgNode): Iterable[CfgNode] = n.succInter.toSet
  override def indep(n: CfgNode): Iterable[CfgNode] = n.predInter.toSet
}

trait IntraproceduralForwardDependencies extends Dependencies[CfgNode] {
  override def outdep(n: CfgNode): Iterable[CfgNode] = n.succIntra.toSet
  override def indep(n: CfgNode): Iterable[CfgNode] = n.predIntra.toSet
}


class IRDependencies[NT <: CFGPosition](val walker: IRWalk[NT, NT]) {
  trait ForwardDependencies extends  Dependencies[NT] {
    override def outdep(n: NT): Iterable[NT] = walker.succ(n)

    override def indep(n: NT): Iterable[NT] = walker.pred(n)
  }

  trait BackwardDependencies extends Dependencies[NT] {
    override def outdep(n: NT): Iterable[NT] = walker.pred(n)

    override def indep(n: NT): Iterable[NT] = walker.succ(n)
  }
}

object IRIntraproceduralDependencies extends IRDependencies[CFGPosition](IntraProcIRCursor)

trait Priorities[N]:
  implicit val ord: Ordering[N]

trait ReversePostOrder extends Priorities[CFGPosition]:
  implicit val priorities: Map[CFGPosition, Int]
  implicit val ord = Ordering.by(e => priorities(e))