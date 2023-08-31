package analysis

/** Dependency methods for worklist-based analyses.
  */
trait Dependencies[N]:

  /** Outgoing dependencies. Used when propagating dataflow to successors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that depend on the given element
    */
  def outdep(n: N, intra: Boolean): Set[N]

  /** Incoming dependencies. Used when computing the join from predecessors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that the given element depends on
    */
  def indep(n: N, intra: Boolean): Set[N]

/** Dependency methods for forward analyses.
  */
trait ForwardDependencies extends Dependencies[CfgNode]:

  /* TODO: add functionality here for distinguishing between Intra / Inter */

  // Also add support for getting edges / conditions here?
  def outdep(n: CfgNode, intra: Boolean = true): Set[CfgNode] = if intra then n.succ(intra).toSet else n.succ(intra).toSet.union(n.succ(!intra).toSet)

  def indep(n: CfgNode, intra: Boolean = true): Set[CfgNode] = if intra then n.pred(intra).toSet else n.pred(intra).toSet.union(n.pred(!intra).toSet)
