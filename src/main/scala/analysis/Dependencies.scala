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
  def outdep(n: N, intra: Boolean): Iterable[N]

  /** Incoming dependencies. Used when computing the join from predecessors.
    * @param n
    *   an element from the worklist
    * @return
    *   the elements that the given element depends on
    */
  def indep(n: N, intra: Boolean): Iterable[N]

/** Dependency methods for forward analyses.
  */
trait ForwardDependencies extends Dependencies[CfgNode]:

  /* TODO: add functionality here for distinguishing between Intra / Inter */

  // Also add support for getting edges / conditions here?
  def outdep(n: CfgNode, intra: Boolean = true) =
    if intra then n.succ(intra) else n.succ(intra).union(n.succ(!intra))

  def indep(n: CfgNode, intra: Boolean = true) =
    if intra then n.pred(intra) else n.pred(intra).union(n.pred(!intra))
