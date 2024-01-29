package analysis

/**
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDEAnalysis.scala
 * The special item representing the empty element in IDE.
 */
//final case class Lambda()

/**
 * Base trait for IDE analyses.
 * @tparam D the type of items
 * @tparam L the type of the value lattice
 */
trait IDEAnalysis[D, T, L <: Lattice[T]] {
  val cfg: ProgramCfg

  type DL = Either[D, Lambda]

  /**
   * The value lattice.
   */
  val valuelattice: L

  /**
   * The edge lattice.
   */
  val edgelattice: EdgeFunctionLattice[T, valuelattice.type]

  /**
   * Edges for call-to-entry.
   */
  def edgesCallToEntry(call: CfgJumpNode, entry: CfgFunctionEntryNode)(d: DL): Map[DL, edgelattice.Element]

  /**
   * Edges for exit-to-aftercall.
   */
  def edgesExitToAfterCall(exit: CfgFunctionExitNode, aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element]

  /**
   * Edges for call-to-aftercall.
   */
  def edgesCallToAfterCall(call: CfgJumpNode, aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element]

  /**
   * Edges for other CFG nodes.
   */
  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.Element]

}
