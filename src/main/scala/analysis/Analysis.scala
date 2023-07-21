package analysis

import ir._
import analysis.solvers._
import analysis.lattices._

/** Trait for program analyses.
  *
  * @tparam R
  *   the type of the analysis result
  */
trait Analysis[+R]:

  /** Performs the analysis and returns the result.
    */
  def analyze(intra: Boolean): R

/** A flow-sensitive analysis.
  * @param stateAfterNode
  *   true if the abstract state of a CFG node represents the program point <em>after</em> the node, false if represents
  *   the program point <em>before</em> the node (used when outputting analysis results)
  */
abstract class FlowSensitiveAnalysis(val stateAfterNode: Boolean) extends Analysis[Any]