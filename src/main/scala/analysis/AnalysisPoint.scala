/* package analysis

import astnodes._
import util.{AnalysisTypeException, LatticeViolationException}
//import vcgen.State

trait AnalysisPoint[T <: AnalysisPoint[T]] {

  /** Whether the analysis operates forwards or backwards over the code.
 */
  val isForwards: Boolean = true

  /** Library functions that get skipped instead of inlining
 */
  val libraryFunctions: Set[String] = Set("malloc")

  /** A general transfer function on the lattice. Gives us a new AnalysisPoint, which is the result of evaluating our
 * analysis transfer functions on the given stmt from the current point.
 *
 * Note that this function should be able to handle all the different transfer functions by if/else'ing every type of
 * statement the analysis needs to handle.
 */
  def transfer(stmt: Statement): T

  /** Creates an AnalysisPoint in the same type of analysis as this one, but with currentState as whatever we're using
 * for the starting state.
 *
 * For most analyses, this will be low/false/no information, but for top-down analyses
 */
  def createLowest: T

  /** Basic placeholder that gives the simple name of the class, which useful for exception handling. Feel free to
 * override this with more specific state information on a per-analysis basis.
 */
  override def toString: String = {
    this.getClass.getSimpleName
  }

  /** Fancy method that uses the transfer and compare methods to guarantee that we maintain monotonicity. This is the
 * method that the worklist actually uses to operate on statements.
 *
 * The only case for overriding this function should be if the analysis is top-down rather than bottom-up In that
 * scenario, changing the comparison to < 0 should make it work.
 */
  def transferAndCheck(stmt: Statement): T = {
    val newState: T = transfer(stmt)

    if (compare(newState) > 0) {
      throw new LatticeViolationException(toString)
    }
    newState
  }

  /** A generically-named "combine" function. For must-analyses, this should be meet(other), but for may- analyses,
 * join(other) is fine.
 *
 * This function gets used by the worklist to combine parents' states as well as overlapping functions' states.
 */
  def combine(other: T): T = {
    join(other)
  }

  def applyChange(stmt: Statement): Statement

}
 */
