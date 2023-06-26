package analysis.solvers

import analysis.*

import scala.collection.immutable.ListSet
import scala.collection.mutable


/** Fixpoint solver.
 *
 * @tparam N
 *   type of the elements in the solver.
  *
  *   TODO: investigate how to visit all reachable nodes at least once, then remove loopEscape.
  *   TODO: in longer term, add a worklist to avoid processing nodes twice.
  *
 */
trait SimpleMonotonicSolver[N] extends MapLatticeSolver[N] with ListSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
   */
  var x: lattice.Element = _

  /** The map domain.
   */
  val domain: Set[N]

  private val loopEscape: mutable.Set[N] = mutable.Set.empty

  def process(n: N): Unit =
    val xn = x(n)
    val y = funsub(n, x)
    if y != xn || !loopEscape.contains(n) then
      loopEscape.add(n)
      x += n -> y
      add(outdep(n))

  def analyze(): lattice.Element =
    x = lattice.bottom
    monotonic_run(domain)
    x
