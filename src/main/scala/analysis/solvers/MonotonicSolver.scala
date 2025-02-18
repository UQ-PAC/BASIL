package analysis.solvers

import analysis._

import scala.collection.mutable

/** Fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the solver.
  *
  * TODO: investigate how to visit all reachable nodes at least once, then remove loopEscape. TODO: in longer term, add
  * a worklist to avoid processing nodes twice.
  */
trait SimpleMonotonicSolver[A, T, L <: Lattice[T]]
    extends MapLatticeSolver[A, T, L]
    with LinkedHashSetWorklist[A]
    with Dependencies[A] {

  /** The current lattice element.
    */
  var x: Map[A, T] = _

  /** The map domain.
    */
  val first: Set[A]

  private val loopEscape: mutable.Set[A] = mutable.Set.empty

  def process(n: A): Unit =
    val xn = x(n)
    val y = funsub(n, x)
    if y != xn || !loopEscape.contains(n) then
      loopEscape.add(n)
      x += n -> y
      add(outdep(n))

  def analyze(): Map[A, T] = {
    x = lattice.bottom
    run(first)
    x
  }
}
