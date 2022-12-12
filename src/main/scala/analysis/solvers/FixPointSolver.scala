package analysis.solvers

import analysis._
import scala.collection.immutable.ListSet

/** Base trait for lattice solvers.
  */
trait LatticeSolver:

  /** The lattice used by the solver.
    */
  val lattice: Lattice

  /** The analyze function.
    */
  def analyze(): lattice.Element

/** Base trait for map lattice solvers.
  * @tparam N
  *   type of the elements in the map domain.
  */
trait MapLatticeSolver[N] extends LatticeSolver with Dependencies[N]:

  /** Must be a map lattice.
    */
  val lattice: MapLattice[N, Lattice]

  /** The transfer function.
    */
  def transfer(n: N, s: lattice.sublattice.Element): lattice.sublattice.Element

  /** The constraint function for individual elements in the map domain. First computes the join of the incoming
    * elements and then applies the transfer function.
    * @param n
    *   the current location in the map domain
    * @param x
    *   the current lattice element for all locations
    * @return
    *   the output sublattice element
    */
  def funsub(n: N, x: lattice.Element): lattice.sublattice.Element =
    transfer(n, join(n, x))

  /** Computes the least upper bound of the incoming elements.
    */
  def join(n: N, o: lattice.Element): lattice.sublattice.Element =
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))

/** An abstract worklist algorithm.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait Worklist[N]:

  /** Called by [[run]] to process an item from the worklist.
    */
  def process(n: N): Unit

  /** Adds an item to the worklist.
    */
  def add(n: N): Unit

  /** Adds a set of items to the worklist.
    */
  def add(ns: Set[N]): Unit

  /** Iterates until there is no more work to do.
    *
    * @param first
    *   the initial contents of the worklist
    */
  def run(first: Set[N]): Unit

/** A simple worklist algorithm based on `scala.collection.immutable.ListSet`.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait ListSetWorklist[N] extends Worklist[N]:

  private var worklist = new ListSet[N]

  def add(n: N) =
    worklist += n

  def add(ns: Set[N]) = worklist ++= ns

  def run(first: Set[N]) =
    worklist = new ListSet[N] ++ first
    while (worklist.nonEmpty) do
      val n = worklist.head;
      worklist = worklist.tail
      process(n)

/** Base trait for worklist-based fixpoint solvers.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait WorklistFixpointSolver[N] extends MapLatticeSolver[N] with ListSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
    */
  var x: lattice.Element = _

  def process(n: N) =
    val xn = x(n)
    val y = funsub(n, x)
    if (y != xn) then
      x += n -> y
      add(outdep(n))

/** Worklist-based fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait SimpleWorklistFixpointSolver[N] extends WorklistFixpointSolver[N]:

  /** The map domain.
    */
  val domain: Set[N]

  def analyze(): lattice.Element =
    x = lattice.bottom
    run(domain)
    x
