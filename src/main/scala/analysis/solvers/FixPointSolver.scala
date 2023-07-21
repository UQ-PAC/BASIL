package analysis.solvers

import analysis._
import analysis.lattices._
import scala.collection.immutable.ListSet

/** Base trait for lattice solvers.
  */
trait LatticeSolver:

  /** The lattice used by the solver.
    */
  val lattice: Lattice

  /** The analyze function.
    */
  def analyze(intra: Boolean): lattice.Element

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
    * @param intra
    *   true if the cfg is treated as intraprocedural, else interprocedural
    * @return
    *   the output sublattice element
    */
  def funsub(n: N, x: lattice.Element, intra: Boolean): lattice.sublattice.Element =
    transfer(n, join(n, x, intra))

  /** Computes the least upper bound of the incoming elements.
    */
  def join(n: N, o: lattice.Element, intra: Boolean): lattice.sublattice.Element =
    val states = indep(n, intra).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))

/** An abstract worklist algorithm.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait Worklist[N]:

  /** Called by [[run]] to process an item from the worklist.
    */
  def process(n: N, intra: Boolean): Unit

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
  def run(first: Set[N], intra: Boolean): Unit

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

  def run(first: Set[N], intra: Boolean) =
    worklist = new ListSet[N] ++ first
    while (worklist.nonEmpty) do
      val n = worklist.head;
      worklist = worklist.tail
      process(n, intra)

  def monotonic_run(first: Set[N], intra: Boolean) =
    worklist = new ListSet[N] ++ first.collect{ case n: CfgFunctionEntryNode if n.pred(intra).isEmpty => n } // no inlined functions
    while (worklist.nonEmpty) do
      val n = worklist.head;
      worklist = worklist.tail
      process(n, intra)

/** Base trait for worklist-based fixpoint solvers.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait WorklistFixpointSolver[N] extends MapLatticeSolver[N] with ListSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
    */
  var x: lattice.Element = _

  def process(n: N, intra: Boolean) =
    val xn = x(n)
    val y = funsub(n, x, intra)
    if (y != xn) then
      x += n -> y
      add(outdep(n, intra))

/** Worklist-based fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait SimpleWorklistFixpointSolver[N] extends WorklistFixpointSolver[N]:

  /** The map domain.
    */
  val domain: Set[N]

  def analyze(intra: Boolean): lattice.Element =
    x = lattice.bottom
    run(domain, intra)
    x
