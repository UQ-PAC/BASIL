package analysis.solvers

import analysis.*
import ir.*
import scala.collection.immutable.ListSet
import scala.collection.mutable.LinkedHashSet

/** Base trait for lattice solvers.
  */
trait LatticeSolver[T]:

  /** The lattice used by the solver.
    */
  val lattice: Lattice[T]

  /** The analyze function.
    */
  def analyze(): T

/** Base trait for map lattice solvers.
  * @tparam N
  *   type of the elements in the map domain.
  */
trait MapLatticeSolver[N, T, L <: Lattice[T]] extends LatticeSolver[Map[N, T]] with Dependencies[N]:

  /** Must be a map lattice.
    */
  val lattice: MapLattice[N, T, L]

  /** The transfer function.
    */
  def transfer(n: N, s: T): T

  /** The constraint function for individual elements in the map domain. First computes the join of the incoming
    * elements and then applies the transfer function.
    * @param n
    *   the current location in the map domain
    * @param x
    *   the current lattice element for all locations
    * @return
    *   the output sublattice element
    */
  def funsub(n: N, x: Map[N, T]): T =
    transfer(n, join(n, x))

  /** Computes the least upper bound of the incoming elements.
    */
  def join(n: N, o: Map[N, T]): T =
    val states = indep(n).map(o(_))
    states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))

/**
 * Base trait for solvers for map lattices with lifted co-domains.
 * @tparam N type of the elements in the map domain.
 */
trait MapLiftLatticeSolver[N, T, L <: Lattice[T]] extends MapLatticeSolver[N, LiftedElement[T], LiftLattice[T, L]] with Dependencies[N] {

  val lattice: MapLattice[N, LiftedElement[T], LiftLattice[T, L]]

  /**
   * The transfer function for the sub-sub-lattice.
   */
  def transferUnlifted(n: N, s: T): T

  override def transfer(n: N, s: LiftedElement[T]): LiftedElement[T] = {
    s match {
      case LiftedBottom => LiftedBottom // unreachable as input implied unreachable at output
      case Lift(a) => lattice.sublattice.lift(transferUnlifted(n, a))
    }
  }
}

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

/** A simple n^2 worklist algorithm based on `scala.collection.immutable.ListSet`.
  *
  * @tparam N
  *   type of the elements in the worklist.
  *
  * Note:
  *  add(m) is O(n * m)
  *  worklist.run() is O(|first|^2)
  *    - ListSet.tail() and ListSet.head() are both O(n)
  */
trait ListSetWorklist[N] extends Worklist[N]:

  private var worklist = ListSet[N]()

  def add(n: N): Unit =
    worklist += n

  def add(ns: Iterable[N]): Unit = worklist ++= ns

  def run(first: Set[N]): Unit =
    worklist = new ListSet[N] ++ first
    while worklist.nonEmpty do
      val n = worklist.head
      worklist = worklist.tail
      process(n)


/** A more performant worklist algorithm.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait LinkedHashSetWorklist[N] extends Worklist[N]:
  private val worklist = LinkedHashSet[N]()

  def add(n: N): Unit =
    worklist += n

  def add(ns: Set[N]): Unit = worklist ++= ns

  def run(first: Set[N]): Unit =
    worklist.addAll(first)
    while worklist.nonEmpty do
      val n = worklist.head
      worklist.remove(n)
      process(n)


/** Base trait for worklist-based fixpoint solvers.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait WorklistFixpointSolver[N, T, L <: Lattice[T]] extends MapLatticeSolver[N, T, L] with LinkedHashSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
    */
  var x: Map[N, T] = _

  def process(n: N): Unit =
    val xn = x(n)
    val y = funsub(n, x)
    if y != xn then
      x = x + (n -> y)
      add(outdep(n))

/** Worklist-based fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait SimpleWorklistFixpointSolver[N, T, L <: Lattice[T]] extends WorklistFixpointSolver[N, T, L]:

  /** The map domain.
    */
  val domain: Set[N]

  /** Push the results of the analysis one node down. This is used to have the results of the pre node in the current
    * node.
    * @param the
    *   current lattice
    * @return
    *   the new lattice element
    */

  def analyze(): Map[N, T] =
    x = lattice.bottom
    run(domain)
    x

/**
 * The worklist-based fixpoint solver with reachability.
 *
 * This solver works for map lattices with lifted co-domains, where the extra bottom element typically represents "unreachable".
 */
trait WorklistFixpointSolverWithReachability[N, T, L <: Lattice[T]] extends WorklistFixpointSolver[N, LiftedElement[T], LiftLattice[T, L]] with MapLiftLatticeSolver[N, T, L] {

  /**
   * The start locations, used as the initial contents of the worklist.
   */
  val first: Set[N]

  def analyze(): Map[N, LiftedElement[T]] = {
    x = lattice.bottom
    run(first)
    x
  }
}

/** A pushDown worklist-based fixpoint solvers. Pushes the results of the analysis one node down. This is used to have
  * the results of the pred node in the current node. ie. NODE 1: R0 = 69551bv64 RESULT LATTICE = {} NODE 2: R0 =
  * MemLoad[R0 + 54bv64] RESULT LATTICE = {R0 = 69551bv64} NODE 3: R1 = 0bv64 RESULT LATTICE = {R0 = TOP} ...
  *
  * @tparam N
  *   type of the elements in the worklist.
  *
  * Better implementation of the same thing
  * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/FixpointSolvers.scala#L311
  */
trait PushDownWorklistFixpointSolver[N, T, L <: Lattice[T]] extends MapLatticeSolver[N, T, L] with LinkedHashSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
    */
  var x: Map[N, T] = _

  /** Propagates lattice element y to node m.
    * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/FixpointSolvers.scala#L286
    */
  def propagate(y: T, m: N): Unit = {
    val xm = x(m)
    val t = lattice.sublattice.lub(xm, y)
    if (t != xm) {
      add(m)
      x += m -> t
    }
  }

  def process(n: N): Unit =
    val xn = x(n)
    val y = transfer(n, xn)

    for succ <- outdep(n) do propagate(y, succ)

/** Worklist-based fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait SimplePushDownWorklistFixpointSolver[N, T, L <: Lattice[T]] extends PushDownWorklistFixpointSolver[N, T, L]:

  /** The map domain.
    */
  val domain: Set[N]

  /** Push the results of the analysis one node down. This is used to have the results of the pre node in the current
    * node.
    * @param the
    *   current lattice
    * @return
    *   the new lattice element
    */

  def analyze(): Map[N, T] =
    x = lattice.bottom
    run(domain)
    x

trait WorklistFixPointFunctions[N, T, L <: Lattice[T]]  extends  LinkedHashSetWorklist[N]:

  val lattice: MapLattice[N, T, L]

  var x: Map[N, T]

  val first: Set[N]

  def init: T

  def propagate(y: T, m: N) = {
    val xm = x(m)
    val t = lattice.sublattice.lub(xm, y)
    if (t != xm) {
      add(m)
      x += m -> t
    }
  }

  def analyze(): Map[N, T] = {
    x = first.foldLeft(lattice.bottom) { (l, cur) =>
      l + (cur -> init)
    }
    run(first)
    x
  }
