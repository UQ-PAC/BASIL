package analysis.solvers

import analysis.*

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.PriorityQueue

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
  def add(ns: Iterable[N]): Unit

  /** Iterates until there is no more work to do.
    *
    * @param first
    *   the initial contents of the worklist
    */
  def run(first: Iterable[N]): Unit

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

  override def add(n: N): Unit =
    worklist += n

  override def add(ns: Iterable[N]): Unit = worklist ++= ns

  override def run(first: Iterable[N]): Unit =
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

  override def add(n: N): Unit =
    worklist += n

  override def add(ns: Iterable[N]): Unit = worklist ++= ns

  override def run(first: Iterable[N]): Unit =
    worklist.addAll(first)
    while worklist.nonEmpty do
      val n = worklist.head
      worklist.remove(n)
      process(n)


/** Priority Queue Worklist
  *
  * Assumes nodes have unique priorities and
  * will only process a node once, regardless of the
  * number of times it is queued. Will process the
  * node again if it is queued AFTER its been processed.
  *
  * Single processing is generally desirable in a worklist
  * solver, as repeated processing of a node without intermediate
  * modifications to its incoming state will be redundant.
  *
  * @tparam N
  *   type of the elements in the worklist.
  */
trait PriorityQueueWorklist[N] extends Worklist[N]:
  val priorities: Map[N, Int] // for rpo ordering

  private val worklist = new mutable.PriorityQueue[(Int, N)]()(Ordering.by(_._1))
  override def add(n: N) = worklist += ((priorities(n), n))
  override def add(ns: Iterable[N]) = worklist ++= ns.map(n => (priorities(n), n))
  override def run(first: Iterable[N]) = {
    add(first)
    while (worklist.nonEmpty) do {
      val N = worklist.dequeue()
      val n = N._2
      /** Drop all items in the queue with the same priority.
        * n is already the greatest, so head >= n implies n == head.
        */
      while (worklist.nonEmpty && worklist.head._1 >= N._1) do {
        val m = worklist.dequeue()._2
        assert(m == n, s"Different nodes with same priority, violates PriorityQueueWorklist assumption: $n and $m")
      }
      process(n)
    }
  }


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
trait PushDownWorklistFixpointSolver[N, T, L <: Lattice[T]] extends MapLatticeSolver[N, T, L] with PriorityQueueWorklist[N] with Dependencies[N]:

  /** The current lattice element.
    */
  var x: Map[N, T] = _

  /** Propagates lattice element y to node m.
    * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/FixpointSolvers.scala#L286
    */
  def propagate(y: T, m: N): Unit = {
    val xm = x(m)
    /* If we are the successors only predecessor, simply overwrite */
    val t = if indep(m).size > 1 then lattice.sublattice.lub(xm, y) else y
    if (t != xm) {
      add(m)
      x += m -> t
    }
  }
  def process(n: N): Unit =
    val xn = x(n)
    var y = transfer(n, xn)
    var next = outdep(n)

    /** Process all nodes that are trivially linked to the current
      * such that the next node is the current's only successor and
      * the current is the next node's only predecessor.
      * Effectively emulating a basic block.
      */
    while (next.size == 1 && indep(next.head).size <= 1) {
      val succ = next.head
      val xm = x(succ)
      if (y == xm) {
        /* Reached a fixed point, no need to progress */
        return
      } else {
        /* Stash the state and continue */
        x += succ -> y
        y = transfer(succ, y)
        next = outdep(succ)
      }
    }

    /* End of the block, propagate onwards */
    for succ <- next do propagate(y, succ)

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
