package analysis.solvers

import analysis._

import scala.collection.immutable.ListSet
import scala.collection.mutable

/** Fixpoint solver.
  *
  * @tparam N
  *   type of the elements in the solver.
  *
  * TODO: investigate how to visit all reachable nodes at least once, then remove loopEscape. TODO: in longer term, add
  * a worklist to avoid processing nodes twice.
  */
trait SimpleMonotonicSolver[T, L <: Lattice[T]] extends MapLatticeSolver[CfgNode, T, L] with ListSetWorklist[CfgNode] with Dependencies[CfgNode] {
  /** The current lattice element.
    */
  var x: Map[CfgNode, T] = _

  /** The map domain.
    */
  val domain: Set[CfgNode]

  private val loopEscape: mutable.Set[CfgNode] = mutable.Set.empty

  def process(n: CfgNode): Unit =
    val xn = x(n)
    val y = funsub(n, x)
    if y != xn || !loopEscape.contains(n) then
      loopEscape.add(n)
      x += n -> y
      add(outdep(n))
}


trait IntraproceduralMonotonicSolver[T, L <: Lattice[T]] extends SimpleMonotonicSolver[T, L] with IntraproceduralForwardDependencies {

  def analyze(): Map[CfgNode, T] = {
    // TODO this sort of type-dependent code should not be in the generic solver
    // should probably base it upon WorklistFixpointSolverWithReachability from TIP instead?
    val first: Set[CfgNode] = domain.collect { case n: CfgFunctionEntryNode if n.predIntra.isEmpty => n }
    x = lattice.bottom
    run(first)
    x
  }
}

trait InterproceduralMonotonicSolver[T, L <: Lattice[T]] extends SimpleMonotonicSolver[T, L] with InterproceduralForwardDependencies {
  def analyze(): Map[CfgNode, T] =
    // TODO this sort of type-dependent code should not be in the generic solver
    // should probably base it upon WorklistFixpointSolverWithReachability from TIP instead?
    val first: Set[CfgNode] = domain.collect { case n: CfgFunctionEntryNode if n.data.name == "main" => n }
    x = lattice.bottom
    run(first)
    x
}