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
trait SimpleMonotonicSolver[N] extends MapLatticeSolver[N] with LinkedHashSetWorklist[N] with Dependencies[N]:
  /** The current lattice element.
    */
  var x: lattice.Element = _

  /** The map domain.
    */
  val domain: Set[N]

  private val loopEscape: mutable.Set[N] = mutable.Set.empty

  def process(n: N, intra: Boolean): Unit =
    val xn = x(n)
    val y = funsub(n, x, intra)
    if y != xn || !loopEscape.contains(n) then
      loopEscape.add(n)
      x += n -> y
      add(outdep(n, intra))

  def analyze(intra: Boolean): lattice.Element =
    // TODO this sort of type-dependent code should not be in the generic solver
    // should probably base it upon WorklistFixpointSolverWithReachability from TIP instead?
    val first: Set[N] = if (intra) {
      domain.collect { case n: CfgFunctionEntryNode if n.pred(intra).isEmpty => n }
    } else {
      // TODO this is not the correct way to do things, we should set Cfg.startNode but don't have visibility here
      domain.collect { case n: CfgFunctionEntryNode if n.data.name == "main" => n }
    }

    x = lattice.bottom
    run(first, intra)
    x
