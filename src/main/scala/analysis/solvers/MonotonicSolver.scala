package analysis.solvers

import analysis._
import scala.collection.immutable.ListSet


/** Worklist-based fixpoint solver.
 *
 * @tparam N
 *   type of the elements in the worklist.
 */
trait SimpleMonotonicSolver[N] extends MapLatticeSolver[N] with Dependencies[N]:
  /** The current lattice element.
   */
  var x: lattice.Element = _

  /** The map domain.
   */
  val domain: Set[N]

  def process(n: N): Unit =
    val y = funsub(n, x)
    x += n -> y
    n.asInstanceOf[CfgNode].succ.foreach(s => process(s.asInstanceOf[N]))

  def run(first: Set[N]) =
    first match
      case nodes: Set[CfgNode] =>
        nodes.foreach(n => if n.isInstanceOf[CfgFunctionEntryNode] then process(n))
      case _ => print("\nNot a set of CfgNodes\n")

  def analyze(): lattice.Element =
    x = lattice.bottom
    run(domain)
    x
