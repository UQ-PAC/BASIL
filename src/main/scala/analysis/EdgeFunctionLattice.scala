package analysis

/**
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/lattices/EdgeFunctionLattice.scala
 * An "edge" represents a function L -> L where L is the value lattice.
 */
trait EdgeFunction[T] extends (T => T) {

  /**
   * Applies the function to the given lattice element.
   */
  def apply(x: T): T

  /**
   * Composes this function with the given one.
   * The resulting function first applies `e` then this function.
   */
  def composeWith(e: EdgeFunction[T]): EdgeFunction[T]

  /**
   * Finds the least upper bound of this function and the given one.
   */
  def joinWith(e: EdgeFunction[T]): EdgeFunction[T]
}

/**
  * The lattice of edge functions, used by [[tip.solvers.IDEAnalysis]].
* A map lattice, but maps are represent differently than in `MapLattice`.
* Currently only supports the identity function and constant functions.
*/
class EdgeFunctionLattice[T, L <: Lattice[T]](val valuelattice: L) extends Lattice[EdgeFunction[T]] {

  val bottom = ConstEdge(valuelattice.bottom)

  def lub(x: Element, y: Element): Element = x.joinWith(y)
  
  /**
   * Edge labeled with identity function.
   */
  case class IdEdge() extends EdgeFunction[T] {

    def apply(x: T): T = x

    def composeWith(e: Element): Element = e

    def joinWith(e: Element): Element =
      if (e == this) this
      else e.joinWith(this)

    override def toString = "IdEdge()"
  }

  /**
   * Edge labeled with constant function.
   */
  case class ConstEdge(c: T) extends EdgeFunction[T] {

    def apply(x: T): T = c

    def composeWith(e: Element): Element = this

    def joinWith(e: Element): Element =
      if (e == this || c == valuelattice.top) this
      else if (c == valuelattice.bottom) e
      else
        e match {
          case IdEdge() => ??? // never reached with the currently implemented analyses
          case ConstEdge(ec) => ConstEdge(valuelattice.lub(c, ec))
          case _ => e.joinWith(this)
        }

    override def toString = s"ConstEdge($c)"
  }
}
