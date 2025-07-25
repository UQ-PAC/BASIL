package analysis

/** Adapted from Tip https://github.com/cs-au-dk/TIP/blob/master/src/tip/lattices/EdgeFunctionLattice.scala An "edge"
  * represents a function L -> L where L is the value lattice.
  */
trait EdgeFunction[T] extends (T => T) {

  /** Applies the function to the given lattice element.
    */
  def apply(x: T): T

  /** Composes this function with the given one. The resulting function first applies `e` then this function.
    */
  def composeWith(e: EdgeFunction[T]): EdgeFunction[T]

  /** Finds the least upper bound of this function and the given one.
    */
  def joinWith(e: EdgeFunction[T]): EdgeFunction[T]
}

/** The lattice of edge functions, used by [[IDEAnalysis]]. A map lattice, but maps are represent differently than in
  * `MapLattice`. Currently only supports the identity function and constant functions.
  */
class EdgeFunctionLattice[T, L <: Lattice[T]](val valuelattice: L) extends Lattice[EdgeFunction[T]] {

  val bottom: ConstEdge = ConstEdge(valuelattice.bottom)

  def lub(x: EdgeFunction[T], y: EdgeFunction[T]): EdgeFunction[T] = x.joinWith(y)

  /** Edge labeled with identity function.
    */
  case class IdEdge() extends EdgeFunction[T] {

    def apply(x: T): T = x

    def composeWith(e: EdgeFunction[T]): EdgeFunction[T] = e

    def joinWith(e: EdgeFunction[T]): EdgeFunction[T] =
      if (e == this) this
      else e.joinWith(this)

    override def toString = "IdEdge()"
  }

  /** Edge labeled with constant function.
    */
  case class ConstEdge(c: T) extends EdgeFunction[T] {

    def apply(x: T): T = c

    def composeWith(e: EdgeFunction[T]): EdgeFunction[T] = this

    def joinWith(e: EdgeFunction[T]): EdgeFunction[T] =
      e match {
        case IdEdge() => JoinEdge(c)
        case ConstEdge(ec) => ConstEdge(valuelattice.lub(c, ec))
        case _ => e.joinWith(this)
      }

    override def toString = s"ConstEdge($c)"
  }

  /** Edge that is the join of an IdEdge and ConstEdge. `\l . lub(l, c)` as a lambda expression.
    */
  case class JoinEdge(c: T) extends EdgeFunction[T] {

    def apply(x: T): T = valuelattice.lub(x, c)

    def composeWith(e: EdgeFunction[T]): EdgeFunction[T] =
      e match {
        case IdEdge() => this
        case ConstEdge(c) => ConstEdge(c)
        case JoinEdge(d) => JoinEdge(valuelattice.lub(c, d))
      }

    def joinWith(e: EdgeFunction[T]): EdgeFunction[T] =
      e match {
        case IdEdge() => this
        case ConstEdge(d) => JoinEdge(valuelattice.lub(c, d))
        case JoinEdge(d) => JoinEdge(valuelattice.lub(c, d))
        case _ => e.joinWith(this)
      }

    override def toString = s"JoinEdge($c)"
  }

  /** Edge that has `\l . lub (glb(l, a), b)` as a lambda expression. */
  case class MeetThenJoinEdge(a: T, b: T) extends EdgeFunction[T] {
    def apply(x: T): T = valuelattice.lub(valuelattice.glb(a, x), b)

    def composeWith(e: EdgeFunction[T]): EdgeFunction[T] =
      e match {
        case IdEdge() => this
        case ConstEdge(c) => ConstEdge(valuelattice.lub(b,  valuelattice.glb(a, c)))
        case JoinEdge(_) => ??? // The JoinEdge case for composition is ignored
        case MeetThenJoinEdge(c, d) => MeetThenJoinEdge(valuelattice.lub(b, d),
          valuelattice.lub(b, valuelattice.glb(a, valuelattice.lub(c, d))))
        case _ => ???
      }

    def joinWith(e: EdgeFunction[T]): EdgeFunction[T] =
      e match {
        case IdEdge() => MeetThenJoinEdge(valuelattice.top, b)
        case ConstEdge(c) => MeetThenJoinEdge(a, valuelattice.lub(b, c))
        case JoinEdge(d) => ???
        case MeetThenJoinEdge(c, d) => MeetThenJoinEdge(valuelattice.lub(a, c), valuelattice.lub(b, d))
        case _ => e.joinWith(this)
      }

    override def toString = s"MeetThenJoinEdge($a, $b)"
  }
}
