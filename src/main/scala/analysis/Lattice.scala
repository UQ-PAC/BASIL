package analysis

/** Lattice data structure used for
  */
trait Lattice[Element]:

  /** A union or join of two lattice elements. Should contain all the information from the first state as well as all
    * the information from the second state - even if this introduces uncertainty.
    */
  def join(x: Element, y: Element): Element

  /** An intersection or meet of two lattice elements. Should contain all the information that appears in both states.
    */
  def meet(x: Element, y: Element): Element

  /** An ordering relation on the lattice elements so we check any given transfer for loss of precision. A return value
    * of true indicates that x is at least as precise as y
    */
  def leq(x: Element, y: Element): Boolean = join(x, y) == y

  /** Bottom element.
    */
  def bottom: Element

  /** Top element.
    */
  def top: Element

trait LatticeWithOps[T] extends Lattice[T]:
  def num(i: Int): T
  def plus(a: T, b: T): T
  def times(a: T, b: T): T
  def eqq(a: T, b: T): T
  def gt(a: T, b: T): T

enum FlatElement[+T]:
  case Top
  case Bottom
  case Elem(value: T)

class FlatLattice[T] extends Lattice[FlatElement[T]]:

  override def join(x: FlatElement[T], y: FlatElement[T]): FlatElement[T] = x match
    case FlatElement.Top         => x
    case FlatElement.Bottom      => y
    case FlatElement.Elem(value) => if y == FlatElement.Bottom then x else FlatElement.Top

  override def meet(x: FlatElement[T], y: FlatElement[T]): FlatElement[T] = x match
    case FlatElement.Top         => y
    case FlatElement.Bottom      => x
    case FlatElement.Elem(value) => if y == FlatElement.Top then x else FlatElement.Bottom

  override def bottom = FlatElement.Bottom

  override def top = FlatElement.Top

class MapLattice[A, B, L <: Lattice[B]](val sublattice: L) extends Lattice[Map[A, B]]:

  override def join(x: Map[A, B], y: Map[A, B]): Map[A, B] =
    x.keys.foldLeft(y)((m, a) => m + (a -> sublattice.join(x(a), y(a)))).withDefaultValue(sublattice.bottom)

  override def meet(x: Map[A, B], y: Map[A, B]): Map[A, B] = ???

  override def bottom = Map().withDefaultValue(sublattice.bottom)

  override def top = Map().withDefaultValue(sublattice.top)

object ConstantPropagationLattice extends FlatLattice[Int] with LatticeWithOps[FlatElement[Int]]:

  def apply(op: (Int, Int) => Int, a: FlatElement[Int], b: FlatElement[Int]): FlatElement[Int] = (a, b) match
    case (FlatElement.Elem(x), FlatElement.Elem(y)) => FlatElement.Elem(op(x, y))
    case (FlatElement.Bottom, _)                    => FlatElement.Bottom
    case (_, FlatElement.Bottom)                    => FlatElement.Bottom
    case (FlatElement.Top, _)                       => FlatElement.Top
    case (_, FlatElement.Top)                       => FlatElement.Top

  override def plus(a: FlatElement[Int], b: FlatElement[Int]): FlatElement[Int] = apply(_ + _, a, b)

  override def times(a: FlatElement[Int], b: FlatElement[Int]): FlatElement[Int] = apply(_ * _, a, b)

  override def eqq(a: FlatElement[Int], b: FlatElement[Int]): FlatElement[Int] =
    apply((x, y) => if (x == y) 1 else 0, a, b)

  override def gt(a: FlatElement[Int], b: FlatElement[Int]): FlatElement[Int] =
    apply((x, y) => if (x > y) 1 else 0, a, b)

  override def num(i: Int): FlatElement[Int] = FlatElement.Elem(i)
