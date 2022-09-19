package analysis

/** Basic lattice
  */
trait Lattice:

  /** The type of the elements of this lattice.
    */
  type Element

  /** The bottom element of this lattice.
    */
  val bottom: Element

  /** The top element of this lattice. Default: not implemented.
    */
  def top: Element = ???

  /** The least upper bound of `x` and `y`.
    */
  def lub(x: Element, y: Element): Element

  /** Returns true whenever `x` <= `y`.
    */
  def leq(x: Element, y: Element): Boolean = lub(x, y) == y // rarely used, but easy to implement :-)

/** Lattice with abstract operators.
  */
trait LatticeWithOps extends Lattice:

  /** Abstract number.
    */
  def num(i: Int): Element

  /** Abstract plus.
    */
  def plus(a: Element, b: Element): Element

  /** Abstract minus.
    */
  def minus(a: Element, b: Element): Element

  /** Abstract times.
    */
  def times(a: Element, b: Element): Element

  /** Abstract division.
    */
  def div(a: Element, b: Element): Element

  /** Abstract equals.
    */
  def eqq(a: Element, b: Element): Element

  /** Abstract greater-than.
    */
  def gt(a: Element, b: Element): Element

/** The flat lattice made of element of `X`. Top is greater than every other element, and Bottom is less than every
  * other element. No additional ordering is defined.
  */
class FlatLattice[X] extends Lattice:

  enum FlatElement:
    case FlatEl(el: X)
    case Top
    case Bot

  type Element = FlatElement

  /** Wrap an element of `X` into an element of the flat lattice.
    */
  implicit def wrap(a: X): Element = FlatElement.FlatEl(a)

  /** Unwrap an element of the lattice to an element of `X`. If the element is Top or Bot then IllegalArgumentException
    * is thrown. Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  implicit def unwrap(a: Element): X = a match
    case FlatElement.FlatEl(n) => n
    case _                     => throw new IllegalArgumentException(s"Cannot unlift $a")

  val bottom: Element = FlatElement.Bot

  override val top: Element = FlatElement.Top

  def lub(x: Element, y: Element): Element =
    if x == FlatElement.Bot || y == FlatElement.Top || x == y then y
    else if y == FlatElement.Bot || x == FlatElement.Top then x
    else FlatElement.Top

/** A lattice of maps from a set of elements of type `A` to the lattice `sublattice`. Bottom is the default value.
  */
class MapLattice[A, +L <: Lattice](val sublattice: L) extends Lattice:

  type Element = Map[A, sublattice.Element]

  val bottom: Element = Map().withDefaultValue(sublattice.bottom)

  def lub(x: Element, y: Element): Element =
    x.keys.foldLeft(y)((m, a) => m + (a -> sublattice.lub(x(a), y(a)))).withDefaultValue(sublattice.bottom)

/** The lift lattice for `sublattice`. Supports implicit lifting and unlifting.
  */
class LiftLattice[+L <: Lattice](val sublattice: L) extends Lattice:

  type Element = Lifted

  enum Lifted:
    case Bottom
    case Lift(n: sublattice.Element)

  val bottom: Element = Lifted.Bottom

  def lub(x: Element, y: Element): Element =
    (x, y) match {
      case (Lifted.Bottom, t)               => t
      case (t, Lifted.Bottom)               => t
      case (Lifted.Lift(a), Lifted.Lift(b)) => Lifted.Lift(sublattice.lub(a, b))
    }

  /** Lift elements of the sublattice to this lattice. Note that this method is declared as implicit, so the conversion
    * can be done automatically.
    */
  implicit def lift(x: sublattice.Element): Element = Lifted.Lift(x)

  /** Un-lift elements of this lattice to the sublattice. Throws an IllegalArgumentException if trying to unlift the
    * bottom element Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  implicit def unlift(x: Element): sublattice.Element = x match
    case Lifted.Lift(s) => s
    case Lifted.Bottom  => throw new IllegalArgumentException("Cannot unlift bottom")

/** Constant propagation lattice.
  */
object ConstantPropagationLattice extends FlatLattice[Int]() with LatticeWithOps:

  private def apply(op: (Int, Int) => Int, a: Element, b: Element): Element = (a, b) match {
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _)                           => FlatElement.Bot
    case (_, FlatElement.Bot)                           => FlatElement.Bot
    case (_, FlatElement.Top)                           => FlatElement.Top
    case (FlatElement.Top, _)                           => FlatElement.Top
  }

  def num(i: Int): Element = FlatElement.FlatEl(i)

  def plus(a: Element, b: Element): Element = apply(_ + _, a, b)

  def minus(a: Element, b: Element): Element = apply(_ - _, a, b)

  def times(a: Element, b: Element): Element = apply(_ * _, a, b)

  def div(a: Element, b: Element): Element = apply((x, y) => if y != 0 then x / y else FlatElement.Bot, a, b)

  def eqq(a: Element, b: Element): Element = apply((x, y) => if x == y then 1 else 0, a, b)

  def gt(a: Element, b: Element): Element = apply((x, y) => if x > y then 1 else 0, a, b)
