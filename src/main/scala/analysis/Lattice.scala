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
  def num(i: BigInt): Element

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

  /** Abstract not equal.
    */
  def neqq(a: Element, b: Element): Element

  /** Abstract less-than.
    */
  def lt(a: Element, b: Element): Element

  /** Abstract less-than or equal.
    */
  def lte(a: Element, b: Element): Element

  /** Abstract bitwise and.
    */
  def and(a: Element, b: Element): Element

  /** Abstract bitwise or.
    */
  def or(a: Element, b: Element): Element

  /** Abstract bitwise xor.
    */
  def xor(a: Element, b: Element): Element

  /** Abstract bitwise not.
    */
  def not(a: Element): Element

  /** Abstract negation.
    */
  def neg(a: Element): Element

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

/** Constant propagation lattice.
  */
object ConstantPropagationLattice extends FlatLattice[BigInt]() with LatticeWithOps:

  private def apply(op: (BigInt, BigInt) => BigInt, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _)                           => FlatElement.Bot
    case (_, FlatElement.Bot)                           => FlatElement.Bot
    case (_, FlatElement.Top)                           => FlatElement.Top
    case (FlatElement.Top, _)                           => FlatElement.Top

  private def apply(op: (BigInt) => BigInt, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(a))
    case FlatElement.Top       => FlatElement.Top
    case FlatElement.Bot       => FlatElement.Bot

  def num(i: BigInt): Element = FlatElement.FlatEl(i)

  def plus(a: Element, b: Element): Element = apply(_ + _, a, b)

  def minus(a: Element, b: Element): Element = apply(_ - _, a, b)

  def times(a: Element, b: Element): Element = apply(_ * _, a, b)

  def div(a: Element, b: Element): Element = apply((x, y) => if y != 0 then x / y else FlatElement.Bot, a, b)

  def eqq(a: Element, b: Element): Element = apply((x, y) => if x == y then 1 else 0, a, b)

  def neqq(a: Element, b: Element): Element = apply((x, y) => if x == y then 0 else 1, a, b)

  def lt(a: Element, b: Element): Element = apply((x, y) => if x < y then 1 else 0, a, b)

  def lte(a: Element, b: Element): Element = apply((x, y) => if x <= y then 1 else 0, a, b)

  def and(a: Element, b: Element): Element = apply(_ & _, a, b)

  def or(a: Element, b: Element): Element = apply(_ | _, a, b)

  def xor(a: Element, b: Element): Element = apply(_ ^ _, a, b)

  def not(a: Element): Element = apply(~_, a)

  def neg(a: Element): Element = apply(-_, a)
