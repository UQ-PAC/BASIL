package analysis

import bap._
import analysis.util._

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

  def literal(l: BAPLiteral): Element
  def plus(a: Element, b: Element): Element
  def minus(a: Element, b: Element): Element
  def times(a: Element, b: Element): Element
  def divide(a: Element, b: Element): Element
  def sdivide(a: Element, b: Element): Element
  def mod(a: Element, b: Element): Element
  def smod(a: Element, b: Element): Element
  def lshift(a: Element, b: Element): Element
  def rshift(a: Element, b: Element): Element
  def arshift(a: Element, b: Element): Element
  def and(a: Element, b: Element): Element
  def or(a: Element, b: Element): Element
  def xor(a: Element, b: Element): Element
  def lt(a: Element, b: Element): Element
  def le(a: Element, b: Element): Element
  def slt(a: Element, b: Element): Element
  def sle(a: Element, b: Element): Element
  def equ(a: Element, b: Element): Element
  def neq(a: Element, b: Element): Element
  def signed(width: Int, a: Element): Element
  def unsigned(width: Int, a: Element): Element
  def extract(high: Int, low: Int, a: Element): Element
  def not(a: Element): Element
  def neg(a: Element): Element
  def concat(a: Element, b: Element): Element

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
object ConstantPropagationLattice extends FlatLattice[BAPLiteral]() with LatticeWithOps:

  private def apply(op: (BAPLiteral, BAPLiteral) => BAPLiteral, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _)                           => FlatElement.Bot
    case (_, FlatElement.Bot)                           => FlatElement.Bot
    case (_, FlatElement.Top)                           => FlatElement.Top
    case (FlatElement.Top, _)                           => FlatElement.Top

  private def apply(op: (BAPLiteral) => BAPLiteral, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top       => FlatElement.Top
    case FlatElement.Bot       => FlatElement.Bot

  override def literal(l: BAPLiteral): Element = FlatElement.FlatEl(l)
  override def plus(a: Element, b: Element): Element = apply(bvadd, a, b)
  override def minus(a: Element, b: Element): Element = apply(bvsub, a, b)
  override def times(a: Element, b: Element): Element = apply(bvmul, a, b)
  override def divide(a: Element, b: Element): Element = apply(bvudiv, a, b)
  override def sdivide(a: Element, b: Element): Element = apply(bvsdiv, a, b)
  override def mod(a: Element, b: Element): Element = apply(bvsrem, a, b)
  override def smod(a: Element, b: Element): Element = apply(bvurem, a, b)
  override def and(a: Element, b: Element): Element = apply(bvand, a, b)
  override def or(a: Element, b: Element): Element = apply(bvor, a, b)
  override def xor(a: Element, b: Element): Element = apply(bvxor, a, b)
  override def not(a: Element): Element = apply(bvnot, a)
  override def neg(a: Element): Element = apply(bvneg, a)
  override def lshift(a: Element, b: Element): Element = apply(bvshl, a, b)
  override def rshift(a: Element, b: Element): Element = apply(bvlshr, a, b)
  override def arshift(a: Element, b: Element): Element = apply(bvashr, a, b)
  override def equ(a: Element, b: Element): Element = apply(bvcomp, a, b)
  override def neq(a: Element, b: Element): Element = apply(bvneq, a, b)
  override def lt(a: Element, b: Element): Element = apply(bvult, a, b)
  override def le(a: Element, b: Element): Element = apply(bvule, a, b)
  override def slt(a: Element, b: Element): Element = apply(bvslt, a, b)
  override def sle(a: Element, b: Element): Element = apply(bvsle, a, b)
  override def signed(width: Int, a: Element): Element = apply(zero_extend(width, _: BAPLiteral), a)
  override def unsigned(width: Int, a: Element): Element = apply(sign_extend(width, _: BAPLiteral), a)
  override def extract(high: Int, low: Int, a: Element): Element = apply(extract(high, low, _: BAPLiteral), a)
  override def concat(a: Element, b: Element): Element = apply(concat, a, b)