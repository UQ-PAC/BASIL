package analysis

import ir._
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

  def literal(l: Literal): Element
  def bvadd(a: Element, b: Element): Element
  def bvsub(a: Element, b: Element): Element
  def bvmul(a: Element, b: Element): Element
  def bvudiv(a: Element, b: Element): Element
  def bvsdiv(a: Element, b: Element): Element
  def bvsrem(a: Element, b: Element): Element
  def bvurem(a: Element, b: Element): Element
  def bvshl(a: Element, b: Element): Element
  def bvlshr(a: Element, b: Element): Element
  def bvashr(a: Element, b: Element): Element
  def bvand(a: Element, b: Element): Element
  def bvor(a: Element, b: Element): Element
  def bvxor(a: Element, b: Element): Element
  def bvult(a: Element, b: Element): Element
  def bvule(a: Element, b: Element): Element
  def bvslt(a: Element, b: Element): Element
  def bvsle(a: Element, b: Element): Element
  def bvcomp(a: Element, b: Element): Element
  def zero_extend(width: Int, a: Element): Element
  def sign_extend(width: Int, a: Element): Element
  def extract(high: Int, low: Int, a: Element): Element
  def bvnot(a: Element): Element
  def bvneg(a: Element): Element
  def bvneq(a: Element, b: Element): Element
  def bveq(a: Element, b: Element): Element
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
object ConstantPropagationLattice extends FlatLattice[Literal]() with LatticeWithOps:

  private def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _) => FlatElement.Bot
    case (_, FlatElement.Bot) => FlatElement.Bot
    case (_, FlatElement.Top) => FlatElement.Top
    case (FlatElement.Top, _) => FlatElement.Top

  private def apply(op: Literal => Literal, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top       => FlatElement.Top
    case FlatElement.Bot       => FlatElement.Bot

  override def literal(l: Literal): Element = FlatElement.FlatEl(l)
  override def bvadd(a: Element, b: Element): Element = apply(smt_bvadd, a, b)
  override def bvsub(a: Element, b: Element): Element = apply(smt_bvsub, a, b)
  override def bvmul(a: Element, b: Element): Element = apply(smt_bvmul, a, b)
  override def bvudiv(a: Element, b: Element): Element = apply(smt_bvudiv, a, b)
  override def bvsdiv(a: Element, b: Element): Element = apply(smt_bvsdiv, a, b)
  override def bvsrem(a: Element, b: Element): Element = apply(smt_bvsrem, a, b)
  override def bvurem(a: Element, b: Element): Element = apply(smt_bvurem, a, b)
  override def bvand(a: Element, b: Element): Element = apply(smt_bvand, a, b)
  override def bvor(a: Element, b: Element): Element = apply(smt_bvor, a, b)
  override def bvxor(a: Element, b: Element): Element = apply(smt_bvxor, a, b)
  override def bvnot(a: Element): Element = apply(smt_bvnot, a)
  override def bvneg(a: Element): Element = apply(smt_bvneg, a)
  override def bvshl(a: Element, b: Element): Element = apply(smt_bvshl, a, b)
  override def bvlshr(a: Element, b: Element): Element = apply(smt_bvlshr, a, b)
  override def bvashr(a: Element, b: Element): Element = apply(smt_bvashr, a, b)
  override def bvcomp(a: Element, b: Element): Element = apply(smt_bvcomp, a, b)
  override def bvult(a: Element, b: Element): Element = apply(smt_bvult, a, b)
  override def bvule(a: Element, b: Element): Element = apply(smt_bvule, a, b)
  override def bvslt(a: Element, b: Element): Element = apply(smt_bvslt, a, b)
  override def bvsle(a: Element, b: Element): Element = apply(smt_bvsle, a, b)
  override def zero_extend(width: Int, a: Element): Element = apply(smt_zero_extend(width, _: Literal), a)
  override def sign_extend(width: Int, a: Element): Element = apply(smt_sign_extend(width, _: Literal), a)
  override def extract(high: Int, low: Int, a: Element): Element = apply(smt_extract(high, low, _: Literal), a)
  override def concat(a: Element, b: Element): Element = apply(smt_concat, a, b)
  override def bvneq(a: Element, b: Element): Element = apply(smt_bvneq, a, b)
  override def bveq(a: Element, b: Element): Element = apply(smt_bveq, a, b)


// value-set lattice
/** Constant propagation lattice.
 */
object ValueSetLattice extends FlatLattice[Literal]() with LatticeWithOps:

  private def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _) => FlatElement.Bot
    case (_, FlatElement.Bot) => FlatElement.Bot
    case (_, FlatElement.Top) => FlatElement.Top
    case (FlatElement.Top, _) => FlatElement.Top

  private def apply(op: Literal => Literal, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top => FlatElement.Top
    case FlatElement.Bot => FlatElement.Bot

  override def literal(l: Literal): Element = FlatElement.FlatEl(l)
  override def bvadd(a: Element, b: Element): Element = apply(smt_bvadd, a, b)
  override def bvsub(a: Element, b: Element): Element = apply(smt_bvsub, a, b)
  override def bvmul(a: Element, b: Element): Element = apply(smt_bvmul, a, b)
  override def bvudiv(a: Element, b: Element): Element = apply(smt_bvudiv, a, b)
  override def bvsdiv(a: Element, b: Element): Element = apply(smt_bvsdiv, a, b)
  override def bvsrem(a: Element, b: Element): Element = apply(smt_bvsrem, a, b)
  override def bvurem(a: Element, b: Element): Element = apply(smt_bvurem, a, b)
  override def bvand(a: Element, b: Element): Element = apply(smt_bvand, a, b)
  override def bvor(a: Element, b: Element): Element = apply(smt_bvor, a, b)
  override def bvxor(a: Element, b: Element): Element = apply(smt_bvxor, a, b)
  override def bvnot(a: Element): Element = apply(smt_bvnot, a)
  override def bvneg(a: Element): Element = apply(smt_bvneg, a)
  override def bvshl(a: Element, b: Element): Element = apply(smt_bvshl, a, b)
  override def bvlshr(a: Element, b: Element): Element = apply(smt_bvlshr, a, b)
  override def bvashr(a: Element, b: Element): Element = apply(smt_bvashr, a, b)
  override def bvcomp(a: Element, b: Element): Element = apply(smt_bvcomp, a, b)
  override def bvult(a: Element, b: Element): Element = apply(smt_bvult, a, b)
  override def bvule(a: Element, b: Element): Element = apply(smt_bvule, a, b)
  override def bvslt(a: Element, b: Element): Element = apply(smt_bvslt, a, b)
  override def bvsle(a: Element, b: Element): Element = apply(smt_bvsle, a, b)
  override def zero_extend(width: Int, a: Element): Element = apply(smt_zero_extend(width, _: Literal), a)
  override def sign_extend(width: Int, a: Element): Element = apply(smt_sign_extend(width, _: Literal), a)
  override def extract(high: Int, low: Int, a: Element): Element = apply(smt_extract(high, low, _: Literal), a)
  override def concat(a: Element, b: Element): Element = apply(smt_concat, a, b)
  override def bvneq(a: Element, b: Element): Element = apply(smt_bvneq, a, b)
  override def bveq(a: Element, b: Element): Element = apply(smt_bveq, a, b)