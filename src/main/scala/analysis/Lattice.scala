package analysis

import ir._
import analysis.BitVectorEval
import util.Logger
import scala.collection.immutable.HashMap

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
  def bvsmod(a: Element, b: Element): Element
  def bvshl(a: Element, b: Element): Element
  def bvlshr(a: Element, b: Element): Element
  def bvashr(a: Element, b: Element): Element
  def bvand(a: Element, b: Element): Element
  def bvor(a: Element, b: Element): Element
  def bvxor(a: Element, b: Element): Element
  def bvnand(a: Element, b: Element): Element
  def bvnor(a: Element, b: Element): Element
  def bvxnor(a: Element, b: Element): Element
  def bvule(a: Element, b: Element): Element
  def bvuge(a: Element, b: Element): Element
  def bvult(a: Element, b: Element): Element
  def bvugt(a: Element, b: Element): Element
  def bvsle(a: Element, b: Element): Element
  def bvsge(a: Element, b: Element): Element
  def bvslt(a: Element, b: Element): Element
  def bvsgt(a: Element, b: Element): Element
  def bvcomp(a: Element, b: Element): Element
  def zero_extend(width: Int, a: Element): Element
  def sign_extend(width: Int, a: Element): Element
  def extract(high: Int, low: Int, a: Element): Element
  def bvnot(a: Element): Element
  def bvneg(a: Element): Element
  def bvneq(a: Element, b: Element): Element
  def bveq(a: Element, b: Element): Element
  def concat(a: Element, b: Element): Element

/** The powerset lattice of a set of elements of type `A` with subset ordering.
  */
class PowersetLattice[A] extends Lattice {

  type Element = Set[A]

  val bottom: Element = Set.empty

  def lub(x: Element, y: Element): Element = x.union(y)
}

/** The flat lattice made of element of `X`. Top is greater than every other element, and Bottom is less than every
  * other element. No additional ordering is defined.
  */
class FlatLattice[X] extends Lattice:

  sealed trait FlatElement

  object FlatElement:
    case class FlatEl(el: X) extends FlatElement

    case object Top extends FlatElement

    case object Bot extends FlatElement

    // Factory method to create FlatEl
    def apply(x: X): FlatEl = FlatEl(x)

    // Extraction/unapply method for pattern matching
    def unapply(arg: FlatEl): Option[X] = Some(arg.el)

  type Element = FlatElement


  val bottom: Element = FlatElement.Bot

  override val top: Element = FlatElement.Top

  def lub(x: Element, y: Element): Element =
    if x == FlatElement.Bot || y == FlatElement.Top || x == y then y
    else if y == FlatElement.Bot || x == FlatElement.Top then x
    else FlatElement.Top

/** A lattice of maps from a set of elements of type `A` to the lattice `sublattice`. Bottom is the default value.
  */
class MapLattice[A, +L <: Lattice](val sublattice: L) extends Lattice:

  /** Specialised map to optimise for:
    * - All elements map to sublattice.bottom
    * - Efficient merge operations
    */
  sealed trait ML:
    def apply(x: A): sublattice.Element
    def +(kv: (A,sublattice.Element)): ML
    def toMap: Map[A,sublattice.Element]
    def merge(x: ML): ML
    def map[B](f: sublattice.Element => B): Map[A,B]
  case class Elem(m: HashMap[A, sublattice.Element]) extends ML:
    def apply(x: A) = m.getOrElse(x, sublattice.bottom)
    def +(kv: (A,sublattice.Element)) =
      if kv._2 == sublattice.bottom then Elem(m - kv._1) else Elem(m + kv)
    def toMap = m.toMap.withDefaultValue(sublattice.bottom)
    def merge(x: ML) = x match {
      case Elem(n) => Elem(m.merged(n)( (p,q) => (p._1,sublattice.lub(p._2,q._2)) ))
      case _ => Elem(m)
    }
    def map[B](f: sublattice.Element => B) = m.map((k,v) => (k,f(v))).withDefaultValue(f(sublattice.bottom))
  case class Bottom() extends ML:
    def apply(x: A) = sublattice.bottom
    def +(kv: (A,sublattice.Element)) =
      if kv._2 == sublattice.bottom then Bottom() else Elem(HashMap(kv))
    def toMap = Map().withDefaultValue(sublattice.bottom)
    def merge(x: ML) = x
    def map[B](f: sublattice.Element => B) = Map().withDefaultValue(f(sublattice.bottom))

  type Element = ML
  val bottom: Element = Bottom()
  def lub(x: Element, y: Element): Element = x.merge(y)

/** Constant propagation lattice.
  */
object ConstantPropagationLattice extends FlatLattice[Literal]() with LatticeWithOps:

  private def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element = try {
    (a, b) match
      case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) =>
        FlatElement.FlatEl(op(x, y))
      case (FlatElement.Bot, _) => FlatElement.Bot
      case (_, FlatElement.Bot) => FlatElement.Bot
      case (_, FlatElement.Top) => FlatElement.Top
      case (FlatElement.Top, _) => FlatElement.Top
  } catch {
    case e: Exception =>
      Logger.error(s"Failed on op $op with $a and $b")
      throw e
  }

  private def apply(op: Literal => Literal, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top       => FlatElement.Top
    case FlatElement.Bot       => FlatElement.Bot

  override def literal(l: Literal): Element = FlatElement.FlatEl(l)
  override def bvadd(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvadd, a, b)
  override def bvsub(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsub, a, b)
  override def bvmul(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvmul, a, b)
  override def bvudiv(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvudiv, a, b)
  override def bvsdiv(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsdiv, a, b)
  override def bvsrem(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsrem, a, b)
  override def bvurem(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvurem, a, b)
  override def bvsmod(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsmod, a, b)
  override def bvand(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvand, a, b)
  override def bvor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvor, a, b)
  override def bvxor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvxor, a, b)
  override def bvnand(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvnand, a, b)
  override def bvnor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvnor, a, b)
  override def bvxnor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvxnor, a, b)
  override def bvnot(a: Element): Element = apply(BitVectorEval.smt_bvnot, a)
  override def bvneg(a: Element): Element = apply(BitVectorEval.smt_bvneg, a)
  override def bvshl(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvshl, a, b)
  override def bvlshr(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvlshr, a, b)
  override def bvashr(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvashr, a, b)
  override def bvcomp(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvcomp, a, b)
  override def bvule(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvule, a, b)
  override def bvuge(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvuge, a, b)
  override def bvult(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvult, a, b)
  override def bvugt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvugt, a, b)
  override def bvsle(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsle, a, b)
  override def bvsge(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsge, a, b)
  override def bvslt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvslt, a, b)
  override def bvsgt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsgt, a, b)
  override def zero_extend(width: Int, a: Element): Element = apply(BitVectorEval.smt_zero_extend(width, _: Literal), a)
  override def sign_extend(width: Int, a: Element): Element = apply(BitVectorEval.smt_sign_extend(width, _: Literal), a)
  override def extract(high: Int, low: Int, a: Element): Element =
    apply(BitVectorEval.boogie_extract(high, low, _: Literal), a)
  override def concat(a: Element, b: Element): Element = apply(BitVectorEval.smt_concat, a, b)
  override def bvneq(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvneq, a, b)
  override def bveq(a: Element, b: Element): Element = apply(BitVectorEval.smt_bveq, a, b)

// value-set lattice
/** Constant propagation lattice.
  */
object ValueSetLattice extends FlatLattice[Literal]() with LatticeWithOps:

  private def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element = (a, b) match
    case (FlatElement.FlatEl(x), FlatElement.FlatEl(y)) => FlatElement.FlatEl(op(x, y))
    case (FlatElement.Bot, _)                           => FlatElement.Bot
    case (_, FlatElement.Bot)                           => FlatElement.Bot
    case (_, FlatElement.Top)                           => FlatElement.Top
    case (FlatElement.Top, _)                           => FlatElement.Top

  private def apply(op: Literal => Literal, a: Element): Element = a match
    case FlatElement.FlatEl(x) => FlatElement.FlatEl(op(x))
    case FlatElement.Top       => FlatElement.Top
    case FlatElement.Bot       => FlatElement.Bot

  override def literal(l: Literal): Element = FlatElement.FlatEl(l)
  override def bvadd(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvadd, a, b)
  override def bvsub(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsub, a, b)
  override def bvmul(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvmul, a, b)
  override def bvudiv(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvudiv, a, b)
  override def bvsdiv(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsdiv, a, b)
  override def bvsrem(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsrem, a, b)
  override def bvsmod(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsmod, a, b)
  override def bvurem(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvurem, a, b)
  override def bvand(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvand, a, b)
  override def bvor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvor, a, b)
  override def bvnand(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvnand, a, b)
  override def bvnor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvnor, a, b)
  override def bvxnor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvxnor, a, b)
  override def bvxor(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvxor, a, b)
  override def bvnot(a: Element): Element = apply(BitVectorEval.smt_bvnot, a)
  override def bvneg(a: Element): Element = apply(BitVectorEval.smt_bvneg, a)
  override def bvshl(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvshl, a, b)
  override def bvlshr(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvlshr, a, b)
  override def bvashr(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvashr, a, b)
  override def bvcomp(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvcomp, a, b)
  override def bvule(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvule, a, b)
  override def bvuge(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvuge, a, b)
  override def bvult(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvult, a, b)
  override def bvugt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvugt, a, b)
  override def bvsle(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsle, a, b)
  override def bvsge(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsge, a, b)
  override def bvslt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvslt, a, b)
  override def bvsgt(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvsgt, a, b)
  override def zero_extend(width: Int, a: Element): Element = apply(BitVectorEval.smt_zero_extend(width, _: Literal), a)
  override def sign_extend(width: Int, a: Element): Element = apply(BitVectorEval.smt_sign_extend(width, _: Literal), a)
  override def extract(high: Int, low: Int, a: Element): Element =
    apply(BitVectorEval.boogie_extract(high, low, _: Literal), a)
  override def concat(a: Element, b: Element): Element = apply(BitVectorEval.smt_concat, a, b)
  override def bvneq(a: Element, b: Element): Element = apply(BitVectorEval.smt_bvneq, a, b)
  override def bveq(a: Element, b: Element): Element = apply(BitVectorEval.smt_bveq, a, b)
