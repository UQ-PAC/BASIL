package analysis

import ir._
import analysis.BitVectorEval
import math.pow
import util.Logger

/** Basic lattice
 */
trait Lattice[T]:

  type Element = T
  /** The bottom element of this lattice.
   */
  val bottom: T

  /** The top element of this lattice. Default: not implemented.
   */
  def top: T = ???

  /** The least upper bound of `x` and `y`.
   */
  def lub(x: T, y: T): T

  /** Returns true whenever `x` <= `y`.
   */
  def leq(x: T, y: T): Boolean = lub(x, y) == y // rarely used, but easy to implement :-)

/** The powerset lattice of a set of elements of type `A` with subset ordering.
 */
class PowersetLattice[A] extends Lattice[Set[A]] {
  val bottom: Set[A] = Set.empty
  def lub(x: Set[A], y: Set[A]): Set[A] = x.union(y)
}

// Single element lattice (using Option)
class SingleElementLattice[T] extends Lattice[Option[T]] {
  val bottom: Option[T] = None
  def lub(x: Option[T], y: Option[T]): Option[T] = (x, y) match {
    case (None, None) => None
    case _ => Some(x.getOrElse(y.get))
  }
}

trait LiftedElement[+T]
case class Lift[T](el: T) extends LiftedElement[T] {
  override def toString = s"Lift($el)"
}
case object LiftedBottom extends LiftedElement[Nothing] {
  override def toString = "LiftBot"
}
/**
 * The lift lattice for `sublattice`.
 * Supports implicit lifting and unlifting.
 */
class LiftLattice[T, +L <: Lattice[T]](val sublattice: L) extends Lattice[LiftedElement[T]] {

  val bottom: LiftedElement[T] = LiftedBottom

  def lub(x: LiftedElement[T], y: LiftedElement[T]): LiftedElement[T] =
    (x, y) match {
      case (LiftedBottom, t) => t
      case (t, LiftedBottom) => t
      case (Lift(a), Lift(b)) => Lift(sublattice.lub(a, b))
    }

  /**
   * Lift elements of the sublattice to this lattice.
   * Note that this method is declared as implicit, so the conversion can be done automatically.
   */
  def lift(x: T): LiftedElement[T] = Lift(x)

  /**
   * Un-lift elements of this lattice to the sublattice.
   * Throws an IllegalArgumentException if trying to unlift the bottom element
   * Note that this method is declared as implicit, so the conversion can be done automatically.
   */
  def unlift(x: LiftedElement[T]): T = x match {
    case Lift(s) => s
    case LiftedBottom => throw new IllegalArgumentException("Cannot unlift bottom")
  }
}

trait TwoElement

case object TwoElementTop extends TwoElement
case object TwoElementBottom extends TwoElement


/**
 * A lattice with only top and bottom
 */
class TwoElementLattice extends Lattice[TwoElement]:
  override val bottom: TwoElement = TwoElementBottom
  override val top: TwoElement = TwoElementTop

  def lub(x: TwoElement, y: TwoElement): TwoElement = (x, y) match {
    case (TwoElementBottom, TwoElementBottom) => TwoElementBottom
    case _ => TwoElementTop
  }

trait FlatElement[+T]
case class FlatEl[T](el: T) extends FlatElement[T]
case object Top extends FlatElement[Nothing]
case object Bottom extends FlatElement[Nothing]

/** The flat lattice made of element of `X`. Top is greater than every other element, and Bottom is less than every
 * other element. No additional ordering is defined.
 */
class FlatLattice[X] extends Lattice[FlatElement[X]] {

  val bottom: FlatElement[X] = Bottom

  override val top: FlatElement[X] = Top

  def lub(x: FlatElement[X], y: FlatElement[X]): FlatElement[X] = (x, y) match {
    case (a, Bottom) => a
    case (Bottom, b) => b
    case (a, b) if a == b => a
    case (Top, _) => Top
    case (_, Top) => Top
    case _ => Top
  }
}

/** The flat lattice made of element of `X` with a default value generator `f`. Top is greater than every other element,
 */
class FlatLatticeWithDefault[X](val f: () => X) extends Lattice[FlatElement[X]] {

  val bottom: FlatElement[X] = FlatEl(f())

  override val top: FlatElement[X] = Top

  def lub(x: FlatElement[X], y: FlatElement[X]): FlatElement[X] = (x, y) match {
    case (a, Bottom) => a
    case (Bottom, b) => b
    case (a, b) if a == b => a
    case (Top, _) => FlatEl(f())
    case (_, Top) => FlatEl(f())
    case _ => FlatEl(f())
  }
}

class TupleLattice[L1 <: Lattice[T1], L2 <: Lattice[T2], T1, T2](val lattice1: L1, val lattice2: L2) extends Lattice[(T1, T2)] {
  override val bottom: (T1, T2) = (lattice1.bottom, lattice2.bottom)

  override def lub(x: (T1, T2), y: (T1, T2)): (T1, T2) = {
    val (x1, x2) = x
    val (y1, y2) = y
    (lattice1.lub(x1, y1), lattice2.lub(x2, y2))
  }

  override def leq(x: (T1, T2), y: (T1, T2)): Boolean = {
    val (x1, x2) = x
    val (y1, y2) = y
    lattice1.leq(x1, y1) && lattice2.leq(x2, y2)
  }

  override def top: (T1, T2) = (lattice1.top, lattice2.top)
}

/** A lattice of maps from a set of elements of type `A` to a lattice with element `L'. Bottom is the default value.
 */
class MapLattice[A, T, +L <: Lattice[T]](val sublattice: L) extends Lattice[Map[A, T]] {
  val bottom: Map[A, T] = Map().withDefaultValue(sublattice.bottom)
  def lub(x: Map[A, T], y: Map[A, T]): Map[A, T] =
    x.keys.foldLeft(y)((m, a) => m + (a -> sublattice.lub(x(a), y(a)))).withDefaultValue(sublattice.bottom)
}

/** Constant propagation lattice.
 *
 */
class ConstantPropagationLattice extends FlatLattice[BitVecLiteral] {
  private def apply(op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral, a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = try {
    (a, b) match
      case (FlatEl(x), FlatEl(y)) => FlatEl(op(x, y))
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (_, Top) => Top
      case (Top, _) => Top
  } catch {
    case e: Exception =>
      Logger.error(s"Failed on op $op with $a and $b")
      throw e
  }

  private def apply(op: BitVecLiteral => BitVecLiteral, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = a match
    case FlatEl(x) => FlatEl(op(x))
    case Top => Top
    case Bottom => Bottom

  def bv(a: BitVecLiteral): FlatElement[BitVecLiteral] = FlatEl(a)
  def bvadd(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvadd, a, b)
  def bvsub(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvsub, a, b)
  def bvmul(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvmul, a, b)
  def bvudiv(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvudiv, a, b)
  def bvsdiv(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvsdiv, a, b)
  def bvsrem(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvsrem, a, b)
  def bvurem(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvurem, a, b)
  def bvsmod(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvsmod, a, b)
  def bvand(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvand, a, b)
  def bvor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvor, a, b)
  def bvxor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvxor, a, b)
  def bvnand(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvnand, a, b)
  def bvnor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvnor, a, b)
  def bvxnor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvxnor, a, b)
  def bvnot(a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvnot, a)
  def bvneg(a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvneg, a)
  def bvshl(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvshl, a, b)
  def bvlshr(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvlshr, a, b)
  def bvashr(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvashr, a, b)
  def bvcomp(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvcomp, a, b)
  def zero_extend(width: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_zero_extend(width, _: BitVecLiteral), a)
  def sign_extend(width: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_sign_extend(width, _: BitVecLiteral), a)
  def extract(high: Int, low: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.boogie_extract(high, low, _: BitVecLiteral), a)
  def concat(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_concat, a, b)
}

/** Constant propagation lattice.
 *
 */
class ConstantPropagationLatticeWithSSA extends PowersetLattice[BitVecLiteral] {
  private def apply(op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral, a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] =
    val res = for {
      x <- a
      y <- b
    } yield op(x, y)
    res

  private def apply(op: BitVecLiteral => BitVecLiteral, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    val res = for {
      x <- a
    } yield op(x)
    res

  def bv(a: BitVecLiteral): Set[BitVecLiteral] = Set(a)
  def bvadd(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvadd, a, b)
  def bvsub(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvsub, a, b)
  def bvmul(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvmul, a, b)
  def bvudiv(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvudiv, a, b)
  def bvsdiv(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvsdiv, a, b)
  def bvsrem(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvsrem, a, b)
  def bvurem(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvurem, a, b)
  def bvsmod(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvsmod, a, b)
  def bvand(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvand, a, b)
  def bvor(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvor, a, b)
  def bvxor(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvxor, a, b)
  def bvnand(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvnand, a, b)
  def bvnor(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvnor, a, b)
  def bvxnor(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvxnor, a, b)
  def bvnot(a: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvnot, a)
  def bvneg(a: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvneg, a)
  def bvshl(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvshl, a, b)
  def bvlshr(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvlshr, a, b)
  def bvashr(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvashr, a, b)
  def bvcomp(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_bvcomp, a, b)
  def zero_extend(width: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_zero_extend(width, _: BitVecLiteral), a)
  def sign_extend(width: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_sign_extend(width, _: BitVecLiteral), a)

  def extract(high: Int, low: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    apply(BitVectorEval.boogie_extract(high, low, _: BitVecLiteral), a)

  def concat(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_concat, a, b)
}