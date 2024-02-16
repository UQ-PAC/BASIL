package analysis

import ir._
import analysis.BitVectorEval
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

class TwoElementLattice extends FlatLattice[Nothing]

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