package analysis

import ir.*
import ir.eval.BitVectorEval
import util.StaticAnalysisLogger
import util.assertion.*

/**
 * Lattice operations on the given type `T`. This is intended to be used
 * as a [type-class]. Notably, this means that the T class should *not*
 * directly extend [[Lattice]][T]. Rather, a separate class should be created
 * to extend `Lattice[T]` (this is automated if you use the [given syntax]).
 * Placing the lattice methods outside of the class itself gives us a lot
 * more flexibility.
 *
 * To access the methods within this trait, you should add a
 * "using" clause like `(using l: Lattice[DesiredType])` to the end of the
 * parameter list of a method or class. Then, you will have access to an `l`
 * variable containing the [[Lattice]] methods. See [given syntax] docs for
 * more details, including how to define given instances.
 *
 * [type-class]: https://docs.scala-lang.org/scala3/book/ca-type-classes.html
 * [given syntax]: https://docs.scala-lang.org/scala3/reference/contextual/previous-givens.html
 */
trait Lattice[T]:

  type Element = T

  /** The bottom element of this lattice.
    */
  def bottom: T

  /** The top element of this lattice.
    */
  def top: T

  /** The least upper bound of `x` and `y`.
    */
  def lub(x: T, y: T): T

  /** The greatest lower bound of `x` and `y`.
    */
  def glb(x: T, y: T): T

  /** Returns true whenever `x` <= `y`.
    */
  def leq(x: T, y: T): Boolean = lub(x, y) == y // rarely used, but easy to implement :-)

  /**
   * These convenience methods give easy access to the `join` and `meet` functions through
   * the `.meet` and `.join` syntax.
   *
   * These methods are provided by the [[Lattice]] trait and can be used whenever a
   * [[Lattice]] (with the correct type) is in scope.
   */
  extension (x: T)
    def join(y: T) = lub(x, y)
    def meet(y: T) = glb(x, y)

/**
 * A terrible hack to translate a [[Lattice]] value into a trait
 * superclass by forwarding its methods to the [[Lattice]] value.
 *
 * Think twice before using.
 */
trait LatticeLattice[L](l: Lattice[L]) extends Lattice[L] {
  def lub(a: L, b: L): L = l.lub(a, b)

  def glb(a: L, b: L): L = l.glb(a, b)

  val top: L = l.top
  val bottom: L = l.bottom
}

trait StridedWrappedInterval

case class SI(s: BigInt, l: BigInt, u: BigInt, w: BigInt) extends StridedWrappedInterval {
  if (l == u) {
    require(s == 0)
  }

  override def toString = s"SASI $s [$l, $u] $w"
}

case object SIBottom extends StridedWrappedInterval {
  override def toString = "SASIBot"
}

// TOP is 1[0^w, 1^w]w
case object SITop extends StridedWrappedInterval {
  override def toString = "SASITop"
}

class SASILattice extends Lattice[StridedWrappedInterval] {
  val lowestPossibleValue: BigInt = 0
  val highestPossibleValue: BigInt = Long.MaxValue - 1

  val bottom: StridedWrappedInterval = SIBottom

  val top: StridedWrappedInterval = SITop

  //  def gamma(x: StridedWrappedInterval): Set[BitVecLiteral] = x match {
  //    case SIBottom => Set.empty
  //    case SI(s, l, u, w) =>
  //      if (s == BitVecLiteral(0, 64)) { // singleton set
  //        Set(l)
  //      } else {
  //        bitVec_interval(l, u, s)
  //      }
  //  }

  def isSingleValue(x: StridedWrappedInterval): Boolean = x match {
    case SI(s, l, u, w) => s == 0 && l == u
    case _ => false
  }

  def modularPlus(a: BigInt, b: BigInt, w: BigInt): BigInt = {
    (a + b) mod BigInt(2).pow(w.toInt)
  }

  def modularMinus(a: BigInt, b: BigInt, w: BigInt): BigInt = {
    (a - b) mod BigInt(2).pow(w.toInt)
  }

  def modularLEQ(a: BigInt, b: BigInt, x: BigInt, w: BigInt): Boolean = {
    modularMinus(a, x, w) <= modularMinus(b, x, w)
  }

  def membershipFunction(v: BigInt, r: StridedWrappedInterval): Boolean = {
    r match {
      case SIBottom => false
      case SITop => true
      case SI(sr, lb, ub, w) =>
        modularLEQ(v, ub, lb, w) && (modularMinus(v, lb, w) mod sr) == 0
    }
  }

  def cardinalityFunction(r: StridedWrappedInterval, w: BigInt): BigInt = {
    r match {
      case SIBottom => 0
      case SITop => BigInt(2).pow(w.toInt)
      case SI(sr, lb, ub, w) => ((ub - lb + 1) / sr) // TODO: this may need to be a math.floor operation
    }
  }

  def orderingOperator(r: StridedWrappedInterval, t: StridedWrappedInterval): Boolean = {
    if (r == SITop && t != SITop) {
      false
    } else if (r == SIBottom || t == SITop) {
      true
    } else {
      (r, t) match {
        case (SI(sr, a, b, w1), SI(st, c, d, w2)) =>

          if ((a == c) && (b == d) && ((st == 0 && sr == 0) || (st != 0 && (sr mod st) == 0))) { // added check for zero division that is not in paper
            return true
          }
          membershipFunction(a, t) && membershipFunction(b, t) && (!membershipFunction(c, r) || !membershipFunction(
            d,
            r
          )) && ((a - c) mod st) == 0 && (sr mod st) == 0
        case _ => false
      }
    }
  }

  def glb(r: StridedWrappedInterval, t: StridedWrappedInterval): Nothing = ???

  /** S1[L1, U1] join S2[L2, U2] -> gcd(S1, S2)[min(L1, L2), max(U1, U2)] */
  def lub(r: StridedWrappedInterval, t: StridedWrappedInterval): StridedWrappedInterval = {
    (r, t) match {
      case (SIBottom, t) => t
      case (t, SIBottom) => t
      case (SITop, _) => SITop
      case (_, SITop) => SITop
      case (SI(sr, a, b, w1), SI(st, c, d, w2)) =>
        debugAssert(w1 == w2)
        val w = w1 // TODO: should this be the largest?
        if (orderingOperator(r, t)) {
          return t
        }
        if (orderingOperator(t, r)) {
          return r
        }
        if (
          membershipFunction(a, t) && membershipFunction(b, t) && membershipFunction(c, r) && membershipFunction(d, r)
        ) {
          return SITop
        }
        if (
          membershipFunction(c, r) && membershipFunction(b, t) && !membershipFunction(a, t) && !membershipFunction(d, r)
        ) {
          return SI(sr.gcd(st).gcd(modularMinus(d, a, w)), a, d, w)
        }
        if (
          membershipFunction(a, t) && membershipFunction(d, r) && !membershipFunction(c, r) && !membershipFunction(b, t)
        ) {
          return SI(sr.gcd(st).gcd(modularMinus(b, c, w)), c, b, w)
        }
        val sad = SI(sr.gcd(st).gcd(modularMinus(d, a, w)), a, d, w)
        val scb = SI(sr.gcd(st).gcd(modularMinus(b, c, w)), c, b, w)
        if (
          !membershipFunction(a, t) && !membershipFunction(d, r) && !membershipFunction(c, r) && !membershipFunction(
            b,
            t
          ) && cardinalityFunction(sad, w) <= cardinalityFunction(scb, w)
        ) {
          return sad
        }
        return scb
      case _ => ???
    }
  }

  def singletonSI(v: BigInt, w: BigInt): StridedWrappedInterval = {
    SI(0, v, v, w)
  }

  //    def valuesToSI(x: List[BigInt], w: BigInt): StridedWrappedInterval = {
  //      if (x.isEmpty) {
  //        SIBottom
  //      } else {
  //        val l = x.min
  //        val u = x.max
  //        val initialStride = u - l
  //        val stride = x.foldLeft(initialStride) {
  //          case (acc, v) => acc.gcd(v - l)
  //        }
  //        SI(stride, l, u, w)
  //      }
  //    }

  /** Convert a set of values to a strided interval. Assumes the widths are the same.
    * @param x
    *   the set of values
    * @param w
    *   the width of each value
    * @return
    *   the strided interval representing the values in the set
    */
  def valuesToSI(x: Set[BigInt], w: BigInt): StridedWrappedInterval = {
    if (x.isEmpty) {
      SIBottom
    } else {
      // create singleton intervals for each value and then join them
      x.foldLeft(bottom) { case (acc, v) =>
        lub(acc, singletonSI(v, w))
      }
    }
  }

  /** s + t = BOT if s = BOT or t = BOT gcd(s, t)(|a +w c, b +w d|) if s = (|a, b|), t = (|c, d|) and #s + #t <= 2^w
    * @param s
    * @param t
    * @return
    */
  def add(s: StridedWrappedInterval, t: StridedWrappedInterval): StridedWrappedInterval = {
    (s, t) match {
      case (SIBottom, _) => SIBottom // TODO: is this correct?
      case (_, SIBottom) => SIBottom // TODO: is this correct?
      case (SI(ss, a, b, w1), SI(st, c, d, w2))
          if (cardinalityFunction(s, w1) + cardinalityFunction(t, w2)) <= BigInt(2).pow(w1.toInt) =>
        debugAssert(w1 == w2)
        return SI(ss.gcd(st), modularPlus(a, c, w1), modularPlus(b, d, w1), w1)
      case _ => SITop
    }
  }

  def add(s: StridedWrappedInterval, t: BigInt, w: BigInt): StridedWrappedInterval = {
    (s, t) match {
      case (SIBottom, _) => SIBottom // TODO: is this correct?
      case (SI(ss, a, b, w1), t) =>
        return add(s, singletonSI(t, w))
      case _ => SITop
    }
  }

  def sub(s: StridedWrappedInterval, t: StridedWrappedInterval): StridedWrappedInterval = {
    (s, t) match {
      case (SIBottom, _) => SIBottom // TODO: is this correct?
      case (_, SIBottom) => SIBottom // TODO: is this correct?
      case (SI(ss, a, b, w1), SI(st, c, d, w2))
          if (cardinalityFunction(s, w1) + cardinalityFunction(t, w2)) <= BigInt(2).pow(w1.toInt) =>
        debugAssert(w1 == w2)
        return SI(ss.gcd(st), modularMinus(a, d, w1), modularMinus(b, c, w1), w1)
      case _ => SITop
    }
  }

  def sub(s: StridedWrappedInterval, t: BigInt, w: BigInt): StridedWrappedInterval = {
    (s, t) match {
      case (SIBottom, _) => SIBottom // TODO: is this correct?
      case (SI(ss, a, b, w1), t) =>
        return sub(s, singletonSI(t, w))
      case _ => SITop
    }
  }
}

sealed trait ValueSet[T]

case class VS[T](m: Map[T, StridedWrappedInterval])
    extends ValueSet[T] { // TODO: default value in map must be assumed to be SIBottom
  override def toString: String = m.toString
}

/** The lattice of integers with the standard ordering.
  */
class ValueSetLattice[T] extends Lattice[ValueSet[T]] {

  case object VSBottom extends ValueSet[T] {
    override def toString = "VSBot"
  }

  case object VSTop extends ValueSet[T] {
    override def toString = "VSTop"
  }

  val bottom: ValueSet[T] = VSBottom

  val top: ValueSet[T] = VSTop

  val lattice: SASILattice = SASILattice()

  def glb(x: ValueSet[T], y: ValueSet[T]): Nothing = ???

  def lub(x: ValueSet[T], y: ValueSet[T]): ValueSet[T] = {
    (x, y) match {
      case (VSBottom, t) => t
      case (t, VSBottom) => t
      case (VSTop, _) => VSTop
      case (_, VSTop) => VSTop
      case (VS(m1), VS(m2)) =>
        VS(m1.keys.foldLeft(m2) { case (acc, k) =>
          val v1 = m1(k)
          val v2 = m2(k)
          acc + (k -> lattice.lub(v1, v2))
        })
    }
  }

  //  def meet(x: ValueSet[String], y: ValueSet[String]): ValueSet[String] = {
  //    (x, y) match {
  //      case (VSBottom, t) => VSBottom
  //      case (t, VSBottom) => VSBottom
  //      case (VSTop, _) => y
  //      case (_, VSTop) => x
  //      case (VS(m1), VS(m2)) =>
  //        VS(m1.keys.foldLeft(m2) {
  //          case (acc, k) =>
  //            val v1 = m1(k)
  //            val v2 = m2(k)
  //            acc + (k -> lattice.meet(v1, v2))
  //        })
  //    }
  //  }

  def applyOp(op: BinOp, lhs: ValueSet[T], rhs: Either[ValueSet[T], BitVecLiteral]): ValueSet[T] = {
    op match
      case EQ => applyOp(EQ, lhs, rhs)
      case NEQ => applyOp(NEQ, lhs, rhs)
      case bvOp: BVBinOp =>
        bvOp match
          case BVAND => ???
          case BVOR => ???
          case BVADD =>
            rhs match
              case Left(vs) => add(lhs, vs)
              case Right(bitVecLiteral) => add(lhs, bitVecLiteral)
          case BVMUL => ???
          case BVUDIV => ???
          case BVUREM => ???
          case BVSHL => ???
          case BVLSHR => ???
          case BVULT => ???
          case BVNAND => ???
          case BVNOR => ???
          case BVXOR => ???
          case BVXNOR => ???
          case BVCOMP => ???
          case BVSUB =>
            rhs match
              case Left(vs) => sub(lhs, vs)
              case Right(bitVecLiteral) => sub(lhs, bitVecLiteral)
          case BVSDIV => ???
          case BVSREM => ???
          case BVSMOD => ???
          case BVASHR => ???
          case BVULE => ???
          case BVUGT => ???
          case BVUGE => ???
          case BVSLT => ???
          case BVSLE => ???
          case BVSGT => ???
          case BVSGE => ???
          case BVCONCAT => ???
      case boolOp: BoolBinOp =>
        boolOp match
          case BoolAND => applyOp(BVAND, lhs, rhs)
          case BoolOR => applyOp(BVOR, lhs, rhs)
          case BoolIMPLIES => ???
      case intOp: IntBinOp =>
        applyOp(intOp.toBV, lhs, rhs)
  }

  def applyOp(op: UnOp, rhs: ValueSet[T]): ValueSet[T] = {
    op match
      case bvOp: BVUnOp =>
        bvOp match
          case BVNOT => ???
          case BVNEG => ???
      case boolOp: BoolUnOp =>
        boolOp match
          case BoolNOT => ???
          case BoolToBV1 => ???
      case intOp: IntUnOp =>
        applyOp(intOp.toBV, rhs)
  }

  def add(x: ValueSet[T], y: ValueSet[T]): ValueSet[T] = {
    (x, y) match {
      case (VSBottom, t) => t
      case (t, VSBottom) => t
      case (VSTop, _) => VSTop
      case (_, VSTop) => VSTop
      case (VS(m1), VS(m2)) =>
        VS(m1.keys.foldLeft(m2) { case (acc, k) =>
          val v1 = m1(k)
          val v2 = m2(k)
          acc + (k -> lattice.add(v1, v2))
        })
    }
  }

  def add(x: ValueSet[T], y: BitVecLiteral): ValueSet[T] = {
    x match {
      case VSBottom => VSBottom
      case VSTop => VSTop
      case VS(m) =>
        VS(m.map { case (k, s) =>
          k -> lattice.add(s, y.value, y.size) // TODO: is the size correct here?
        })
    }
  }

  def sub(x: ValueSet[T], y: ValueSet[T]): ValueSet[T] = {
    (x, y) match {
      case (VSTop, _) => VSTop // TODO: is this correct?
      case (_, VSTop) => VSTop // TODO: is this correct?
      case (VSBottom, t) => VSBottom
      case (t, VSBottom) => t
      case (VS(m1), VS(m2)) =>
        VS(m1.keys.foldLeft(m2) { case (acc, k) =>
          val v1 = m1(k)
          val v2 = m2(k)
          acc + (k -> lattice.sub(v1, v2))
        })
    }
  }

  def sub(x: ValueSet[T], y: BitVecLiteral): ValueSet[T] = {
    x match {
      case VSTop => VSTop
      case VSBottom => VSBottom
      case VS(m) =>
        VS(m.map { case (k, s) =>
          k -> lattice.sub(s, y.value, y.size) // TODO: is the size correct here?
        })
    }
  }

  //  def widen(vs1: ValueSet[T], vs2: ValueSet[T]): ValueSet[T] = {
  //    (vs1, vs2) match {
  //      case (VSBottom, t) => ???
  //      case (t, VSBottom) => ???
  //      case (VSTop, _) => VSTop
  //      case (_, VSTop) => VSTop
  //      case (VS(m1), VS(m2)) =>
  //        VS(m1.keys.foldLeft(m2) {
  //          case (acc, k) =>
  //            val v1 = m1(k)
  //            val v2 = m2(k)
  //            acc + (k -> lattice.widen(v1, v2))
  //        })
  //    }
  //  }

  def removeLowerBounds(vs: ValueSet[T]): ValueSet[T] = {
    vs match {
      case VSBottom => VSBottom
      case VSTop => VSTop
      case VS(m) =>
        VS(m.map { case (k, SI(s, l, u, w)) =>
          k -> SI(s, lattice.lowestPossibleValue, u, w)
        })
    }
  }

  def removeUpperBound(vs: ValueSet[T]): ValueSet[T] = {
    vs match {
      case VSBottom => VSBottom
      case VSTop => VSTop
      case VS(m) =>
        VS(m.map { case (k, SI(s, l, u, w)) =>
          k -> SI(s, l, lattice.highestPossibleValue, w)
        })
    }
  }
}

trait Bool3

case object BOTTOM_BOOL3 extends Bool3 {
  override def toString = "BOTTOM"
}

case object FALSE_BOOL3 extends Bool3 {
  override def toString = "FALSE"
}

case object TURE_BOOL3 extends Bool3 {
  override def toString = "TRUE"
}

case object MAYBE_BOOL3 extends Bool3 {
  override def toString = "MAYBE"
}

/** The lattice of booleans with the standard ordering.
  */
class Bool3Lattice extends Lattice[Bool3] {

  val bottom: Bool3 = BOTTOM_BOOL3

  val top: Bool3 = MAYBE_BOOL3

  def lub(x: Bool3, y: Bool3): Bool3 = {
    (x, y) match {
      case (BOTTOM_BOOL3, t) => t
      case (t, BOTTOM_BOOL3) => t
      case (TURE_BOOL3, FALSE_BOOL3) => MAYBE_BOOL3
      case (FALSE_BOOL3, TURE_BOOL3) => MAYBE_BOOL3
      case _ => x
    }
  }

  def glb(x: Bool3, y: Bool3): Nothing = ???
}

enum Flags {
  case CF // Carry Flag
  case ZF // Zero Flag
  case SF // Sign Flag
  case PF // Parity Flag
  case AF // Auxiliary Flag
  case OF // Overflow Flag
}

/** case CF // Carry Flag case ZF // Zero Flag case SF // Sign Flag case PF // Parity Flag case AF // Auxiliary Flag
  * case OF // Overflow Flag
  */
trait Flag

case object BOTTOM_Flag extends Flag {
  override def toString = "BOTTOM_FLAG"
}

case class FlagMap(m: Map[Flags, Bool3]) extends Flag {
  override def toString: String = m.toString
}

/** The lattice of booleans with the standard ordering.
  */
class FlagLattice extends Lattice[Flag] {

  val bottom: Flag = BOTTOM_Flag

  val top: Flag = FlagMap(
    Map(
      Flags.CF -> MAYBE_BOOL3,
      Flags.ZF -> MAYBE_BOOL3,
      Flags.SF -> MAYBE_BOOL3,
      Flags.PF -> MAYBE_BOOL3,
      Flags.AF -> MAYBE_BOOL3,
      Flags.OF -> MAYBE_BOOL3
    )
  )

  val lattice: Bool3Lattice = Bool3Lattice()

  def glb(x: Flag, y: Flag): Nothing = ???
  def lub(x: Flag, y: Flag): Flag = {
    (x, y) match {
      case (BOTTOM_Flag, t) => t
      case (t, BOTTOM_Flag) => t
      case (FlagMap(m1), FlagMap(m2)) =>
        FlagMap(m1.keys.foldLeft(m2) { case (acc, k) =>
          val v1 = m1(k)
          val v2 = m2(k)
          acc + (k -> lattice.lub(v1, v2))
        })
    }
  }

  def setFlag(flag: Flags, value: Bool3): Flag = {
    FlagMap(Map(flag -> value))
  }
}

/** The powerset lattice of a set of elements of type `A` with subset ordering.
  */
class PowersetLattice[A] extends Lattice[Set[A]] {
  val bottom: Set[A] = Set.empty
  def top: Nothing = ???
  def lub(x: Set[A], y: Set[A]): Set[A] = x.union(y)
  def glb(x: Set[A], y: Set[A]): Nothing = ???
}

given [A]: Lattice[Set[A]] = PowersetLattice[A]()

// Single element lattice (using Option)
class SingleElementLattice[T] extends Lattice[Option[T]] {
  val bottom: Option[T] = None
  def top: Nothing = ???
  def lub(x: Option[T], y: Option[T]): Option[T] = (x, y) match {
    case (None, None) => None
    case _ => Some(x.getOrElse(y.get))
  }
  def glb(x: Option[T], y: Option[T]): Nothing = ???
}

trait LiftedElement[+T]
case class Lift[T](el: T) extends LiftedElement[T] {
  override def toString = s"Lift($el)"
}
case object LiftedBottom extends LiftedElement[Nothing] {
  override def toString = "LiftBot"
}

/** The lift lattice for `sublattice`. Supports implicit lifting and unlifting.
  */
class LiftLattice[T, +L <: Lattice[T]](val sublattice: L) extends Lattice[LiftedElement[T]] {

  val bottom: LiftedElement[T] = LiftedBottom
  def top: Nothing = ???

  def glb(x: LiftedElement[T], y: LiftedElement[T]): Nothing = ???
  def lub(x: LiftedElement[T], y: LiftedElement[T]): LiftedElement[T] =
    (x, y) match {
      case (LiftedBottom, t) => t
      case (t, LiftedBottom) => t
      case (Lift(a), Lift(b)) => Lift(sublattice.lub(a, b))
    }

  /** Lift elements of the sublattice to this lattice. Note that this method is declared as implicit, so the conversion
    * can be done automatically.
    */
  def lift(x: T): LiftedElement[T] = Lift(x)

  /** Un-lift elements of this lattice to the sublattice. Throws an IllegalArgumentException if trying to unlift the
    * bottom element Note that this method is declared as implicit, so the conversion can be done automatically.
    */
  def unlift(x: LiftedElement[T]): T = x match {
    case Lift(s) => s
    case LiftedBottom => throw new IllegalArgumentException("Cannot unlift bottom")
  }
}

trait TwoElement

case object TwoElementTop extends TwoElement
case object TwoElementBottom extends TwoElement

/** A lattice with only top and bottom
  */
class TwoElementLattice extends Lattice[TwoElement]:
  override val bottom: TwoElement = TwoElementBottom
  override val top: TwoElement = TwoElementTop

  def glb(x: TwoElement, y: TwoElement): Nothing = ???
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

  val top: FlatElement[X] = Top

  def glb(x: FlatElement[X], y: FlatElement[X]): Nothing = ???
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

  val top: FlatElement[X] = Top

  def glb(x: FlatElement[X], y: FlatElement[X]): Nothing = ???
  def lub(x: FlatElement[X], y: FlatElement[X]): FlatElement[X] = (x, y) match {
    case (a, Bottom) => a
    case (Bottom, b) => b
    case (a, b) if a == b => a
    case (Top, _) => FlatEl(f())
    case (_, Top) => FlatEl(f())
    case _ => FlatEl(f())
  }
}

class TupleLattice[+L1 <: Lattice[T1], +L2 <: Lattice[T2], T1, T2](val lattice1: L1, val lattice2: L2)
    extends Lattice[(T1, T2)] {
  val bottom: (T1, T2) = (lattice1.bottom, lattice2.bottom)

  def glb(x: (T1, T2), y: (T1, T2)): Nothing = ???

  def lub(x: (T1, T2), y: (T1, T2)): (T1, T2) = {
    val (x1, x2) = x
    val (y1, y2) = y
    (lattice1.lub(x1, y1), lattice2.lub(x2, y2))
  }

  override def leq(x: (T1, T2), y: (T1, T2)): Boolean = {
    val (x1, x2) = x
    val (y1, y2) = y
    lattice1.leq(x1, y1) && lattice2.leq(x2, y2)
  }

  val top: (T1, T2) = (lattice1.top, lattice2.top)
}

/** A lattice of maps from a set of elements of type `A` to a lattice with element `L'. Bottom is the default value.
  */
class MapLattice[A, T, +L <: Lattice[T]](val sublattice: L) extends Lattice[Map[A, T]] {
  val bottom: Map[A, T] = Map().withDefaultValue(sublattice.bottom)
  def lub(x: Map[A, T], y: Map[A, T]): Map[A, T] =
    x.keys.foldLeft(y)((m, a) => m + (a -> sublattice.lub(x(a), y(a)))).withDefaultValue(sublattice.bottom)

  def glb(x: Map[A, T], y: Map[A, T]): Nothing = ???
  def top: Nothing = ???
}

/** Constant propagation lattice.
  */
class ConstantPropagationLattice extends FlatLattice[BitVecLiteral] {
  private def apply(
    op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral,
    a: FlatElement[BitVecLiteral],
    b: FlatElement[BitVecLiteral]
  ): FlatElement[BitVecLiteral] = try {
    (a, b) match
      case (FlatEl(x), FlatEl(y)) => FlatEl(op(x, y))
      case (Bottom, _) => Bottom
      case (_, Bottom) => Bottom
      case (_, Top) => Top
      case (Top, _) => Top
  } catch {
    case e: Exception =>
      StaticAnalysisLogger.error(s"Failed on op $op with $a and $b")
      throw e
  }

  private def apply(op: BitVecLiteral => BitVecLiteral, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    a match
      case FlatEl(x) => FlatEl(op(x))
      case Top => Top
      case Bottom => Bottom

  def bv(a: BitVecLiteral): FlatElement[BitVecLiteral] = FlatEl(a)
  def bvadd(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvadd, a, b)
  def bvsub(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvsub, a, b)
  def bvmul(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvmul, a, b)
  def bvudiv(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvudiv, a, b)
  def bvsdiv(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvsdiv, a, b)
  def bvsrem(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvsrem, a, b)
  def bvurem(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvurem, a, b)
  def bvsmod(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvsmod, a, b)
  def bvand(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvand, a, b)
  def bvor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvor, a, b)
  def bvxor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvxor, a, b)
  def bvnand(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvnand, a, b)
  def bvnor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvnor, a, b)
  def bvxnor(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvxnor, a, b)
  def bvnot(a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvnot, a)
  def bvneg(a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] = apply(BitVectorEval.smt_bvneg, a)
  def bvshl(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvshl, a, b)
  def bvlshr(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvlshr, a, b)
  def bvashr(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvashr, a, b)
  def bvcomp(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_bvcomp, a, b)
  def zero_extend(width: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_zero_extend(width, _: BitVecLiteral), a)
  def sign_extend(width: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_sign_extend(width, _: BitVecLiteral), a)
  def extract(high: Int, low: Int, a: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.boogie_extract(high, low, _: BitVecLiteral), a)
  def concat(a: FlatElement[BitVecLiteral], b: FlatElement[BitVecLiteral]): FlatElement[BitVecLiteral] =
    apply(BitVectorEval.smt_concat, a, b)
}

/** Constant propagation lattice.
  */
class ConstantPropagationLatticeWithSSA extends PowersetLattice[BitVecLiteral] {
  private def apply(
    op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral,
    a: Set[BitVecLiteral],
    b: Set[BitVecLiteral]
  ): Set[BitVecLiteral] =
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
  def zero_extend(width: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    apply(BitVectorEval.smt_zero_extend(width, _: BitVecLiteral), a)
  def sign_extend(width: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    apply(BitVectorEval.smt_sign_extend(width, _: BitVecLiteral), a)

  def extract(high: Int, low: Int, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    apply(BitVectorEval.boogie_extract(high, low, _: BitVecLiteral), a)

  def concat(a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = apply(BitVectorEval.smt_concat, a, b)
}
