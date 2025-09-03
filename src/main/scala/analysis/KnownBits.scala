package analysis
import ir.*
import ir.eval.InfixBitVectorEval.given
import ir.eval.InfixBitVectorEval.{
  bvuge as _,
  bvugt as _,
  bvule as _,
  bvult as _,
  sign_extend as _,
  zero_extend as _,
  *
}
import ir.eval.{BitVectorEval, InfixBitVectorEval}
import ir.transforms.applyRPO
import util.assertion.*
import util.writeToFile

import scala.language.implicitConversions

/**
 *
 * Known bits analysis using tristate numbers.
 *
 *  https://arxiv.org/abs/2105.05398
 *
 *  A = definite 1 bits, B = unknown bits
 *  A_[i] = 1, B_[i] = 0 -> Bit i of TNum _ is definitely 1
 *  A_[i] = 0, B_[i] = 0 -> Bit i of TNum _ is definitely 0
 *  A_[i] = 0, B_[i] = 1 -> Bit i of TNum _ is unknown (T)
 *  where A_[i] = value and B_[i] = mask (value and mask are unsigned BigInt)
 *
 *  e.g.
 *  x = {1, T, 0, T}
 *  Ax = 1000, Bx = 0101
 *
 *  z = {T, 1, 1, 0}
 *  Az = 0110, Bz = 1000
 *
 *  Bitwise AND (y = x & z):
 *  Ay[i] = Ax[i] & Az[i]    ->    Ay[i] = 0000
 *  By[i] = (Ax[i] | Bx[i]) & (Az[i] | Bz[i]) & ~(Ax[i] & Az[i])   ->   By[i] = 1100
 *  y = {T, T, 0, 0}
 *
 *  Bitwise OR:
 *  Ay[i] = Ax[i] | Az[i]    ->    Ay[i] = 1110
 *  By[i] = (Bx[i] | Bz[i]) & ~(Ax[i] | Az[i])   ->   By[i] = 0001
 *  y = {1, 1, 1, T}
 *
 *  Bitwise XOR:
 *  Ay[i] = (Ax[i] ^ Az[i]) & ~(Bx[i] | Bz[i])   ->    Ay[i] = 0010
 *  By[i] = (Bx[i] | Bz[i])    ->    By[i] = 1101
 *  y = {T, T, 1, T}
 */

object TNum {
  def trueBool = TNum(1.bv1, 0.bv1)
  def falseBool = TNum(0.bv1, 0.bv1)
  def unkBool = TNum(0.bv1, 1.bv1)
  def top(width: Int) = TNum(0.bv(width), BitVecLiteral(BitVecType(width).maxValue, width))
}

case class TNum(value: BitVecLiteral, mask: BitVecLiteral) {
  import TNum.*

  def width: Int = {
    require(value.size == mask.size)
    value.size
  }

  def getType = BitVecType(width)

  def wellFormed: Boolean = {
    value.value >= 0 && mask.value >= 0 && value.size >= 0 && mask.size >= 0 && value.size == mask.size && ((value & mask) == 0
      .bv(width))
  }

  debugAssert(wellFormed, s"not well formed $this")

  def top = {
    TNum(0.bv(width), BitVecLiteral(BitVecType(width).maxValue, width))
  }

  def zero = TNum(0.bv(width), 0.bv(width))
  def constant(n: Int) = {
    TNum(n.bv(width), 0.bv(width))
  }
  def constant(n: BitVecLiteral) = {
    TNum(n, 0.bv(n.size))
  }

  override def toString() = {
    val padwidth = width / 4 + (if width % 4 != 0 then 1 else 0)
    def padded(number: BigInt) = {
      "0x" + ("%x".format(number).reverse.padTo(width / 4, '0').reverse)
    }
    "(%s, %s, bv%d)".format(padded(value.value), padded(mask.value), width)
  }

  def intersect(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val mu = this.mask | that.mask | (this.value ^ that.value)
    val v = this.value & that.value
    TNum(v, mu);
  }

  def join(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    this.intersect(that)
  }

  // Bitwise AND
  def TAND(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val alpha = this.value | this.mask
    val beta = that.value | that.mask
    val v = this.value & that.value
    TNum(v, (alpha & beta & ~v))
  }

  // Bitwise OR
  def TOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val v = this.value | that.value
    val mu = this.mask | that.mask
    TNum(v, (mu & ~v));
  }

  // Bitwise XOR
  def TXOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val v = this.value ^ that.value
    val mu = this.mask | that.mask
    TNum((v & ~mu), mu)
  }

  // Bitwise NOR
  def TNOR(that: TNum): TNum = {
    (this.TOR(that)).TNOT()
  }

  // Bitwise XNOR
  def TXNOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    TXOR(that).TNOT()
  }

  // Bitwise NAND
  def TNAND(that: TNum): TNum = {
    TAND(that).TNOT()
  }

  // Addition
  def TADD(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val sm = this.mask + that.mask
    val sv = this.value + that.value
    val sigma = sm + sv
    val chi = sigma ^ sv
    val mu = chi | this.mask | that.mask
    TNum(sv & ~mu, mu)
  }

  // Multiplication
  def TMUL(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    var acc_v = this.value * that.value
    var acc_m = constant(0)

    var a = this
    var b = that

    while ((a.value | a.mask) != 0.bv(width)) {
      if (a.value & 1.bv(a.width)) {
        acc_m = acc_m.TADD(TNum(0.bv(b.width), b.mask))
      } else if (a.mask & 1.bv(a.width)) {
        acc_m = acc_m.TADD(TNum(0.bv(b.width), b.value | b.mask))
      }

      a = a.TLSHR(constant(1))
      b = b.TSHL(constant(1))
    }

    constant(acc_v).TADD(acc_m)
  }

  // Subtraction
  def TSUB(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    require(that.wellFormed)
    val dv = this.value - that.value
    val alpha = dv + this.mask
    val beta = dv - that.mask
    val chi = alpha ^ beta
    val mu = chi | this.mask | that.mask
    TNum((dv & ~mu), mu)
  }

  // Bitwise Comparison
  def TCOMP(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    TEQ(that)
  }

  // Shift Left
  def TSHL(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    // Lower and upper bounds of shift value
    def thatLB = (that.value & ~that.mask)
    def thatUB = (that.value | that.mask)

    // Value and mask accumulator begins with lower bound
    var accValue = this.value << thatLB
    var accMask = this.mask << thatLB

    // Iterate through each shift value

    val lower = thatLB.value.toInt
    val upper = Math.min(thatUB.value.toInt, that.width)
    for (ii <- lower to upper) {
      val i = ii.bv(width)
      // Check if the shift is possible
      if ((i & ~that.mask) == that.value) {
        accMask |= (this.mask << i) | ((this.value << i) ^ accValue)
        accValue &= this.value << i
      }
    }

    if (thatUB >= this.width.bv(width)) {
      accMask |= accValue
      accValue = 0.bv(width)
    }

    TNum(accValue, accMask)
  }

  // Logical Shift Right
  def TLSHR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")

    // Lower and upper bounds of shift value
    val thatLB = (that.value & ~that.mask)
    val thatUB = (that.value | that.mask)

    // Value and mask accumulator begins with lower bound
    var accValue = this.value >>> thatLB
    var accMask = this.mask >>> thatLB

    // Iterate through each shift value
    val lower = thatLB.value.toInt
    val upper = Math.min(thatUB.value.toInt, this.width - 1)
    for (ii <- lower to upper) {
      val i = ii.bv(width)
      // Check if the shift is possible
      if ((i & ~that.mask) == that.value) {
        accMask = accMask | ((this.mask >>> i) | (this.value >>> i) ^ accValue)
        accValue = accValue & (this.value >>> i)
      }
    }

    if (thatUB >= width.bv(width)) {
      accMask |= accValue
      accValue = 0.bv(width)
    }

    TNum(accValue, accMask)
  }

  // Arithmetic Shift Right
  def TASHR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    // Lower and upper bounds of shift value
    val thatLB = (that.value & ~that.mask)
    val thatUB = (that.value | that.mask)

    // Value and mask accumulator begins with lower bound
    var accValue = this.value >> thatLB
    var accMask = this.mask >> thatLB

    // Iterate through each shift value

    val lower = thatLB.value.toInt
    val upper = Math.min(thatUB.value.toInt, this.width - 1)
    //  for (i <- thatLB to Math.min(thatUB, this.width - 1)) {

    for (ii <- lower to upper) {
      val i = ii.bv(width)
      // Check if the shift is possible
      if ((i & ~that.mask) == that.value) {
        accMask = accMask | ((this.mask >> i) | (this.value >> i) ^ accValue)
        accValue = accValue & (this.value >> i)
      }
    }

    if (thatUB >= this.width.bv(width)) {
      accMask = accMask | (this.mask >> this.width.bv(width) | (this.value >> this.width.bv(width)) ^ accValue)
      accValue = accValue & (this.value >> this.width.bv(width))
    }

    TNum(accValue, accMask)
  }

  // TODO
  // Unsigned Division
  def TUDIV(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & ~this.mask
    val dividendH = this.value | this.mask

    val divisorL = that.value & ~that.mask
    val divisorH = that.value | that.mask

    if (divisorL == 0.bv(width) || divisorH == 0.bv(width)) {
      return top
    }

    val q1 = dividendL / divisorL
    val q2 = dividendL / divisorH
    val q3 = dividendH / divisorL
    val q4 = dividendH / divisorH

    val newValue = q1 & q2 & q3 & q4
    val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

    TNum(newValue, newMask)
  }

  // Converts a BigInt to signed representation
  def toSigned(n: BigInt): BigInt = {
    val msb = BigInt(1) << (this.width - 1)
    if ((n & msb) != 0) n - (msb << 1) else n
  }

  // TODO
  // Signed Division
  def TSDIV(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & ~this.mask
    val dividendH = this.value | this.mask

    val divisorL = that.value & ~that.mask
    val divisorH = that.value | that.mask

    if (divisorL == 0.bv(width) || divisorH == 0.bv(width)) {
      return top
    }

    val q1 = dividendL / divisorL
    val q2 = dividendL / divisorH
    val q3 = dividendH / divisorL
    val q4 = dividendH / divisorH

    val newValue = q1 & q2 & q3 & q4
    val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

    TNum(newValue, newMask)
  }

  // TODO
  // Unsigned Remainder
  def TUREM(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & ~this.mask
    val dividendH = this.value | this.mask

    val divisorL = that.value & ~that.mask
    val divisorH = that.value | that.mask

    if (divisorL == 0.bv(width) || divisorH == 0.bv(width)) {
      return top
    }

    val r1 = dividendL % divisorL
    val r2 = dividendL % divisorH
    val r3 = dividendH % divisorL
    val r4 = dividendH % divisorH

    val newValue = r1 & r2 & r3 & r4
    val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

    TNum(newValue, newMask)
  }

  // TODO
  // Signed Remainder
  def TSREM(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & ~this.mask
    val dividendH = this.value | this.mask

    val divisorL = that.value & ~that.mask
    val divisorH = that.value | that.mask

    if (divisorL == 0.bv(width) || divisorH == 0.bv(width)) {
      return top
    }

    // Sign extend both dividend and divisor and convert to signed representation before division
    val r1 = dividendL % divisorL
    val r2 = dividendL % divisorH
    val r3 = dividendH % divisorL
    val r4 = dividendH % divisorH

    val newValue = r1 & r2 & r3 & r4
    val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

    TNum(newValue, newMask)
  }

  // Converts a remainder to a modulo result
  def toModulo(remainder: BitVecLiteral, dividend: BitVecLiteral, divisor: BitVecLiteral): BitVecLiteral = {
    if (remainder < 0.bv(width)) {
      if (divisor < 0.bv(width)) {
        remainder - divisor
      } else {
        remainder + divisor
      }
    } else {
      remainder
    }
  }

  // TODO
  // Signed Modulo
  def TSMOD(that: TNum): TNum = {
    require(width == that.width)
    val dividendL = this.value & ~this.mask
    val dividendH = this.value | this.mask

    val divisorL = that.value & ~that.mask
    val divisorH = that.value | that.mask

    if (divisorL == 0.bv(width) || divisorH == 0.bv(width)) {
      return top
    }

    // Determine maximum bit length for sign extension

    // Sign extend both dividend and divisor and convert to signed representation
    val r1 = dividendL % divisorL
    val r2 = dividendL % divisorH
    val r3 = dividendH % divisorL
    val r4 = dividendH % divisorH

    val modr1 = toModulo(r1, dividendL, divisorL)
    val modr2 = toModulo(r2, dividendL, divisorH)
    val modr3 = toModulo(r3, dividendH, divisorL)
    val modr4 = toModulo(r4, dividendH, divisorH)

    // Combine the adjusted remainders
    val newValue = modr1 & modr2 & modr3 & modr4
    val newMask =
      (modr1 ^ modr2) | (modr1 ^ modr3) | (modr1 ^ modr4) | (modr2 ^ modr3) | (modr2 ^ modr4) | (modr3 ^ modr4)

    TNum(newValue, newMask)
  }

  // Two's complement negation
  def TNEG(): TNum = {
    constant(0).TSUB(this)
  }

  // Bitwise Not
  def TNOT(): TNum = {
    TNum(~this.value & ~this.mask, this.mask)
  }

  // Equality (TNum cannot have Top elements when checking for equality)
  def TEQ(that: TNum): TNum = {
    require(width == that.width)
    if ((this.mask == 0.bv(width) && that.mask == 0.bv(width))) then {
      if (this.value == that.value) then trueBool else falseBool
    } else {
      unkBool
    }
  }

  // Not Equal (If a Top element exists, we assume not equal since Top could be 1 or 0)
  def TNEQ(that: TNum): TNum = {
    this.TEQ(that).TNOT()
  }

  // Get smallest possible unsigned value of the TNum (e.g. Min value of TT0 is 000)
  def minUnsigned = mustBits.value

  // Get largest possible unsigned value of the TNum (e.g. Max value of TT0 is 110)
  def maxUnsigned = mayBits.value

  def mustBits = (this.value & ~this.mask)
  def mustNotBits = ((~this.value) & ~this.mask)
  def mayBits = this.value | this.mask

  def maxPositive: Option[BitVecLiteral] = {
    if (mustBits(width, width - 1) < 0.bv(width)) {
      // must be negative
      None
    } else {
      Some(InfixBitVectorEval.zero_extend(1, mayBits(width, width - 1)))
    }
  }

  def minPositive: Option[BitVecLiteral] = {
    if (mustBits(width, width - 1) < 0.bv(width)) {
      // must be negative
      None
    } else {
      Some(mustBits)
    }
  }

  def maxNegative: Option[BitVecLiteral] = {
    if (mustBits(width, width - 1) > 0.bv(width)) {
      // must be positive
      None
    } else {
      // all possible bits set incl msb
      Some(mayBits)
    }
  }

  def minNegative: Option[BitVecLiteral] = {
    if (mustBits(width, width - 1) > 0.bv(width)) {
      // must be positive
      None
    } else {
      // fewest bits set, and msb set
      Some(mustBits | (1.bv(width) << (width - 1).bv(width)))
    }
  }

  // Get smallest possible signed value of the TNum
  def minSigned: BitVecLiteral = minNegative.getOrElse(minPositive.get)

  // Get largest possible signed value of the TNum
  def maxSigned: BitVecLiteral = maxPositive.getOrElse(maxNegative.get)

  def slt(that: TNum): Option[Boolean] = if (this.maxSigned < that.minSigned) then Some(true) else None
  def sle(that: TNum): Option[Boolean] = if (this.maxSigned <= that.minSigned) then Some(true) else None
  def ult(that: TNum): Option[Boolean] = if (this.maxUnsigned < that.minUnsigned) then Some(true) else None
  def ule(that: TNum): Option[Boolean] = if (this.maxUnsigned <= that.minUnsigned) then Some(true) else None

  def sgt(that: TNum): Option[Boolean] = if (this.minSigned > that.maxSigned) then Some(true) else None
  def sge(that: TNum): Option[Boolean] = if (this.minSigned >= that.maxSigned) then Some(true) else None
  def ugt(that: TNum): Option[Boolean] = if (this.minUnsigned > that.maxUnsigned) then Some(true) else None
  def uge(that: TNum): Option[Boolean] = if (this.minUnsigned >= that.maxUnsigned) then Some(true) else None

  def isOrElseNot(soundOp: TNum => Option[Boolean], soundDualOp: TNum => Option[Boolean])(t: TNum): TNum = {
    soundOp(t).orElse(soundDualOp(t).map(x => !x)) match {
      case Some(true) => trueBool
      case Some(false) => falseBool
      case None => unkBool
    }
  }

  def TULT(that: TNum): TNum = isOrElseNot(ult, uge)(that)

  def TSLT(that: TNum): TNum = isOrElseNot(slt, sge)(that)

  def TULE(that: TNum): TNum = isOrElseNot(ule, ugt)(that)

  def TSLE(that: TNum): TNum = isOrElseNot(sle, sgt)(that)

  def TUGT(that: TNum): TNum = isOrElseNot(ugt, ule)(that)

  def TSGT(that: TNum): TNum = isOrElseNot(sgt, sle)(that)

  def TUGE(that: TNum): TNum = isOrElseNot(uge, ult)(that)

  def TSGE(that: TNum): TNum = isOrElseNot(sge, slt)(that)

  // Concatenation
  def TCONCAT(that: TNum): TNum = {
    val v = this.value ++ that.value
    val mu = this.mask ++ that.mask
    TNum(v, mu)
  }

  inline def mapBoth(f: BitVecLiteral => BitVecLiteral) =
    TNum(f(value), f(mask))
}

given TypedValueLattice[TNum, IRType] with {
  import TNum.*

  def getType(x: TNum) = BitVecType(x.width)

  def top(ty: IRType): TNum = ty match {
    case BitVecType(w) => TNum.top(w)
    case ty => throw Exception("unable to construct top TNum for type: " + ty)
  }

  // XXX: the reference defines "bottom" as having at least one
  // position which is simultaneously set in the mask and value.
  // it is not clear if this is something that we can do without
  // adding special cases for all the operations to detect and
  // propagate this.
  def bottom(ty: IRType): TNum = throw Exception("TNum has no bottom")
  def bottom: TNum = throw Exception("TNum has no bottom")

  def top: TNum = throw Exception("TNum has no universal top")

  def constant(v: ir.Literal): TNum = v match {
    case x: BitVecLiteral => constant(x)
    case TrueLiteral => trueBool
    case FalseLiteral => falseBool
    case IntLiteral(x) => throw Exception("TNum undefined for integers")
  }

  def glb(x: TNum, y: TNum) = x.meet(y)
  def lub(x: TNum, y: TNum) = x.join(y)

  def booland(x: TNum, other: TNum): TNum = x.bvand(other)
  def boolnot(x: TNum): TNum = x.bvnot()
  def boolor(x: TNum, other: TNum): TNum = x.bvor(other)
  def booltobv1(x: TNum): TNum = x // bools are already bv1
  def bvadd(x: TNum, other: TNum): TNum = x.TADD(other)
  def bvand(x: TNum, other: TNum): TNum = x.TAND(other)
  def bvashr(x: TNum, other: TNum): TNum = x.TASHR(other)
  def bvcomp(x: TNum, other: TNum): TNum = x.TCOMP(other)
  def bvconcat(x: TNum, other: TNum): TNum = x.TCONCAT(other)
  def bvlshr(x: TNum, other: TNum): TNum = x.TLSHR(other)
  def bvmul(x: TNum, other: TNum): TNum = x.TMUL(other)
  def bvneg(x: TNum): TNum = x.TNEG()
  def bvnot(x: TNum): TNum = x.TNOT()
  def bvor(x: TNum, other: TNum): TNum = x.TOR(other)
  def bvsge(x: TNum, other: TNum): TNum = x.TSGE(other)
  def bvsgt(x: TNum, other: TNum): TNum = x.TSGT(other)
  def bvshl(x: TNum, other: TNum): TNum = x.TSHL(other)
  def bvsle(x: TNum, other: TNum): TNum = x.TSLE(other)
  def bvslt(x: TNum, other: TNum): TNum = x.TSLT(other)
  def bvsub(x: TNum, other: TNum): TNum = x.TSUB(other)
  def bvuge(x: TNum, other: TNum): TNum = x.TUGE(other)
  def bvugt(x: TNum, other: TNum): TNum = x.TUGT(other)
  def bvule(x: TNum, other: TNum): TNum = x.TULE(other)
  def bvult(x: TNum, other: TNum): TNum = x.TULT(other)

  // TODO: TNum division-related functions currently broken
  def bvsmod(x: TNum, other: TNum): TNum = top(x.getType)
  def bvsrem(x: TNum, other: TNum): TNum = top(x.getType)
  def bvudiv(x: TNum, other: TNum): TNum = top(x.getType)
  def bvurem(x: TNum, other: TNum): TNum = top(x.getType)
  def bvsdiv(x: TNum, other: TNum): TNum = top(x.getType)

  def bvxor(x: TNum, other: TNum): TNum = x.TXOR(other)
  def equal(x: TNum, other: TNum): TNum = x.TEQ(other)
  def extract(x: TNum, hi: Int, lo: Int): TNum = TNum(x.value(hi, lo), x.mask(hi, lo))

  def intadd(x: TNum, other: TNum): TNum = x.bvadd(other)
  def intdiv(x: TNum, other: TNum): TNum = x.bvsdiv(other)
  def intge(x: TNum, other: TNum): TNum = x.bvsge(other)
  def intgt(x: TNum, other: TNum): TNum = x.bvsgt(other)
  def intle(x: TNum, other: TNum): TNum = x.bvsle(other)
  def intlt(x: TNum, other: TNum): TNum = x.bvslt(other)
  def intmod(x: TNum, other: TNum): TNum = x.bvsmod(other)
  def intmul(x: TNum, other: TNum): TNum = x.bvmul(other)
  def intneg(x: TNum): TNum = x.bvneg()
  def intsub(x: TNum, other: TNum): TNum = x.bvsub(other)
  def repeat(x: TNum, repeats: Int): TNum = x.mapBoth(BitVectorEval.repeat_bits(repeats, _))
  def sign_extend(x: TNum, extend: Int): TNum = x.mapBoth(InfixBitVectorEval.sign_extend(extend, _))
  def zero_extend(x: TNum, extend: Int): TNum = x.mapBoth(InfixBitVectorEval.zero_extend(extend, _))
  }

def knownBitsAnalysis(p: Program) = {
  applyRPO(p)
  val lattice = DefaultValueLattice[TNum]()
  val solver = transforms.worklistSolver(ValueStateDomain(DefaultTransfer(lattice), lattice))

  val (beforeIn, afterIn) = solver.solveProgIntraProc(p, backwards = false)
  (beforeIn, afterIn)
}

class SimplifyKnownBits() {
  val lattice = DefaultValueLattice[TNum]()
  val solver = transforms.worklistSolver(ValueStateDomain(DefaultTransfer(lattice), lattice))

  def applyTransform(p: Program): Unit = {
    for (proc <- p.procedures) {
      applyTransform(proc)
    }
  }

  def applyTransform(procedure: Procedure): Unit = {
    val (beforeIn, afterIn) = solver.solveProc(procedure, backwards = false)
    writeToFile(
      translating.PrettyPrinter.pp_proc_with_analysis_results(beforeIn, afterIn, procedure, x => x.toString),
      s"${procedure.name}_known_bits.il"
    )
  }
}
