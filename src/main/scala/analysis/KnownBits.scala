package analysis
import ir.*
import ir.eval.BitVectorEval
import ir.eval.InfixBitVectorEval.*
import ir.eval.InfixBitVectorEval.given
import ir.transforms.{AbstractDomain, applyRPO}
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

  def wellFormed: Boolean = {
    value.value >= 0 && mask.value >= 0 && value.size >= 0 && mask.size >= 0 && value.size == mask.size && ((value & mask) == 0
      .bv(width))
  }

  debugAssert(wellFormed, s"not well formed $this")

  def top() = {
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
    val beta = that.value
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
      return top()
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
      return top()
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
      return top()
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
      return top()
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
      return top()
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
      Some(zero_extend(1, mayBits(width, width - 1)))
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
}

class TNumDomain extends AbstractDomain[Map[Variable, TNum]] {
  override def top: Map[Variable, TNum] = Map.empty
  override def bot: Map[Variable, TNum] = Map.empty

  def sizeBits(v: IRType) = v match {
    case BoolType => 1
    case BitVecType(n) => n
    case IntType => Integer.MAX_VALUE
    case _ => ???
  }

  // Converts a bitvector or integer literal to a TNum
  def ofLiteral(literal: Literal): TNum = literal match {
    case bv: BitVecLiteral =>
      TNum(bv, BitVecLiteral(0, bv.size))
    case iv: IntLiteral =>
      val w = sizeBits(iv.getType)
      TNum(BitVecLiteral(iv.value, w), 0.bv(w))
    case TrueLiteral => TNum.trueBool
    case FalseLiteral => TNum.falseBool
  }

  // Evaluates binary operation and returns either a TNum or TNum
  def evaluateValueBinOp(op: BVBinOp | IntBinOp, tn1: TNum, tn2: TNum): TNum = {
    op match {
      case BVAND => tn1.TAND(tn2)
      case BVOR => tn1.TOR(tn2)
      case BVXOR => tn1.TXOR(tn2)
      case BVNOR => tn1.TNOR(tn2)
      case BVXNOR => tn1.TXNOR(tn2)
      case BVNAND => tn1.TNAND(tn2)
      case BVADD => tn1.TADD(tn2)
      case BVMUL => tn1.TMUL(tn2)
      case BVUDIV => tn1.top() // broken // tn1.TUDIV(tn2)
      case BVUREM => tn1.top() // broekn tn1.TUREM(tn2)
      case BVSDIV => tn1.top() // tn1.TSDIV(tn2) // broken
      case BVSREM => tn1.top() // tn1.TSREM(tn2) // broken
      case BVSMOD => tn1.top() // tn1.TSMOD(tn2) // broken
      case BVSHL => tn1.TSHL(tn2)
      case BVLSHR => tn1.TLSHR(tn2)
      case BVULT => tn1.TULT(tn2)
      case BVCOMP => tn1.TCOMP(tn2)
      case BVSUB => tn1.TSUB(tn2)
      case BVASHR => tn1.TASHR(tn2)
      case BVULE => tn1.TULE(tn2)
      case BVUGT => tn1.TUGT(tn2)
      case BVUGE => tn1.TUGE(tn2)
      case BVSLT => tn1.TSLT(tn2)
      case BVSLE => tn1.TSLE(tn2)
      case BVSGT => tn1.TSGT(tn2)
      case BVSGE => tn1.TSGE(tn2)
      case BVCONCAT => tn1.TCONCAT(tn2)
      case IntADD => tn1.TADD(tn2)
      case IntMUL => tn1.TMUL(tn2)
      case IntSUB => tn1.TSUB(tn2)
      case IntDIV => tn1.TSDIV(tn2)
      case IntMOD => tn1.TSMOD(tn2)
      case IntLT => tn1.TSLT(tn2)
      case IntLE => tn1.TSLE(tn2)
      case IntGT => tn1.TSGT(tn2)
      case IntGE => tn1.TSGE(tn2)
    }
  }

  def evaluateBoolBinOp(op: BoolBinOp, tn1: TNum, tn2: TNum): TNum = {
    op match {
      case BoolAND => tn1.TAND(tn2)
      case BoolOR => tn1.TOR(tn2)
      case BoolIMPLIES => (tn1.TOR(tn2.TNOT()))
    }
  }

  // Evaluates unary operations
  def evaluateValueUnOp(op: BVUnOp | IntUnOp, tn: TNum): TNum = {
    op match {
      case BVNOT => tn.TNOT()
      case BVNEG => tn.TNEG()
      case IntNEG => tn.TNEG()
    }
  }

  def evaluateBoolUnOp(op: BoolUnOp, tn: TNum): TNum = {
    op match {
      case BoolNOT => tn.TNOT()
      case BoolToBV1 => tn
    }
  }

  // Recursively evaluates nested or non-nested expression
  def evaluateExprToTNum(s: Map[Variable, TNum], expr: Expr): TNum =
    val r = expr match {
      case b: AssocExpr => evaluateExprToTNum(s, b.toBinaryExpr)
      case u: FApplyExpr => TNum.top(sizeBits(u.getType))
      case u: LambdaExpr => TNum.top(sizeBits(u.getType))
      case u: QuantifierExpr => TNum.top(sizeBits(u.getType))
      case u: OldExpr => TNum.top(sizeBits(u.getType))
      case l: Literal => ofLiteral(l)
      case v: Variable => s.getOrElse(v, TNum.top(sizeBits(v.getType)))
      case UnaryExpr(op: UnOp, arg: Expr) =>
        val argTNum = evaluateExprToTNum(s, arg)
        (op, argTNum) match {
          case (opVal: BVUnOp, tnum: TNum) => evaluateValueUnOp(opVal, tnum)
          case (opVal: IntUnOp, tnum: TNum) => evaluateValueUnOp(opVal, tnum)
          case (opVal: BoolUnOp, tnum: TNum) => evaluateBoolUnOp(opVal, tnum)
        }

      case BinaryExpr(op, arg1: Expr, arg2: Expr) =>
        val arg1TNum = evaluateExprToTNum(s, arg1)
        val arg2TNum = evaluateExprToTNum(s, arg2)

        (op, arg1TNum, arg2TNum) match {
          case (EQ, tn1, tn2) => tn1.TEQ(tn2)
          case (NEQ, tn1, tn2) => tn1.TNEQ(tn2)
          case (opVal: BVBinOp, tnum1: TNum, tnum2: TNum) => evaluateValueBinOp(opVal, tnum1, tnum2)
          case (opVal: IntBinOp, tnum1: TNum, tnum2: TNum) => evaluateValueBinOp(opVal, tnum1, tnum2)
          case (opVal: BoolBinOp, tnum1: TNum, tnum2: TNum) => evaluateBoolBinOp(opVal, tnum1, tnum2)
        }

      case Extract(hi: Int, lo: Int, body: Expr) => {
        val tnum = evaluateExprToTNum(s, body)
        TNum(tnum.value(hi, lo), tnum.mask(hi, lo))
      }

      case Repeat(repeats: Int, body: Expr) =>
        val bodyTNum = evaluateExprToTNum(s, body)

        bodyTNum match {
          case tnum: TNum =>
            val repeatedValue = BitVectorEval.repeat_bits(repeats, tnum.value)
            val repeatedMask = BitVectorEval.repeat_bits(repeats, tnum.mask)
            TNum(repeatedValue, repeatedMask)
        }

      case ZeroExtend(ex: Int, body: Expr) =>
        val b = evaluateExprToTNum(s, body)
        TNum(zero_extend(ex, b.value), zero_extend(ex, b.mask))

      case SignExtend(ex: Int, body: Expr) =>
        val tnum = evaluateExprToTNum(s, body)
        TNum(sign_extend(ex, tnum.value), sign_extend(ex, tnum.mask))
      case _: StackMemory => ???
      case _: SharedMemory => ???
    }
    r

  // s is the abstract state from previous command/block
  override def transfer(s: Map[Variable, TNum], b: Command): Map[Variable, TNum] = {
    val r = b match {
      // Assign variable to variable (e.g. x = y)
      case SimulAssign(assignments, _) => {
        s ++ assignments.map { case (lhs, rhs) =>
          lhs -> evaluateExprToTNum(s, rhs)
        }
      }
      case LocalAssign(lhs: Variable, rhs: Expr, _) =>
        s.updated(lhs, evaluateExprToTNum(s, rhs))

      // Load from memory and store in variable
      case MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int, _) if !s.contains(lhs) =>
        // Overapproxiate memory values with Top
        s.updated(lhs, TNum.top(size))

      case i: IndirectCall => Map()
      case a: Assign => s ++ a.assignees.map(l => l -> TNum.top(sizeBits(l.irType)))
      // Default case
      case _: NOP => s
      case _: Assert => s
      case _: Assume => s
      case _: GoTo => s
      case _: Return => s
      case _: Unreachable => s
      case _: MemoryStore => s
    }
    r
  }

  override def join(left: Map[Variable, TNum], right: Map[Variable, TNum], pos: Block): Map[Variable, TNum] = {
    join(left, right)
  }

  /**
   * Joins the same variables and merges TNum values using bitwise OR
   *
   *   e.g. Join: x = 0011, x = 1111
   *   x = 0011 => value = 0011, mask = 0000
   *   x = 1111 => value = 1111, mask = 0000
   *   Joined x = 1111 => value = 1111, mask = 0000
   */
  def join(left: Map[Variable, TNum], right: Map[Variable, TNum]): Map[Variable, TNum] = {
    (left.keySet ++ right.keySet).map { key =>
      val width = sizeBits(key.getType)
      val leftTNum = left.getOrElse(key, TNum.top(width))
      val rightTNum = right.getOrElse(key, TNum.top(width))

      if (left.contains(key) && !right.contains(key)) {
        // Only left map contains key
        key -> leftTNum
      } else if (!left.contains(key) && right.contains(key)) {
        // Only right map contains key
        key -> rightTNum
      } else {
        // Merge the TNum of variables that appear in both program states but need to be compatible
        // OR the unknown bits, AND the known bits
        (leftTNum, rightTNum) match {
          case (left: TNum, right: TNum) if left.width == right.width => {
            key -> left.join(right)
          }
          // case (left: TNum, right: TNum) => key -> left.TOR(right)
          case _ => key -> TNum.top(leftTNum.width)
        }
      }
    }.toMap
  }
}

def knownBitsAnalysis(p: Program) = {
  applyRPO(p)
  val solver = transforms.worklistSolver(TNumDomain())
  val (beforeIn, afterIn) = solver.solveProgIntraProc(p, backwards = false)
  (beforeIn, afterIn)
}

class SimplifyKnownBits() {
  val solver = transforms.worklistSolver(TNumDomain())

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
