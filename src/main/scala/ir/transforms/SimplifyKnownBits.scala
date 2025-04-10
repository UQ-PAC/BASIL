package ir.transforms
import ir.*
import util.writeToFile
import ir.eval.BitVectorEval

/**
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


def bvnot(v: BigInt, size: Int) = {
  val x = BitVecType(size).maxValue & v
  BitVectorEval.smt_bvnot(BitVecLiteral(x, size)).value
}

case class TNum(value: BigInt, mask: BigInt, width: Int) {

  def wellFormed : Boolean = {
    value >= 0 && mask >= 0 && width >= 0 && ((value & mask) == 0)
  }

  assert(wellFormed, s"not well formed $this")

  def top() = {
    TNum(0, BitVecType(width).maxValue, width)
  }

  def trueBool = TNum(1, 0, 1)
  def falseBool = TNum(0, 0, 1)
  def unkBool = TNum(0, 1, 1)

  override def toString() = {
    val padwidth = width / 4 + (if width % 4 != 0 then 1 else 0)
    def padded(number: BigInt) = {
      "0x" + ("%x".format(number).reverse.padTo(width / 4, '0').reverse)
    }
    "(%s, %s, bv%d)".format(padded(value), padded(mask), width)
  }

  def join(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val mu = this.mask | that.mask | (this.value ^ that.value)
    val v = this.value & that.value
    TNum(v, mu, this.width);
  }

  // Bitwise AND
  def TAND(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val alpha = this.value | this.mask
    val beta = that.value | that.mask
    val v = this.value & that.value
    TNum(v, (alpha & beta & bvnot(v, width)), this.width)
  }

  // Bitwise OR
  def TOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val v = this.value | that.value
    val mu = this.mask | that.mask
    TNum(v, (mu & bvnot(v, width)), this.width);
  }

  // Bitwise XOR
  def TXOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val v = this.value ^ that.value
    val mu = this.mask | that.mask
    TNum((v & bvnot(mu, width)), mu, this.width)
  }

  // Bitwise NOR
  def TNOR(that: TNum): TNum = {
    (this.TOR(that)).TNOT()
  }

  // Bitwise XNOR
  def TXNOR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val v = bvnot(this.value ^ that.value, width)
    val mu = this.mask | that.mask
    TNum((v & bvnot(mu, width)), mu, this.width)
  }

  // Bitwise NAND
  def TNAND(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val alpha = this.value | this.mask
    val beta = that.value | that.mask
    val v = this.value & that.value
    TNum(
      (bvnot(v, width) & bvnot(alpha & beta & bvnot(v, width), width)),
      (alpha & beta & bvnot(v, width)),
      this.width
    )
  }

  // Addition
  def TADD(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val sm = this.mask + that.mask
    val sv = this.value + that.value
    val sigma = sm + sv
    val chi = sigma ^ sv
    val mu = chi | this.mask | that.mask
    TNum(sv & bvnot(mu, width), mu, this.width)
  }

  // Multiplication
  def TMUL(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    var a = this
    var b = that
    val v = this.value * that.value
    var mu = TNum(BigInt(0), BigInt(0), this.width)

    while ((a.value | a.mask) != 0) {
      if ((a.value & BigInt(1)) != 0) {
        mu = mu.TADD(TNum(BigInt(0), b.mask, this.width))
      } else if ((a.mask & BigInt(1)) != 0) {
        mu = mu.TADD(TNum(BigInt(0), b.value | b.mask, this.width))
      }
      a = a.TLSHR(TNum(BigInt(1), BigInt(0), this.width))
      b = b.TSHL(TNum(BigInt(1), BigInt(0), this.width))
    }
    TNum(v, BigInt(0), this.width).TADD(mu)
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
    TNum((dv & bvnot(mu, width)), mu, this.width)
  }

  // Bitwise Comparison
  def TCOMP(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    if ((this.mask == 0 && that.mask == 0) && (this.value == that.value)) {
      TNum(BigInt(1), BigInt(0), 1)
    } else if (this.value != that.value) {
      TNum(BigInt(0), BigInt(0), 1)
    } else {
      TNum(BigInt(0), BigInt(1), 1)
    }
  }

  // Shift Left
  def TSHL(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    // Lower and upper bounds of shift value
    def thatLB = (that.value & bvnot(that.mask, that.width)).toInt
    def thatUB = (that.value | that.mask).toInt

    // Value and mask accumulator begins with lower bound
    var accValue = bvshl(this.value, thatLB, width)
    var accMask = bvshl(this.mask, thatLB, width)

    // Iterate through each shift value

    val lower = thatLB.toInt
    val upper = Math.min(thatUB, that.width)
    for (i <- lower to upper) {
      // Check if the shift is possible
      if ((i & bvnot(that.mask, that.width)) == that.value) {
        accMask |= bvshl(this.mask, i, width) | (bvshl(this.value, i, width) ^ accValue)
        accValue &= bvshl(this.value, i, width)
      }
    }

    if (thatUB >= this.width) {
      accMask |= accValue
      accValue = 0
    }

    TNum(accValue, accMask, this.width)
  }

  // Logical Shift Right
  def TLSHR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    // Handle logical shift right since >> is arithmetic shift right
    def logicalShiftRight(n: BigInt, shift: Int): BigInt = {
      n / BigInt(2).pow(shift)
    }

    // Lower and upper bounds of shift value
    val thatLB = (that.value & bvnot(that.mask, that.width)).toInt
    val thatUB = (that.value | that.mask).toInt

    // Value and mask accumulator begins with lower bound
    var accValue = logicalShiftRight(this.value, thatLB)
    var accMask = logicalShiftRight(this.mask, thatLB)

    // Iterate through each shift value
    for (i <- thatLB to Math.min(thatUB, this.width - 1)) {
      // Check if the shift is possible
      if ((i & bvnot(that.mask, that.width)) == that.value) {
        accMask |= logicalShiftRight(this.mask, i) | (logicalShiftRight(this.value, i) ^ accValue)
        accValue &= logicalShiftRight(this.value, i)
      }
    }

    if (thatUB >= width) {
      accMask |= accValue
      accValue = 0
    }

    TNum(accValue, accMask, this.width)
  }

  // Arithmetic Shift Right
  def TASHR(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    // Lower and upper bounds of shift value
    val thatLB = (that.value & bvnot(that.mask, that.width)).toInt
    val thatUB = (that.value | that.mask).toInt

    // Value and mask accumulator begins with lower bound
    var accValue = bvashr(this.value, thatLB, width)
    var accMask = bvashr(this.mask, thatLB, width)

    // Iterate through each shift value
    for (i <- thatLB to Math.min(thatUB, this.width - 1)) {
      // Check if the shift is possible
      if ((i & bvnot(that.mask, that.width)) == that.value) {
        accMask |= bvashr(this.mask, i, width) | (bvashr(this.value, i, width) ^ accValue)
        accValue &= bvashr(this.value, i, width)
      }
    }

    assert(thatUB >= 0)
    assert(this.width >= 0)
    if (thatUB >= this.width) {
      accMask |= bvashr(this.mask, this.width, width) | (bvashr(this.value, this.width, width) ^ accValue)
      accValue &= bvashr(this.value, this.width, width)
    }

    TNum(accValue, accMask, this.width)
  }

  // TODO
  // Unsigned Division
  def TUDIV(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & bvnot(this.mask, width)
    val dividendH = this.value | this.mask

    val divisorL = that.value & bvnot(that.mask, width)
    val divisorH = that.value | that.mask

    if (divisorL == 0 || divisorH == 0) {
      return TNum(BigInt(0), BigInt(0), this.width).top()
    }

    val q1 = dividendL / divisorL
    val q2 = dividendL / divisorH
    val q3 = dividendH / divisorL
    val q4 = dividendH / divisorH

    val newValue = q1 & q2 & q3 & q4
    val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

    TNum(newValue, newMask, this.width)
  }

  def bvshl(l: BigInt, r: BigInt, width: Int) = {
    BitVectorEval.smt_bvshl(BitVecLiteral(l, width), BitVecLiteral(r, width)).value
  }
  def bvashr(l: BigInt, r: BigInt, width: Int) = {
    BitVectorEval.smt_bvashr(BitVecLiteral(l, width), BitVecLiteral(r, width)).value
  }

  // Handles sign extension of BigInt binary values to targetBits
  def signExtend(n: BigInt, targetBits: Int): BigInt = {
    val msb = bvshl(BigInt(1), (this.width - 1), this.width)
    val mask = bvshl(BigInt(1), targetBits, width) - 1
    val highOnes = BitVecType(targetBits - this.width).maxValue
    if ((n & msb) != 0) (n | (highOnes << (targetBits - this.width))) & mask else n
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
    val dividendL = this.value & bvnot(this.mask, width)
    val dividendH = this.value | this.mask

    val divisorL = that.value & bvnot(that.mask, width)
    val divisorH = that.value | that.mask

    if (divisorL == 0 || divisorH == 0) {
      return top()
    }

    val maxBitLength =
      this.width // dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

    // Sign extend both dividend and divisor and convert to signed representation before division
    val q1 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
    val q2 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))
    val q3 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
    val q4 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))

    val newValue = q1 & q2 & q3 & q4
    val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

    TNum(newValue, newMask, width)
  }

  // TODO
  // Unsigned Remainder
  def TUREM(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & bvnot(this.mask, width)
    val dividendH = this.value | this.mask

    val divisorL = that.value & bvnot(that.mask, width)
    val divisorH = that.value | that.mask

    if (divisorL == 0 || divisorH == 0) {
      return top()
    }

    val r1 = dividendL % divisorL
    val r2 = dividendL % divisorH
    val r3 = dividendH % divisorL
    val r4 = dividendH % divisorH

    val newValue = r1 & r2 & r3 & r4
    val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

    TNum(newValue, newMask, width)
  }

  // TODO
  // Signed Remainder
  def TSREM(that: TNum): TNum = {
    require(this.width == that.width, s"$this $that bv width")
    val dividendL = this.value & bvnot(this.mask, width)
    val dividendH = this.value | this.mask

    val divisorL = that.value & bvnot(that.mask, width)
    val divisorH = that.value | that.mask

    if (divisorL == 0 || divisorH == 0) {
      return top()
    }

    // Determine maximum bit length for sign extension
    val maxBitLength =
      this.width // dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

    // Sign extend both dividend and divisor and convert to signed representation before division
    val r1 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
    val r2 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))
    val r3 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
    val r4 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))

    val newValue = r1 & r2 & r3 & r4
    val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

    TNum(newValue, newMask, width)
  }

  // Converts a remainder to a modulo result
  def toModulo(remainder: BigInt, dividend: BigInt, divisor: BigInt): BigInt = {
    if (remainder < 0) {
      if (divisor < 0) {
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
    val dividendL = this.value & bvnot(this.mask, width)
    val dividendH = this.value | this.mask

    val divisorL = that.value & bvnot(that.mask, width)
    val divisorH = that.value | that.mask

    if (divisorL == 0 || divisorH == 0) {
      return top()
    }

    // Determine maximum bit length for sign extension
    val maxBitLength =
      this.width // dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

    // Sign extend both dividend and divisor and convert to signed representation
    val r1 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
    val r2 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))
    val r3 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
    val r4 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))

    val modr1 = toModulo(r1, dividendL, divisorL)
    val modr2 = toModulo(r2, dividendL, divisorH)
    val modr3 = toModulo(r3, dividendH, divisorL)
    val modr4 = toModulo(r4, dividendH, divisorH)

    // Combine the adjusted remainders
    val newValue = modr1 & modr2 & modr3 & modr4
    val newMask =
      (modr1 ^ modr2) | (modr1 ^ modr3) | (modr1 ^ modr4) | (modr2 ^ modr3) | (modr2 ^ modr4) | (modr3 ^ modr4)

    TNum(newValue, newMask, this.width)
  }

  // Two's complement negation
  def TNEG(): TNum = {
    TNum(BigInt(0), BigInt(0), width).TSUB(this)
  }

  // Bitwise Not
  def TNOT(): TNum = {
    TNum(bvnot(this.value, this.width) & bvnot(this.mask, this.width), this.mask, this.width)
  }

  // Equality (TNum cannot have Top elements when checking for equality)
  def TEQ(that: TNum): TNum = {
    require(width == that.width)
    if ((this.mask == 0 && that.mask == 0)) then {
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
  def getUnsignedMinValue(): BigInt = {
    this.value & bvnot(this.mask, width)
  }

  // Get largest possible unsigned value of the TNum (e.g. Max value of TT0 is 110)
  def getUnsignedMaxValue(): BigInt = {
    this.value | this.mask
  }

  // Get smallest possible signed value of the TNum
  def getSignedMinValue(): BigInt = {
    toSigned(this.getUnsignedMinValue())
  }

  // Get largest possible signed value of the TNum
  def getSignedMaxValue(): BigInt = {
    toSigned(this.getUnsignedMaxValue())
  }

  // Unsigned Less Than
  def TULT(that: TNum): TNum = {
    val thisUnsignedMaxValue = this.getUnsignedMaxValue()
    val thatUnsignedMinValue = that.getUnsignedMinValue()

    if (thisUnsignedMaxValue < thatUnsignedMinValue) trueBool else unkBool
  }

  // Signed Less Than
  def TSLT(that: TNum): TNum = {
    val thisSignedMaxValue = this.getSignedMaxValue()
    val thatSignedMinValue = that.getSignedMinValue()

    if (thisSignedMaxValue < thatSignedMinValue) trueBool else unkBool
  }

  // Unsigned Less Than or Equal
  def TULE(that: TNum): TNum = {
    val thisUnsignedMaxValue = this.getUnsignedMaxValue()
    val thatUnsignedMinValue = that.getUnsignedMinValue()

    if (thisUnsignedMaxValue <= thatUnsignedMinValue) trueBool else unkBool
  }

  // Signed Less Than or Equal
  def TSLE(that: TNum): TNum = {
    val thisSignedMaxValue = this.getSignedMaxValue()
    val thatSignedMinValue = that.getSignedMinValue()

    if (thisSignedMaxValue <= thatSignedMinValue) trueBool else unkBool
  }

  // Unsigned Greater Than
  def TUGT(that: TNum): TNum = {
    val thisUnsignedMinValue = this.getUnsignedMinValue()
    val thatUnsignedMaxValue = that.getUnsignedMaxValue()

    if (thisUnsignedMinValue > thatUnsignedMaxValue) trueBool else unkBool
  }

  // Signed Greater Than
  def TSGT(that: TNum): TNum = {
    val thisSignedMinValue = this.getSignedMinValue()
    val thatSignedMaxValue = that.getSignedMaxValue()

    if (thisSignedMinValue > thatSignedMaxValue) trueBool else unkBool
  }

  // Unsigned Greater Than or Equal
  def TUGE(that: TNum): TNum = {
    val thisUnsignedMinValue = this.getUnsignedMinValue()
    val thatUnsignedMaxValue = that.getUnsignedMaxValue()

    if (thisUnsignedMinValue >= thatUnsignedMaxValue) trueBool else unkBool
  }

  // Signed Greater Than or Equal
  def TSGE(that: TNum): TNum = {
    val thisSignedMinValue = this.getSignedMinValue()
    val thatSignedMaxValue = that.getSignedMaxValue()

    if (thisSignedMinValue >= thatSignedMaxValue) trueBool else unkBool
  }

  // Concatenation
  def TCONCAT(that: TNum): TNum = {
    val v = (this.value << that.width) | that.value
    val mu = (this.mask <<  that.width) | that.mask
    TNum(v, mu, width + that.width)
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
  def toTNum(literal: BitVecLiteral | IntLiteral): TNum = literal match {
    case bv: BitVecLiteral =>
      val mask = (BigInt(1) << bv.size) - 1
      TNum(bv.value & mask, BigInt(0), bv.size)

    case iv: IntLiteral =>
      val mask = (BigInt(1) << iv.value.bitLength) - 1
      TNum(iv.value & mask, BigInt(0), sizeBits(IntType))
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
      case BVMUL => tn1.top() // tn1.TMUL(tn2) // broken, overflows
      case BVUDIV => tn1.TUDIV(tn2)
      case BVUREM => tn1.TUREM(tn2)
      case BVSHL => tn1.TSHL(tn2)
      case BVLSHR => tn1.TLSHR(tn2)
      case BVULT => tn1.TULT(tn2)
      case BVCOMP => tn1.TCOMP(tn2)
      case BVSUB => tn1.TSUB(tn2)
      case BVSDIV => tn1.top() // tn1.TSDIV(tn2) // broken
      case BVSREM => tn1.top() // tn1.TSREM(tn2) // broken
      case BVSMOD => tn1.top() // tn1.TSMOD(tn2) // broken
      case BVASHR => tn1.TASHR(tn2)
      case BVULE => tn1.TULE(tn2)
      case BVUGT => tn1.TUGT(tn2)
      case BVUGE => tn1.TUGE(tn2)
      case BVSLT => tn1.TSLT(tn2)
      case BVSLE => tn1.TSLE(tn2)
      case BVSGT => tn1.TSGT(tn2)
      case BVSGE => tn1.TSGE(tn2)
      case BVEQ => tn1.TEQ(tn2)
      case BVNEQ => tn1.TNEQ(tn2)
      case BVCONCAT => tn1.TCONCAT(tn2)
      case IntADD => tn1.TADD(tn2)
      case IntMUL => tn1.top() // tn1.TMUL(tn2)
      case IntSUB => tn1.TSUB(tn2)
      case IntDIV => tn1.TSDIV(tn2)
      case IntMOD => tn1.TSMOD(tn2)
      case IntEQ => tn1.TEQ(tn2)
      case IntNEQ => tn1.TNEQ(tn2)
      case IntLT => tn1.TSLT(tn2)
      case IntLE => tn1.TSLE(tn2)
      case IntGT => tn1.TSGT(tn2)
      case IntGE => tn1.TSGE(tn2)
    }
  }

  def evaluateBoolBinOp(op: BoolBinOp, tn1: TNum, tn2: TNum): TNum = {
    op match {
      case BoolEQ => tn1.TEQ(tn2)
      case BoolNEQ => tn1.TNEQ(tn2)
      case BoolAND => tn1.TAND(tn2)
      case BoolOR => tn1.TOR(tn2)
      case BoolIMPLIES => (tn1.TOR(tn2.TNOT()))
      case BoolEQUIV => tn1.TEQ(tn2)
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
  def evaluateExprToTNum(s: Map[Variable, TNum], expr: Expr): TNum = expr match {
    case b: BitVecLiteral => toTNum(b)

    case i: IntLiteral => toTNum(i)

    case TrueLiteral => TNum(1, 0, 1)

    case FalseLiteral => TNum(0, 0, 1)

    case v: Variable => s.getOrElse(v, TNum(BigInt(0), BigInt(0), sizeBits(v.getType)).top())

    case UnaryExpr(op: UnOp, arg: Expr) =>
      val argTNum = evaluateExprToTNum(s, arg)

      (op, argTNum) match {
        case (opVal: BVUnOp, tnum: TNum) => evaluateValueUnOp(opVal, tnum)
        case (opVal: IntUnOp, tnum: TNum) => evaluateValueUnOp(opVal, tnum)
        case (opVal: BoolUnOp, tnum: TNum) => evaluateBoolUnOp(opVal, tnum)
      }

    case BinaryExpr(op: BVBinOp, arg1: Expr, arg2: Expr) =>
      val arg1TNum = evaluateExprToTNum(s, arg1)
      val arg2TNum = evaluateExprToTNum(s, arg2)

      (op, arg1TNum, arg2TNum) match {
        case (opVal: BVBinOp, tnum1: TNum, tnum2: TNum) => evaluateValueBinOp(opVal, tnum1, tnum2)
        case (opVal: IntBinOp, tnum1: TNum, tnum2: TNum) => evaluateValueBinOp(opVal, tnum1, tnum2)
        case (opVal: BoolBinOp, tnum1: TNum, tnum2: TNum) => evaluateBoolBinOp(opVal, tnum1, tnum2)
      }

    case Extract(endIndex: Int, startIndex: Int, body: Expr) =>
      val bodyTNum = evaluateExprToTNum(s, body)

      bodyTNum match {
        case tnum: TNum =>
          val bodyTNumExtract = (tnum.value >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
          val bodyTNumMaskExtract = (tnum.mask >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
          TNum(bodyTNumExtract, bodyTNumMaskExtract, endIndex - startIndex)
      }

    case Repeat(repeats: Int, body: Expr) =>
      val bodyTNum = evaluateExprToTNum(s, body)

      bodyTNum match {
        case tnum: TNum =>
          val repeatedValue = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) =>
            (acc << tnum.width) | tnum.value
          }
          val repeatedMask = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) =>
            (acc << tnum.width) | tnum.mask
          }
          TNum(repeatedValue, repeatedMask, tnum.width)
      }

    case ZeroExtend(extension: Int, body: Expr) =>
      val bodyTNum = evaluateExprToTNum(s, body)

        //val newLength = tnum.width + extension
        //val zeroExtendedValue = tnum.value & ((BigInt(1) << newLength) - 1)
        //val zeroExtendedMask = tnum.mask & ((BigInt(1) << newLength) - 1)
        bodyTNum.copy(width = bodyTNum.width + extension)

    case SignExtend(extension: Int, body: Expr) =>
      val tnum = evaluateExprToTNum(s, body)

      val valueMsb = (tnum.value >> (tnum.width - 1)) & 1
      val maskMsb = (tnum.mask >> (tnum.width - 1)) & 1

      val (extendedValue, extendedMask) = (valueMsb, maskMsb) match {
        case (0, 0) =>
          // If MSB of value is 0 and MSB of mask is 0, extend value and mask with 0
          (tnum.value, tnum.mask)
        case (1, 0) =>
          // If MSB of value is 1 and MSB of mask is 0, extend value with 1 and mask with 0
          val extendedPart = (BigInt(1) << extension) - 1
          (tnum.value | (extendedPart << tnum.width), tnum.mask)
        case (0, 1) =>
          // If MSB of value is 0 and MSB of mask is 1, extend value with 0 and mask with 1
          val extendedPartMask = (BigInt(1) << extension) - 1
          (tnum.value, tnum.mask | (extendedPartMask << tnum.width))
        case (1, 1) =>
          // If MSB 0 in value or mask has been removed due to BigInt type
          // (e.g. value = 001 = BigInt(1) != MSB 0, mask = 100), compare their bit lengths
          // since (value, mask) can never be (1, 1) and have the same length
          if (tnum.width > tnum.width) {
            // If value has more bits, extend value with 1 and mask with 0
            val extendedPartValue = (BigInt(1) << extension) - 1
            (tnum.value | (extendedPartValue << tnum.width), tnum.mask)
          } else { // if (tnum.width < tnum.mask.bitLength) {
            // If value has less bits, extend value with 0 and mask with 1
            val extendedPartMask = (BigInt(1) << extension) - 1
            (tnum.value, tnum.mask | (extendedPartMask << tnum.width))
          }
      }
      TNum(extendedValue, extendedMask, tnum.width + extension)

  }

  // s is the abstract state from previous command/block
  override def transfer(s: Map[Variable, TNum], b: Command): Map[Variable, TNum] = {
    b match {
      // Assign variable to variable (e.g. x = y)
      case LocalAssign(lhs: Variable, rhs: Expr, _) =>
        s.updated(lhs, evaluateExprToTNum(s, rhs))

      // Load from memory and store in variable
      case MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int, _) if !s.contains(lhs) =>
        // Overapproxiate memory values with Top
        s.updated(lhs, TNum(BigInt(0), BigInt(0), size).top())

      // Default case
      case _ => s
    }
  }

  /**
   * Joins the same variables and merges TNum values using bitwise OR
   *
   *   e.g. Join: x = 0011, x = 1111
   *   x = 0011 => value = 0011, mask = 0000
   *   x = 1111 => value = 1111, mask = 0000
   *   Joined x = 1111 => value = 1111, mask = 0000
   */
  override def join(left: Map[Variable, TNum], right: Map[Variable, TNum], pos: Block): Map[Variable, TNum] = {
    (left.keySet ++ right.keySet).map { key =>
      val leftTNum = left.getOrElse(key, TNum(BigInt(0), BigInt(0), sizeBits(key.getType)).top())
      val rightTNum = right.getOrElse(key, TNum(BigInt(0), BigInt(0), sizeBits(key.getType)).top())

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
          case _ => key -> TNum(BigInt(0), BigInt(0), leftTNum.width).top()
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
