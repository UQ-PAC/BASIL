package ir.transforms
import ir.*

// A = definite 1 bits, B = unknown bits
// A_[i] = 1, B_[i] = 0 -> Bit i of TNum _ is definitely 1
// A_[i] = 0, B_[i] = 0 -> Bit i of TNum _ is definitely 0
// A_[i] = 0, B_[i] = 1 -> Bit i of TNum _ is unknown (T)
// where A_[i] = value and B_[i] = mask (value and mask are unsigned BigInt)

// e.g.
// x = {1, T, 0, T}
// Ax = 1000, Bx = 0101

// z = {T, 1, 1, 0}
// Az = 0110, Bz = 1000

// Bitwise AND (y = x & z):
// Ay[i] = Ax[i] & Az[i]    ->    Ay[i] = 0000
// By[i] = (Ax[i] | Bx[i]) & (Az[i] | Bz[i]) & ~(Ax[i] & Az[i])   ->   By[i] = 1100
// y = {T, T, 0, 0}

// Bitwise OR:
// Ay[i] = Ax[i] | Az[i]    ->    Ay[i] = 1110
// By[i] = (Bx[i] | Bz[i]) & ~(Ax[i] | Az[i])   ->   By[i] = 0001
// y = {1, 1, 1, T}

// Bitwise XOR:
// Ay[i] = (Ax[i] ^ Az[i]) & ~(Bx[i] | Bz[i])   ->    Ay[i] = 0010
// By[i] = (Bx[i] | Bz[i])    ->    By[i] = 1101
// y = {T, T, 1, T}

sealed trait TNum

case class TNumBool(boolean: Int) extends TNum

case class TNumValue(value: BigInt, mask: BigInt) extends TNum {
    // Bitwise AND
    def TAND(that: TNumValue): TNumValue = {
        val alpha = this.value | this.mask
        val beta = that.value | that.mask
        val v = this.value & that.value
        TNumValue(v, (alpha & beta & ~v))
    }

    // Bitwise OR
    def TOR(that: TNumValue): TNumValue = {
        val v = this.value | that.value
        val mu = this.mask | that.mask
        TNumValue(v, (mu & ~v));
    }

    // Bitwise XOR
    def TXOR(that: TNumValue): TNumValue = {
        val v = this.value ^ that.value
        val mu = this.mask | that.mask
        TNumValue((v & ~mu), mu)
    }

    // Bitwise NOR
    def TNOR(that: TNumValue): TNumValue = {
        val v = this.value | that.value
        val mu = this.mask | that.mask
        TNumValue((~v & ~mu), (~v & mu))
    }

    // Bitwise XNOR
    def TXNOR(that: TNumValue): TNumValue = {
        val v = ~(this.value ^ that.value)
        val mu = this.mask | that.mask
        TNumValue((v & ~mu), mu)
    }

    // Bitwise NAND
    def TNAND(that: TNumValue): TNumValue = {
        val alpha = this.value | this.mask
        val beta = that.value | that.mask
        val v = this.value & that.value
        TNumValue((~v & ~(alpha & beta & ~v)), (alpha & beta & ~v))
    }

    // Addition
    def TADD(that: TNumValue): TNumValue = {
        val sm = this.mask + that.mask
        val sv = this.value + that.value
        val sigma = sm + sv
        val chi = sigma ^ sv
        val mu = chi | this.mask | that.mask
        TNumValue(sv & ~mu, mu)
    }

    // Multiplication
    def TMUL(that: TNumValue): TNumValue = {
        var a = this
        var b = that
        val v = this.value * that.value
        var mu = TNumValue(BigInt(0), BigInt(0))

        while ((a.value | a.mask) != 0) {
            if ((a.value & BigInt(1)) != 0) {
                mu = mu.TADD(TNumValue(BigInt(0), b.mask))
            } else if ((a.mask & BigInt(1)) != 0) {
                mu = mu.TADD(TNumValue(BigInt(0), b.value | b.mask))
            }
            a = a.TLSHR(TNumValue(BigInt(1), BigInt(0)))
            b = b.TSHL(TNumValue(BigInt(1), BigInt(0)))
        }
        TNumValue(v, BigInt(0)).TADD(mu)
    }

    // Subtraction
    def TSUB(that: TNumValue): TNumValue = {
        val dv = this.value - that.value
        val alpha = dv + this.mask
        val beta = dv - that.mask
        val chi = alpha ^ beta
        val mu = chi | this.mask | that.mask
        TNumValue((dv & ~mu), mu)
    }

    // Bitwise Comparison
    def TCOMP(that: TNumValue): TNumValue = {
        if ((this.mask == 0 && that.mask == 0) && (this.value == that.value)) {
            TNumValue(BigInt(1), BigInt(0))
        } else {
            TNumValue(BigInt(0), BigInt(0))
        }
    }

    // Shift Left
    def TSHL(that: TNumValue): TNumValue = {
        // Lower and upper bounds of shift value
        val thatLB = (that.value & ~that.mask).toInt
        val thatUB = (that.value | that.mask).toInt

        val bitWidth = (this.value | this.mask).bitLength

        // Value and mask accumulator begins with lower bound
        var accValue = this.value << thatLB
        var accMask = this.mask << thatLB

        // Iterate through each shift value
        for (i <- thatLB to Math.min(thatUB, bitWidth - 1)) {
            // Check if the shift is possible
            if ((i & ~that.mask) == that.value) {
                accMask |= (this.mask << i) | ((this.value << i) ^ accValue)
                accValue &= (this.value << i)
            }
        }

        if (thatUB >= bitWidth) {
            accMask |= accValue
            accValue = 0
        }

        TNumValue(accValue, accMask)
    }

    // Logical Shift Right
    def TLSHR(that: TNumValue): TNumValue = {
        // Handle logical shift right since >> is arithmetic shift right
        def logicalShiftRight(n: BigInt, shift: Int): BigInt = {
            return (n >> shift) & ~(BigInt(-1) << (n.bitLength - shift))
        }

        // Lower and upper bounds of shift value
        val thatLB = (that.value & ~that.mask).toInt
        val thatUB = (that.value | that.mask).toInt

        val bitWidth = (this.value | this.mask).bitLength

        // Value and mask accumulator begins with lower bound
        var accValue = logicalShiftRight(this.value, thatLB)
        var accMask = logicalShiftRight(this.mask, thatLB)

        // Iterate through each shift value
        for (i <- thatLB to Math.min(thatUB, bitWidth - 1)) {
            // Check if the shift is possible
            if ((i & ~that.mask) == that.value) {
                accMask |= logicalShiftRight(this.mask, i) | (logicalShiftRight(this.value, i) ^ accValue)
                accValue &= logicalShiftRight(this.value, i)
            }
        }

        if (thatUB >= bitWidth) {
            accMask |= accValue
            accValue = 0
        }

        TNumValue(accValue, accMask)
    }

    // Arithmetic Shift Right
    def TASHR(that: TNumValue): TNumValue = {
        // Lower and upper bounds of shift value
        val thatLB = (that.value & ~that.mask).toInt
        val thatUB = (that.value | that.mask).toInt

        val bitWidth = (this.value | this.mask).bitLength

        // Value and mask accumulator begins with lower bound
        var accValue = this.value >> thatLB
        var accMask = this.mask >> thatLB

        // Iterate through each shift value
        for (i <- thatLB to Math.min(thatUB, bitWidth - 1)) {
            // Check if the shift is possible
            if ((i & ~that.mask) == that.value) {
                accMask |= (this.mask >> i) | ((this.value >> i) ^ accValue)
                accValue &= (this.value >> i)
            }
        }

        if (thatUB >= bitWidth) {
            accMask |= (this.mask >> bitWidth) | ((this.value >> bitWidth) ^ accValue)
            accValue &= (this.value >> bitWidth)
        }

        TNumValue(accValue, accMask)
    }

    // TODO
    // Unsigned Division
    def TUDIV(that: TNumValue): TNumValue = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNumValue(BigInt(0), BigInt(-1))
        }

        val q1 = dividendL / divisorL
        val q2 = dividendL / divisorH
        val q3 = dividendH / divisorL
        val q4 = dividendH / divisorH

        val newValue = q1 & q2 & q3 & q4
        val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

        TNumValue(newValue, newMask)
    }

    // Handles sign extension of BigInt binary values to targetBits
    def signExtend(n: BigInt, targetBits: Int): BigInt = {
        val msb = BigInt(1) << (n.bitLength - 1)
        val mask = (BigInt(1) << targetBits) - 1
        if ((n & msb) != 0) (n | (BigInt(-1) << (targetBits - n.bitLength))) & mask else n
    }

    // Converts a BigInt to signed representation
    def toSigned(n: BigInt): BigInt = {
        val msb = BigInt(1) << (n.bitLength - 1)
        if ((n & msb) != 0) n - (msb << 1) else n
    }

    // TODO
    // Signed Division
    def TSDIV(that: TNumValue): TNumValue = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNumValue(BigInt(0), BigInt(-1))
        }

        val maxBitLength = dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

        // Sign extend both dividend and divisor and convert to signed representation before division
        val q1 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
        val q2 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))
        val q3 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
        val q4 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))

        val newValue = q1 & q2 & q3 & q4
        val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

        TNumValue(newValue, newMask)
    }

    // TODO
    // Unsigned Remainder
    def TUREM(that: TNumValue): TNumValue = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNumValue(BigInt(0), BigInt(-1))
        }

        val r1 = dividendL % divisorL
        val r2 = dividendL % divisorH
        val r3 = dividendH % divisorL
        val r4 = dividendH % divisorH

        val newValue = r1 & r2 & r3 & r4
        val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

        TNumValue(newValue, newMask)
    }

    // TODO
    // Signed Remainder
    def TSREM(that: TNumValue): TNumValue = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNumValue(BigInt(0), BigInt(-1))
        }

        // Determine maximum bit length for sign extension
        val maxBitLength = dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

        // Sign extend both dividend and divisor and convert to signed representation before division
        val r1 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
        val r2 = toSigned(signExtend(dividendL, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))
        val r3 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorL, maxBitLength))
        val r4 = toSigned(signExtend(dividendH, maxBitLength)) % toSigned(signExtend(divisorH, maxBitLength))

        val newValue = r1 & r2 & r3 & r4
        val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

        TNumValue(newValue, newMask)       
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
    def TSMOD(that: TNumValue): TNumValue = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNumValue(BigInt(0), BigInt(-1))
        }

        // Determine maximum bit length for sign extension
        val maxBitLength = dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

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
        val newMask = (modr1 ^ modr2) | (modr1 ^ modr3) | (modr1 ^ modr4) | (modr2 ^ modr3) | (modr2 ^ modr4) | (modr3 ^ modr4)

        TNumValue(newValue, newMask)
    }

    // Two's complement negation
    def TNEG(): TNumValue = {
        TNumValue(-this.value & ~this.mask, this.mask)
    }

    // Bitwise Not
    def TNOT(): TNumValue = {
        TNumValue(~this.value & ~this.mask, this.mask)
    }

    // Equality (TNum cannot have Top elements when checking for equality)
    def TEQ(that: TNumValue): TNumBool = {
        if ((this.mask == 0 && that.mask == 0) && (this.value == that.value)) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Not Equal (If a Top element exists, we assume not equal since Top could be 1 or 0)
    def TNEQ(that: TNumValue): TNumBool = {
        if ((this.mask != 0 || that.mask != 0) || (this.value != that.value)) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Get smallest possible unsigned value of the TNum (e.g. Min value of TT0 is 000)
    def getUnsignedMinValue(): BigInt = {
        this.value & ~this.mask
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
    def TULT(that: TNumValue): TNumBool = {
        val thisUnsignedMaxValue = this.getUnsignedMaxValue()
        val thatUnsignedMinValue = that.getUnsignedMinValue()

        if (thisUnsignedMaxValue < thatUnsignedMinValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Signed Less Than
    def TSLT(that: TNumValue): TNumBool = {
        val thisSignedMaxValue = this.getSignedMaxValue()
        val thatSignedMinValue = that.getSignedMinValue()

        if (thisSignedMaxValue < thatSignedMinValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Unsigned Less Than or Equal
    def TULE(that: TNumValue): TNumBool = {
        val thisUnsignedMaxValue = this.getUnsignedMaxValue()
        val thatUnsignedMinValue = that.getUnsignedMinValue()

        if (thisUnsignedMaxValue <= thatUnsignedMinValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Signed Less Than or Equal
    def TSLE(that: TNumValue): TNumBool = {
        val thisSignedMaxValue = this.getSignedMaxValue()
        val thatSignedMinValue = that.getSignedMinValue()
        
        if (thisSignedMaxValue <= thatSignedMinValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }  

    // Unsigned Greater Than
    def TUGT(that: TNumValue): TNumBool = {
        val thisUnsignedMinValue = this.getUnsignedMinValue()
        val thatUnsignedMaxValue = that.getUnsignedMaxValue()

        if (thisUnsignedMinValue > thatUnsignedMaxValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Signed Greater Than
    def TSGT(that: TNumValue): TNumBool = {
        val thisSignedMinValue = this.getSignedMinValue()
        val thatSignedMaxValue = that.getSignedMaxValue()

        if (thisSignedMinValue > thatSignedMaxValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Unsigned Greater Than or Equal
    def TUGE(that: TNumValue): TNumBool = {
        val thisUnsignedMinValue = this.getUnsignedMinValue()
        val thatUnsignedMaxValue = that.getUnsignedMaxValue()
        
        if (thisUnsignedMinValue >= thatUnsignedMaxValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Signed Greater Than or Equal
    def TSGE(that: TNumValue): TNumBool = {
        val thisSignedMinValue = this.getSignedMinValue()
        val thatSignedMaxValue = that.getSignedMaxValue()
        
        if (thisSignedMinValue >= thatSignedMaxValue) {
            TNumBool(1)
        } else {
            TNumBool(0)
        }
    }

    // Concatenation
    def TCONCAT(that: TNumValue): TNumValue = {
        val v = (this.value << that.value.bitLength) | that.value
        val mu = (this.mask << that.value.bitLength) | that.mask
        TNumValue(v, mu)
    }
}

class TNumDomain extends AbstractDomain[Map[Variable, TNum]] {
    override def top: Map[Variable, TNum] = Map.empty
    override def bot: Map[Variable, TNum] = Map.empty

    // Converts a bitvector or integer literal to a TNum
    def toTNum(literal: BitVecLiteral | IntLiteral): TNumValue = literal match {
        case bv: BitVecLiteral =>
            val mask = (BigInt(1) << bv.size) - 1
            TNumValue(bv.value & mask, BigInt(0))

        case iv: IntLiteral =>
            val mask = (BigInt(1) << iv.value.bitLength) - 1
            TNumValue(iv.value & mask, BigInt(0))
    }

    // Evaluates binary operation and returns either a TNumValue or TNumBool
    def evaluateBinOp(op: BVBinOp, tn1: TNumValue, tn2: TNumValue): TNum = {
        op match {
            case BVAND => tn1.TAND(tn2)
            case BVOR => tn1.TOR(tn2)
            case BVXOR => tn1.TXOR(tn2)
            case BVNOR => tn1.TNOR(tn2)
            case BVXNOR => tn1.TXNOR(tn2)
            case BVNAND => tn1.TNAND(tn2)
            case BVADD => tn1.TADD(tn2)
            case BVMUL => tn1.TMUL(tn2)
            case BVUDIV => tn1.TUDIV(tn2)
            case BVUREM => tn1.TUREM(tn2)
            case BVSHL => tn1.TSHL(tn2)
            case BVLSHR => tn1.TLSHR(tn2)
            case BVULT => tn1.TULT(tn2)
            case BVCOMP => tn1.TCOMP(tn2)
            case BVSUB => tn1.TSUB(tn2)
            case BVSDIV => tn1.TSDIV(tn2)
            case BVSREM => tn1.TSREM(tn2)
            case BVSMOD => tn1.TSMOD(tn2)
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
            case _ => TNumValue(BigInt(0), BigInt(-1))
        }
    }

    // Evaluates unary operations
    def evaluateUnOp(op: BVUnOp, tn: TNumValue): TNumValue = {
        op match {
            case BVNOT => tn.TNOT()
            case BVNEG => tn.TNEG()
            case _ => TNumValue(BigInt(0), BigInt(-1))
        }
    }

    // Recursively evaluates nested or non-nested expression
    def evaluateExprToTNum(s: Map[Variable, TNum], expr: Expr): TNum = expr match {
        case b: BitVecLiteral => toTNum(b)

        case i: IntLiteral => toTNum(i)

        case v: Variable => s.getOrElse(v, TNumValue(BigInt(0), BigInt(-1)))

        case BinaryExpr(op: BVBinOp, arg1: Expr, arg2: Expr) => 
            val arg1TNum = evaluateExprToTNum(s, arg1)
            val arg2TNum = evaluateExprToTNum(s, arg2)
            evaluateBinOp(op, arg1TNum, arg2TNum)

        case Extract(endIndex: Int, startIndex: Int, body: Expr) => 
            val bodyTNum = evaluateExprToTNum(s, body)
            val bodyTNumValueExtract = (bodyTNum.value >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
            val bodyTNumMaskExtract = (bodyTNum.mask >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
            TNumValue(bodyTNumValueExtract, bodyTNumMaskExtract)

        case Repeat(repeats: Int, body: Expr) =>
            val bodyTNum = evaluateExprToTNum(s, body)
            val repeatedValue = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) => (acc << bodyTNum.value.bitLength) | bodyTNum.value }
            val repeatedMask = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) => (acc << bodyTNum.mask.bitLength) | bodyTNum.mask }
            TNumValue(repeatedValue, repeatedMask)

        case ZeroExtend(extension: Int, body: Expr) =>
            val bodyTNum = evaluateExprToTNum(s, body)
            val newLength = bodyTNum.value.bitLength + extension
            val zeroExtendedValue = bodyTNum.value & ((BigInt(1) << newLength) - 1)
            val zeroExtendedMask = bodyTNum.mask & ((BigInt(1) << newLength) - 1)
            TNumValue(zeroExtendedValue, zeroExtendedMask)

        case SignExtend(extension: Int, body: Expr) =>
            val bodyTNum = evaluateExprToTNum(s, body)
            val valueMsb = (bodyTNum.value >> (bodyTNum.value.bitLength - 1)) & 1
            val maskMsb = (bodyTNum.mask >> (bodyTNum.mask.bitLength - 1)) & 1

            val (extendedValue, extendedMask) = (valueMsb, maskMsb) match {
                case (0, 0) =>
                    // If MSB of value is 0 and MSB of mask is 0, extend value and mask with 0
                    (bodyTNum.value, bodyTNum.mask)
                case (1, 0) =>
                    // If MSB of value is 1 and MSB of mask is 0, extend value with 1 and mask with 0
                    val extendedPart = (BigInt(1) << extension) - 1
                    (bodyTNum.value | (extendedPart << bodyTNum.value.bitLength), bodyTNum.mask)
                case (0, 1) =>
                    // If MSB of value is 0 and MSB of mask is 1, extend value with 0 and mask with 1
                    val extendedPartMask = (BigInt(1) << extension) - 1
                    (bodyTNum.value, bodyTNum.mask | (extendedPartMask << bodyTNum.mask.bitLength))
                case (1, 1) =>
                    // If MSB 0 in value or mask has been removed due to BigInt type 
                    // (e.g. value = 001 = BigInt(1) != MSB 0, mask = 100), compare their bit lengths 
                    // since (value, mask) can never be (1, 1) and have the same length
                    if (bodyTNum.value.bitLength > bodyTNum.mask.bitLength) {
                        // If value has more bits, extend value with 1 and mask with 0
                        val extendedPartValue = (BigInt(1) << extension) - 1
                        (bodyTNum.value | (extendedPartValue << bodyTNum.value.bitLength), bodyTNum.mask)
                    } else if (bodyTNum.value.bitLength < bodyTNum.mask.bitLength) {
                        // If value has less bits, extend value with 0 and mask with 1
                        val extendedPartMask = (BigInt(1) << extension) - 1
                        (bodyTNum.value, bodyTNum.mask | (extendedPartMask << bodyTNum.mask.bitLength))
                    }
            }
            TNumValue(extendedValue, extendedMask)
            
        case UnaryExpr(op: BVUnOp, arg: Expr) =>
            val argTNum = evaluateExprToTNum(s, arg)
            evaluateUnOp(op, argTNum)

        case _ => TNumValue(BigInt(0), BigInt(-1))
    }

    // s is the abstract state from previous command/block
    override def transfer(s: Map[Variable, TNum], b: Command): Map[Variable, TNum] = {
        b match {
            // Assign variable to variable (e.g. x = y)
            case LocalAssign(lhs: Variable, rhs: Expr) =>
                s.updated(lhs, evaluateExprToTNum(rhs))

            // Load from memory and store in variable
            case MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int) 
                if !s.contains(lhs) =>
                    // Overapproxiate memory values with Top
                    s.updated(lhs, TNum(BigInt(0), BigInt(-1)))

            // Default case    
            case _ => s 
        }
    }

    // Joins the same variables and merges TNum values using bitwise OR
    // e.g. Join: x = 0011, x = 1111
    // x = 0011 => value = 0011, mask = 0000
    // x = 1111 => value = 1111, mask = 0000
    // Joined x = 1111 => value = 1111, mask = 0000
    override def join(left: Map[Variable, TNum], right: Map[Variable, TNum], pos: Block): Map[Variable, TNum] = {
        (left.keySet ++ right.keySet).map { key =>
            val leftTNum = left.getOrElse(key, TNumValue(BigInt(0), BigInt(-1)))
            val rightTNum = right.getOrElse(key, TNumValue(BigInt(0), BigInt(-1)))

            // Only merge the TNum of variables that appear in both program states
            if (left.contains(key) && !right.contains(key)) {
                key -> leftTNum
            } else if (!left.contains(key) && right.contains(key)) {
                key -> rightTNum
            } else {
                key -> leftTNum.TOR(rightTNum)
            }
        }.toMap
    }
}

// TODO
// call applyTransform method in RunUtils
// transforms.SimplifyExprWithTNum().applyTransform(ctx.program)
class SimplifyKnownBits() {
    val solver = transforms.worklistSolver(TNumDomain())

    def applyTransform(p: Program): Unit = {
        for (proc <- p.procedures) {
            applyTransform(proc)
        }
    }

    def applyTransform(procedure: Procedure): Unit = {
        val (beforeIn, afterIn) = solver.solveProc(procedure, backwards = false)
        writeToFile(translating.PrettyPrinter.pp_proc_with_analysis_results(beforeIn, afterIn, procedure, x => s"Live vars: ${x.map(_.name).toList.sorted.mkString(", ")}"), "live-vars.il")
    }
}