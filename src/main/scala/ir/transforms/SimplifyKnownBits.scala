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

case class TNum(value: BigInt, mask: BigInt) {
    // Bitwise AND
    def TAND(that: TNum): TNum = {
        val alpha = this.value | this.mask
        val beta = that.value | that.mask
        val v = this.value & that.value
        TNum(v, (alpha & beta & ~v))
    }

    // Bitwise OR
    def TOR(that: TNum): TNum = {
        val v = this.value | that.value
        val mu = this.mask | that.mask
        TNum(v, (mu & ~v));
    }

    // Bitwise XOR
    def TXOR(that: TNum): TNum = {
        val v = this.value ^ that.value
        val mu = this.mask | that.mask
        TNum((v & ~mu), mu)
    }

    // Bitwise NOR
    def TNOR(that: TNum): TNum = {
        val v = this.value | that.value
        val mu = this.mask | that.mask
        TNum((~v & ~mu), (~v & mu))
    }

    // Bitwise XNOR
    def TXNOR(that: TNum): TNum = {
        val v = ~(this.value ^ that.value)
        val mu = this.mask | that.mask
        TNum((v & ~mu), mu)
    }

    // Bitwise NAND
    def TNAND(that: TNum): TNum = {
        val alpha = this.value | this.mask
        val beta = that.value | that.mask
        val v = this.value & that.value
        TNum((~v & ~(alpha & beta & ~v)), (alpha & beta & ~v))
    }

    // Addition
    def TADD(that: TNum): TNum = {
        val sm = this.mask + that.mask
        val sv = this.value + that.value
        val sigma = sm + sv
        val chi = sigma ^ sv
        val mu = chi | this.mask | that.mask
        TNum(sv & ~mu, mu)
    }

    // Multiplication
    def TMUL(that: TNum): TNum = {
        var a = this
        var b = that
        val v = this.value * that.value
        var mu = TNum(BigInt(0), BigInt(0))

        while (a.value | a.mask) {
            if (a.value & BigInt(1)) {
                mu = mu.TADD(TNum(BigInt(0), b.mask))
            } else if (a.mask & BigInt(1)) {
                mu = mu.TADD(TNum(BigInt(0), b.value | b.mask))
            }
            a = a.TLSHR(1)
            b = b.TSHL(1)
        }
        TNum(v, BigInt(0)).TADD(mu)
    }

    // Subtraction
    def TSUB(that: TNum): TNum = {
        val dv = this.value - that.value
        val alpha = dv + this.mask
        val beta = dv - that.mask
        val chi = alpha ^ beta
        val mu = chi | this.mask | that.mask
        TNum((dv & ~mu), mu)
    }

    // Bitwise Comparison (Check if values and masks are equal unlike 
    // equality where Top elements cannot exist)
    def TCOMP(that: TNum): Boolean = {
        (this.value == that.value) && (this.mask == that.mask) 
    }

    // Shift Left
    def TSHL(that: TNum): TNum = {
        val shiftL = that.value & ~that.mask
        val shiftH = that.value | that.mask

        val vL = this.value << shiftL
        val vH = this.value << shiftH

        val newValue = vL & vH
        val newMask = (this.mask << shiftL) | (this.mask << shiftH) | (vL ^ vH)

        TNum(newValue, newMask)
    }

    // Logical Shift Right
    def TLSHR(that: TNum): TNum = {
        val shiftL = that.value & ~that.mask
        val shiftH = that.value | that.mask

        // '>>' used for arithmetic shift, not logical
        val vL = this.value.shiftRight(shiftL)
        val vH = this.value.shiftRight(shiftH)

        val newValue = vL & vH
        val newMask = (this.mask >> shiftL) | (this.mask >> shiftH) | (vL ^ vH)

        TNum(newValue, newMask)
    }

    // Arithmetic Shift Right
    def TASHR(that: TNum): TNum = {
        val shiftL = that.value & ~that.mask
        val shiftH = that.value | that.mask

        val vL = this.value >> shiftL
        val vH = this.value >> shiftH

        val newValue = vL & vH
        val newMask = (this.mask >> shiftL) | (this.mask >> shiftH) | (vL ^ vH)

        TNum(newValue, newMask)
    }

    // Unsigned Division
    def TUDIV(that: TNum): TNum = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNum(BigInt(0), BigInt(-1))
        }

        val q1 = dividendL / divisorL
        val q2 = dividendL / divisorH
        val q3 = dividendH / divisorL
        val q4 = dividendH / divisorH

        val newValue = q1 & q2 & q3 & q4
        val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

        TNum(newValue, newMask)
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

    // Signed Division
    def TSDIV(that: TNum): TNum = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNum(BigInt(0), BigInt(-1))
        }

        val maxBitLength = dividendL.bitLength max dividendH.bitLength max divisorL.bitLength max divisorH.bitLength

        // Sign extend both dividend and divisor and convert to signed representation before division
        val q1 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
        val q2 = toSigned(signExtend(dividendL, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))
        val q3 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorL, maxBitLength))
        val q4 = toSigned(signExtend(dividendH, maxBitLength)) / toSigned(signExtend(divisorH, maxBitLength))

        val newValue = q1 & q2 & q3 & q4
        val newMask = (q1 ^ q2) | (q1 ^ q3) | (q1 ^ q4) | (q2 ^ q3) | (q2 ^ q4) | (q3 ^ q4)

        TNum(newValue, newMask)
    }

    // Unsigned Remainder
    def TUREM(that: TNum): TNum = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNum(BigInt(0), BigInt(-1))
        }

        val r1 = dividendL % divisorL
        val r2 = dividendL % divisorH
        val r3 = dividendH % divisorL
        val r4 = dividendH % divisorH

        val newValue = r1 & r2 & r3 & r4
        val newMask = (r1 ^ r2) | (r1 ^ r3) | (r1 ^ r4) | (r2 ^ r3) | (r2 ^ r4) | (r3 ^ r4)

        TNum(newValue, newMask)
    }

    // Signed Remainder
    def TSREM(that: TNum): TNum = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNum(BigInt(0), BigInt(-1))
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

        TNum(newValue, newMask)       
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

    // Signed Modulo
    def TSMOD(that: TNum): TNum = {
        val dividendL = this.value & ~this.mask
        val dividendH = this.value | this.mask

        val divisorL = that.value & ~that.mask
        val divisorH = that.value | that.mask

        if (divisorL == 0 || divisorH == 0) {
            return TNum(BigInt(0), BigInt(-1))
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

        TNum(newValue, newMask)
    }

    // Two's complement negation
    def TNEG(): TNum = {
        TNum(-this.value & ~this.mask, this.mask)
    }

    // Bitwise Not
    def TNOT(): TNum = {
        TNum(~this.value & ~this.mask, this.mask)
    }

    // Equality (TNum cannot have Top elements when checking for equality)
    def TEQ(that: TNum): Boolean = {
        (this.mask == 0 && that.mask == 0) && (this.value == that.value)
    }

    // Not Equal (If a Top element exists, we assume not equal since Top could be 1 or 0)
    def TNEQ(that: TNum): Boolean = {
        (this.mask != 0 || that.mask != 0) || (this.value != that.value)
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
    def TULT(that: TNum): Boolean = {
        val thisUnsignedMaxValue = this.getUnsignedMaxValue()
        val thatUnsignedMinValue = that.getUnsignedMinValue()
        thisUnsignedMaxValue < thatUnsignedMinValue
    }

    // Signed Less Than
    def TSLT(that: TNum): Boolean = {
        val thisSignedMaxValue = this.getSignedMaxValue()
        val thatSignedMinValue = that.getSignedMinValue()
        thisSignedMaxValue < thatSignedMinValue
    }

    // Unsigned Less Than or Equal
    def TULE(that: TNum): Boolean = {
        val thisUnsignedMaxValue = this.getUnsignedMaxValue()
        val thatUnsignedMinValue = that.getUnsignedMinValue()
        thisUnsignedMaxValue <= thatUnsignedMinValue
    }

    // Signed Less Than or Equal
    def TSLE(that: TNum): Boolean = {
        val thisSignedMaxValue = this.getSignedMaxValue()
        val thatSignedMinValue = that.getSignedMinValue()
        thisSignedMaxValue <= thatSignedMinValue
    }  

    // Unsigned Greater Than
    def TUGT(that: TNum): Boolean = {
        val thisUnsignedMinValue = this.getUnsignedMinValue()
        val thatUnsignedMaxValue = that.getUnsignedMaxValue()
        thisUnsignedMinValue > thatUnsignedMaxValue
    }

    // Signed Greater Than
    def TSGT(that: TNum): Boolean = {
        val thisSignedMinValue = this.getSignedMinValue()
        val thatSignedMaxValue = that.getSignedMaxValue()
        thisSignedMinValue > thatSignedMaxValue
    }

    // Unsigned Greater Than or Equal
    def TUGE(that: TNum): Boolean = {
        val thisUnsignedMinValue = this.getUnsignedMinValue()
        val thatUnsignedMaxValue = that.getUnsignedMaxValue()
        thisUnsignedMinValue >= thatUnsignedMaxValue
    }

    // Signed Greater Than or Equal
    def TSGE(that: TNum): Boolean = {
        val thisSignedMinValue = this.getSignedMinValue()
        val thatSignedMaxValue = that.getSignedMaxValue()
        thisSignedMinValue >= thatSignedMaxValue
    }

    // Concatenation
    def TCONCAT(that: TNum): TNum = {
        val v = (this.value << that.value.bitLength) | that.value
        val mu = (this.mask << that.value.bitLength) | that.mask
        TNum(v, mu)
    }
}

class TNumDomain extends AbstractDomain[Map[Variable, TNum]] {
    override def top: Map[Variable, TNum] = Map.empty
    override def bot: Map[Variable, TNum] = Map.empty

    // Converts a bitvector or integer literal to a TNum
    def toTNum(literal: BitVecLiteral | IntLiteral): TNum = literal match {
        case bv: BitVecLiteral =>
            val mask = (BigInt(1) << bv.size) - 1
            TNum(bv.value & mask, BigInt(0))

        case iv: IntLiteral =>
            val mask = (BigInt(1) << iv.size) - 1
            TNum(iv.value & mask, BigInt(0))
    }

    // Evaluates binary operation
    def evaluateBinOp(op: BVBinOp, tn1: TNum, tn2: TNum): TNum = {
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
            case _ => TNum(BigInt(0), BigInt(-1))
        }
    }

    // Evaluates unary operations
    def evaluateUnOp(op: BVUnOp, tn: TNum): TNum = {
        op match {
            case BVNOT => tn.TNOT()
            case BVNEG => tn.TNEG()
            case _ => TNum(BigInt(0), BigInt(-1))
        }
    }

    // Recursively evaluates nested or non-nested expression
    def evaluateExprToTNum(s: Map[Variable, TNum], expr: Expr): TNum = expr match {
        case b: BitVecLiteral => toTNum(b)

        case i: IntLiteral => toTNum(i)

        case v: Variable => s.getOrElse(v, TNum(BigInt(0), BigInt(-1)))

        case BinaryExpr(op: BVBinOp, arg1: Expr, arg2: Expr) => 
            val arg1TNum = evaluateExprToTNum(arg1)
            val arg2TNum = evaluateExprToTNum(arg2)
            evaluateBinOp(op, arg1TNum, arg2TNum)

        case Extract(endIndex: Int, startIndex: Int, body: Expr) => 
            val bodyTNum = evaluateExprToTNum(body)
            val bodyTNumValueExtract = (bodyTNum.value >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
            val bodyTNumMaskExtract = (bodyTNum.mask >> startIndex) & ((BigInt(1) << (endIndex - startIndex)) - 1)
            TNum(bodyTNumValueExtract, bodyTNumMaskExtract)

        case Repeat(repeats: Int, body: Expr) =>
            val bodyTNum = evaluateExprToTNum(s, body)
            val repeatedValue = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) => (acc << bodyTNum.value.bitLength) | bodyTNum.value }
            val repeatedMask = (0 until repeats).foldLeft(BigInt(0)) { (acc, _) => (acc << bodyTNum.mask.bitLength) | bodyTNum.mask }
            TNum(repeatedValue, repeatedMask)

        case ZeroExtend(extension: Int, body: Expr) =>
            val bodyTNum = evaluateExprToTNum(s, body)
            val newLength = bodyTNum.value.bitLength + extension
            val zeroExtendedValue = bodyTNum.value & ((BigInt(1) << newLength) - 1)
            val zeroExtendedMask = bodyTNum.mask & ((BigInt(1) << newLength) - 1)
            TNum(zeroExtendedValue, zeroExtendedMask)

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
            TNum(extendedValue, extendedMask)
            
        case UnaryExpr(op: BVUnOp, arg: Expr) =>
            val argTNum = evaluateExprToTNum(s, arg)
            evaluateUnOp(op, argTNum)

        case _ => TNum(BigInt(0), BigInt(-1))
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

    // Joins the same variables and merges shared TNum values, otherwise replaces with Top
    // e.g. Join: x = 0011, x = 1111
    // x = 0011 => value = 0011, mask = 0000
    // x = 1111 => value = 1111, mask = 0000
    // Joined x = TT11 => value = 0011, mask = 1100
    override def join(left: Map[Variable, TNum], right: Map[Variable, TNum], pos: Block): Map[Variable, TNum] = {
        (left.keySet ++ right.keySet).map { key =>
            val leftTNum = left.getOrElse(key, TNum(BigInt(0), BigInt(-1)))
            val rightTNum = right.getOrElse(key, TNum(BigInt(0), BigInt(-1)))

            // Only merge the TNum of variables that appear in both program states
            if (left.contains(key) && !right.contains(key)) {
                key -> leftTNum
            } else if (!left.contains(key) && right.contains(key)) {
                key -> rightTNum
            } else {
                // Change to bitwise OR
                val newValue = (leftTNum.value & ~leftTNum.mask) & (rightTNum.value & ~rightTNum.mask)
                val newMask = leftTNum.mask | rightTNum.mask | (leftTNum.value ^ rightTNum.value)
                
                key -> TNum(newValue, newMask)
            }
        }.toMap
    }
}

// call applyTransform method in RunUtils
// transforms.SimplifyExprWithTNum().applyTransform(ctx.program)
class SimplifyKnownBits() {
    val solver = transforms.worklistSolver(TNumDomain())

    def applyTransform(p: Program): Unit = {
        for (proc <- p.procedures) {
            applyTransform(proc)
        }
    }

    def applyTransform(p: Procedure): Unit = {
        val (beforeIn, afterIn) = solver.solveProc(p, backwards = true)
    }
}