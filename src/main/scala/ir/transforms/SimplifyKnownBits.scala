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
        TNum(v, alpha & beta & ~v)
    }

    // Bitwise OR
    def TOR(that: TNum): TNum = {
        val v = this.value | that.value
        val mu = this.mask | that.mask
        TNum(v, mu & ~v);
    }

    // Bitwise XOR
    def TXOR(that: TNum): TNum = {
        val v = this.value ^ that.value
        val mu = this.mask | that.mask
        TNum(v & ~mu, mu)
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

        case UnaryExpr(op: BVNOT, arg: Expr) =>
            val argTNum = evaluateExprToTNum(s, arg)
            TNum(~argTNum.value & ~argTNum.mask, argTNum.mask)

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