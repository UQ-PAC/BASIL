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

    // s is the abstract state from previous command/block
    override def transfer(s: Map[Variable, TNum], b: Command): Map[Variable, TNum] = {
        b match {
            // Binary expression assignment with two bitvectors (e.g. x = 0101 & 1010)
            case LocalAssign(lhs: Variable, BinaryExpr(op: BinOp, arg1: BitVecLiteral, arg2: BitVecLiteral)) =>
                val tn1 = toTNum(arg1)
                val tn2 = toTNum(arg2)

                op match {
                    case BVAND => s.updated(lhs, tn1.TAND(tn2))
                    case BVOR => s.updated(lhs, tn1.TOR(tn2))
                    case BVXOR => s.updated(lhs, tn1.TXOR(tn2))
                }

            // Assign variable to variable (e.g. x = y)
            case LocalAssign(lhs: Variable, rhs: Variable) =>
                if (s.contains(rhs)) { 
                    s.updated(lhs, s(rhs)) 
                } else {
                    // Overapproxiate unknown variables with Top
                    s.updated(lhs, TNum(BigInt(0), BigInt(-1)))
                }

            // Assign bitvector to variable (e.g. x = 0101)
            case LocalAssign(lhs: Variable, rhs: BitVecLiteral) =>
                s.updated(lhs, toTNum(rhs))

            case LocalAssign(lhs: Variable, BinaryExpr(op: BinOp, arg1: IntLiteral, arg2: IntLiteral)) =>

            case LocalAssign(lhs: Variable, BinaryExpr(op: BinOp, arg1: Variable, arg2: Variable)) =>
            case LocalAssign(lhs: Variable, BinaryExpr(op: BinOp, arg1: Extract, arg2: Extract)) =>
            case LocalAssign(lhs: Variable, BinaryExpr(op: BinOp, arg1: Repeat, arg2: Repeat)) =>

            case MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int) =>
                if (s.contains(lhs)) {
                    s
                } else {
                    // Overapproxiate memory values with Top
                    s.updated(lhs, TNum(BigInt(0), BigInt(-1)))
                }

            // Default case    
            case _ => s 
        }
    }

    // Join the same variables and merge shared TNum values, otherwise replace with Top
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