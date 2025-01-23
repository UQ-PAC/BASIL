package ir

// A = definite 1 bits, B = unknown bits
// A[i] = 1, B[i] = 0 -> Bit i is definitely 1
// A[i] = 0, B[i] = 0 -> Bit i is definitely 0
// A[i] = 0, B[i] = 1 -> Bit i is unknown (T)

// e.g.
// y = x & z

// x = {1, T, 0, T}
// Ax = 1000, Bx = 0101

// z = {T, 1, 1, 0}
// Az = 0110, Bz = 1000

// Bitwise AND:
// Ay[i] = Ax[i] & Az[i]    ->    Ay[i] = 0000
// By[i] = (Bx[i] || Bz[i]) & ~(Ay[i])  ->  By[i] = 1101
// y = {T, T, 0, T}

// Represents a single bit in a bitvector
case class Trit(a: Int, b: Int) {
    // Definite 1
    def isOne: Boolean = a == 1 && b == 0

    // Definite 0
    def isZero: Boolean = a == 0 && b == 0

    // Unknown (Top)
    def isTop: Boolean = a == 0 && b == 1

    // Bitwise AND
    def TAND(that: Trit): Trit = {
        val newA = this.a & that.a
        val newB = (this.b | that.b) & ~(newA)
        Trit(newA, newB)
    }

    // Bitwise OR
    def TOR(that: Trit): Trit = {
        val newA = this.a | that.a
        val newB = (this.b | that.b) & ~(newA)
        Trit(newA, newB)
    }

    // Bitwise XOR
    def TXOR(that: Trit): Trit = {
        val newA = this.a ^ that.a
        val newB = (this.b | that.b) & ~(newA)
        Trit(newA, newB)
    }
}

// A bitvector is represented by one or more Trits
case class TritVector(trits: Vector[Trit]) {
    def TVAND(that: TritVector): TritVector = {
        TritVector(this.trits.zip(that.trits) map { case (t1, t2) => t1.TAND(t2)})
    }

    def TVOR(that:TritVector): TritVector = {
        TritVector(this.trits.zip(that.trits) map { case (t1, t2) => t1.TOR(t2)})
    }

    def TVXOR(that:TritVector): TritVector = {
        TritVector(this.trits.zip(that.trits) map { case (t1, t2) => t1.TXOR(t2)})
    }
}

// Check AbstractDomain signature
class TritSimplificationDomain extends AbstractDomain[Map[Variable, Expr]] {
    def toTritVector(b: BitVecLiteral): TritVector = {
        val trits = (0 until bv.length).map { i =>
            val bit = (bv.value >> i) & 1
            if (bit == 1) {
                Trit(1, 0)
            } else if (bit == 0) {
                Trit(0, 0)
            } else {
                Trit(0, 1) // Top element - condition needs to change since BitVecLiteral does not store Top
            }
        }
        TritVector(trits.toVector)
    }

    def toBitVecLiteral(t: TritVector): BitVecLiteral = {
        val bitVectorString = t.trits.map {
            case Trit(1, 0) => '1'
            case Trit(0, 0) => '0'
            case Trit(0, 1) => ???
        }.mkString

        val value = BigInt(bitVectorString, 2)
        val size = t.trits.size
        BitVecLiteral(value, size)
    }

    // BitVecLiteral does not store Top element so parameter type should change
    override def transfer(s: Map[Variable, BitVecLiteral], b: Command): Map[Variable, TritVector] = {
        b match {
            case a: LocalAssign if a.rhs.isInstanceOf[BinaryExpr] => 
                if (a.rhs.arg1.isInstanceOf[BitVecLiteral] && a.rhs.arg2.isInstanceOf[BitVecLiteral]) {
                    val tv1 = toTritVector(a.rhs.arg1.asInstanceOf[BitVecLiteral])
                    val tv2 = toTritVector(a.rhs.arg2.asInstanceOf[BitVecLiteral])

                    a.rhs.op match {
                        case result: BVAND => tv1.TVAND(tv2)
                        case result: BVOR => tv1.TVOR(tv2)
                        case result: BVXOR => tv1.TVXOR(tv2)
                    }

                    s.updated(a.lhs, result)
                } else {
                    s
                }
            case _ => s 
        }
    }

    override def top: Map[Variable, TritVector] = Map.empty
    override def bot: Map[Variable, TritVector] = Map.empty

    override def join(left: Map[Variable, TritVector], right: Map[Variable, TritVector], pos: Block): Map[Variable, TritVector] = {
        (left.keySet ++ right.keySet).map { key =>
            val leftVector = left.getOrElse(key, TritVector(Vector.empty))
            val rightVector = right.getOrElse(key, TritVector(Vector.empty))
    
            val joinedVector = TritVector(
                leftVector.trits.zipAll(rightVector.trits, Trit(0, 1), Trit(0, 1)).map {
                    _ => Trit(0, 1)
                }
            )
            key -> joinedVector
        }.toMap
    }
}

// call applyTransform method in RunUtils
// transforms.SimplifyExprWithTrit().applyTransform(ctx.program)
class SimplifyExprWithTrit() {
    val solver = transforms.worklistSolver(TritSimplificationDomain())

    def applyTransform(p: Program): Unit = {
        for (proc <- p.procedures) {
            applyTransform(proc)
        }
    }

    def applyTransform(p: Procedure): Unit = {
        val (beforeIn, afterIn) = solver.solveProc(p, backwards = true)
    }
}