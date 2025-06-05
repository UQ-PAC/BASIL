package ir.eval
import ir.*
import scala.annotation.tailrec
import scala.math.pow
import BitVectorEval.*

object InfixBitVectorEval {

  given BVTruthValue: Conversion[BitVecLiteral, Boolean] with
    def apply(x: BitVecLiteral): Boolean = x.value > 0

  def zero_extend(sz: Int, l: BitVecLiteral) = smt_zero_extend(sz, l)
  def sign_extend(sz: Int, l: BitVecLiteral) = smt_sign_extend(sz, l)
  def bvult(l: BitVecLiteral, r: BitVecLiteral) = smt_bvult(l, r)
  def bvugt(l: BitVecLiteral, r: BitVecLiteral) = smt_bvugt(l, r)
  def bvule(l: BitVecLiteral, r: BitVecLiteral) = smt_bvule(l, r)
  def bvuge(l: BitVecLiteral, r: BitVecLiteral) = smt_bvuge(l, r)

  extension (l: Int)
    def bv(w: Int) =
      require(w >= 0)
      BitVecLiteral(l, w)

    def bv1 = BitVecLiteral(l, 1)
    def bv16 = BitVecLiteral(l, 16)
    def bv32 = BitVecLiteral(l, 32)
    def bv64 = BitVecLiteral(l, 64)

  extension (l: BitVecLiteral)

    def unary_- = smt_bvneg(l)
    def unary_~ = smt_bvnot(l)

    infix def +(r: BitVecLiteral) = smt_bvadd(l, r)
    infix def -(r: BitVecLiteral) = smt_bvsub(l, r)
    infix def &(r: BitVecLiteral) = smt_bvand(l, r)
    infix def |(r: BitVecLiteral) = smt_bvor(l, r)
    infix def ^(r: BitVecLiteral) = smt_bvxor(l, r)

    infix def >>(r: BitVecLiteral) = smt_bvashr(l, r)
    infix def >>>(r: BitVecLiteral) = smt_bvlshr(l, r)
    infix def <<(r: BitVecLiteral) = smt_bvshl(l, r)
    infix def *(r: BitVecLiteral) = smt_bvmul(l, r)
    infix def /(r: BitVecLiteral) = smt_bvsdiv(l, r)

    // TODO: rem, urem
    infix def %(r: BitVecLiteral) = smt_bvsmod(l, r)

    infix def <(r: BitVecLiteral) = smt_bvslt(l, r)
    infix def >(r: BitVecLiteral) = smt_bvsgt(l, r)
    infix def <=(r: BitVecLiteral) = smt_bvsle(l, r)
    infix def >=(r: BitVecLiteral) = smt_bvsge(l, r)

    infix def ++(r: BitVecLiteral) = smt_concat(l, r)
    def apply(hi: Int, lo: Int) = boogie_extract(hi, lo, l)

}
