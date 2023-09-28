package analysis
import ir._
import analysis.BitVectorEval.*

import scala.math.pow

object BitVectorEval {

  /** nat2bv[m], with 0 < m, which takes a non-negative integer n and returns the (unique) bitvector b: [0, m) → {0, 1}
    * such that b(m-1)*2^{m-1} + ⋯ + b(0)*2^0 = n rem 2^m
    */
  def nat2bv(bitSize: Int, n: BigInt): BitVecLiteral =
    require(bitSize > 0, "length of bitvector must be positive")
    require(n >= 0, "input must be non-negative")

    BitVecLiteral(n % BigInt(2).pow(bitSize), bitSize)

  /** Converts a boolean value into the BoolLit enumerated type
    */
  private def bool2BoolLit(value: Boolean): BoolLit =
    if (value)
      TrueLiteral
    else
      FalseLiteral

  /** bv2nat, which takes a bitvector b: [0, m) → {0, 1} with 0 < m, and returns an integer in the range [0, 2^m), and
    * is defined as follows: bv2nat(b) := b(m-1)*2^{m-1} + b(m-2)*2^{m-2} + ⋯ + b(0)*2^0
    */
  def bv2nat(b: Literal): BigInt = b.asInstanceOf[BitVecLiteral].value

  /** (bvadd (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - addition modulo 2^m
    *
    * [[(bvadd s t)]] := nat2bv[m](bv2nat([[s]]) + bv2nat([[t]]))
    */
  def smt_bvadd(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size)

    nat2bv(sb.size, bv2nat(s) + bv2nat(tb))
  }

  /** */

  /** (bvmul (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - multiplication modulo 2^m
    *
    * [[(bvmul s t)]] := nat2bv[m](bv2nat([[s]]) * bv2nat([[t]]))
    */
  def smt_bvmul(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    nat2bv(sb.size, bv2nat(sb) * bv2nat(tb))
  }

  /** (bvneg (_ BitVec m) (_ BitVec m))
    *   - 2's complement unary minus
    *
    * [[(bvneg s)]] := nat2bv[m](2^m - bv2nat([[s]]))
    */
  def smt_bvneg(s: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]

    nat2bv(sb.size, BigInt(2).pow(sb.size) - bv2nat(sb))
  }

  /** (bvsub (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement subtraction modulo 2^m
    */
  def smt_bvsub(s: Literal, t: Literal): BitVecLiteral = smt_bvadd(s, smt_bvneg(t))

  /** (bvand (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise and
    */
  def smt_bvand(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    BitVecLiteral(sb.value & tb.value, sb.size)
  }

  /** [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
    */

  /** (bvor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise or
    *
    * [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
    */
  def smt_bvor(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    BitVecLiteral(sb.value | tb.value, sb.size)
  }

  /** (bvnot (_ BitVec m) (_ BitVec m))
    *   - bitwise negation
    *
    * [[(bvnot s)]] := λx:[0, m). if [[s]](x) = 0 then 1 else 0
    */
  def smt_bvnot(s: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]

    BitVecLiteral(BigInt(2).pow(sb.size) - (sb.value + 1), sb.size)
  }

  /** (bvudiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - unsigned division, truncating towards 0
    *
    * [[(bvudiv s t)]] := if bv2nat([[t]]) = 0 then λx:[0, m). 1 else nat2bv[m](bv2nat([[s]]) div bv2nat([[t]]))
    */
  def smt_bvudiv(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    if bv2nat(tb) == 0 then BitVecLiteral(BigInt(2).pow(sb.size) - 1, sb.size)
    else nat2bv(sb.size, bv2nat(s) / bv2nat(t))
  }

  /** (bvxor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise exclusive or
    */
  def smt_bvxor(s: Literal, t: Literal): BitVecLiteral =
    smt_bvor(smt_bvand(s, smt_bvnot(t)), smt_bvand(smt_bvnot(s), t))

  /** (bvnand (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise nand (negation of and)
    */
  def smt_bvnand(s: Literal, t: Literal): BitVecLiteral = smt_bvnot(smt_bvand(s, t))

  /** (bvxor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise exclusive or
    */
  def smt_bvnor(s: Literal, t: Literal): BitVecLiteral = smt_bvnot(smt_bvor(s, t))

  /** (bvxnor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *
    * bitwise equivalence (equivalently, negation of bitwise exclusive or)
    */
  def smt_bvxnor(s: Literal, t: Literal): BitVecLiteral = smt_bvnot(smt_bvxor(s, t))

  /** ((_ extract i j) (_ BitVec m) (_ BitVec n))
    *
    * extraction of bits i down to j from a bitvector of size m to yield a new bitvector of size n, where n = i - j + 1
    */
  def smt_extract(i: Int, j: Int, s: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(i >= j)
    val sb = s.asInstanceOf[BitVecLiteral]

    BitVecLiteral((sb.value >> j) & ((BigInt(1) << (i - j + 1)) - 1), i - j + 1)
  }

  /** Boogie unintuitively uses a slightly different extract operator to SMT-Lib. We are matching the Boogie semantics
    */
  def boogie_extract(i: Int, j: Int, s: Literal): BitVecLiteral = smt_extract(i - 1, j, s)

  /** ((_ zero_extend i) (_ BitVec m) (_ BitVec m+i))
    *   - ((_ zero_extend i) x) means extend x with zeroes to the (unsigned) equivalent bitvector of size m+i
    */
  def smt_zero_extend(i: Int, s: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]

    require(i >= 0, "bits to be extended must be non-negative")

    BitVecLiteral(sb.value, sb.size + i)
  }

  /** (bvcomp (_ BitVec m) (_ BitVec m) (_ BitVec 1))
    *   - bit comparator: equals #b1 iff all bits are equal
    */
  def smt_bvcomp(s: Literal, t: Literal): BitVecLiteral =
    if s == t then BitVecLiteral(1, 1)
    else BitVecLiteral(0, 1)

  /** (bvneq (_ BitVec m) (_ BitVec m))
    *   - not equal too
    */
  def smt_bveq(s: Literal, t: Literal): BoolLit = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    bool2BoolLit(s == t)
  }

  /** (bvneq (_ BitVec m) (_ BitVec m))
    *   - not equal too
    */
  def smt_bvneq(s: Literal, t: Literal): BoolLit = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    bool2BoolLit(s != t)
  }

  /** (bvshl (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - shift left (equivalent to multiplication by 2^x where x is the value of the second argument)
    */
  def smt_bvshl(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    nat2bv(sb.size, bv2nat(s) * BigInt(2).pow(bv2nat(t).toInt))
  }

  /** (bvlshr (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - logical shift right (equivalent to unsigned division by 2^x where x is the value of the second argument)
    */
  def smt_bvlshr(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    nat2bv(sb.size, bv2nat(s) / BigInt(2).pow(bv2nat(t).toInt))
  }

  def isNegative(s: Literal): Boolean = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]

    sb.value >= BigInt(2).pow(sb.size - 1)
  }

  /** (bvsdiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement signed division
    */
  def smt_bvsdiv(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val msb_s = isNegative(s)
    val msb_t = isNegative(t)
    if (!msb_s && !msb_t) {
      smt_bvudiv(s, t)
    } else if (msb_s && !msb_t) {
      smt_bvneg(smt_bvudiv(smt_bvneg(s), t))
    } else if (!msb_s && msb_t) {
      smt_bvneg(smt_bvudiv(s, smt_bvneg(t)))
    } else {
      smt_bvudiv(smt_bvneg(s), smt_bvneg(t))
    }
  }

  /** (bvurem (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - unsigned remainder from truncating division
    */
  def smt_bvurem(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    require(sb.size == tb.size, "bitvector sizes must be the same")

    if (bv2nat(t) == BigInt(0)) {
      sb
    } else {
      nat2bv(sb.size, bv2nat(s) % bv2nat(t))
    }
  }

  /** (bvsrem (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement signed remainder (sign follows dividend)
    */
  def smt_bvsrem(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val msb_s = isNegative(s)
    val msb_t = isNegative(t)
    if (!msb_s && !msb_t) {
      smt_bvurem(s, t)
    } else if (msb_s && !msb_t) {
      smt_bvneg(smt_bvurem(smt_bvneg(s), t))
    } else if (!msb_s && msb_t) {
      smt_bvurem(s, smt_bvneg(t))
    } else {
      smt_bvneg(smt_bvurem(smt_bvneg(s), smt_bvneg(t)))
    }
  }

  /** (bvult (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned less-than
    */
  def smt_bvult(s: Literal, t: Literal): BoolLit = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    bool2BoolLit(bv2nat(s) < bv2nat(t))
  }

  /** (bvule (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned less than or equal
    */
  def smt_bvule(s: Literal, t: Literal): BoolLit = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    bool2BoolLit(bv2nat(s) <= bv2nat(t))
  }

  /** (bvugt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned greater than
    */
  def smt_bvugt(s: Literal, t: Literal): BoolLit = {
    smt_bvult(t, s)
  }

  /** (bvuge (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned greater than or equal
    */
  def smt_bvuge(s: Literal, t: Literal): BoolLit = smt_bvule(t, s)

  /** (bvslt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed less than
    */
  def smt_bvslt(s: Literal, t: Literal): BoolLit = {
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    bool2BoolLit((sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvult(s, t) == TrueLiteral)))
  }

  /** (bvsle (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed less than or equal
    */
  def smt_bvsle(s: Literal, t: Literal): BoolLit =
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    bool2BoolLit((sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvule(s, t) == TrueLiteral)))

  /** (bvsgt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed greater than
    */
  def smt_bvsgt(s: Literal, t: Literal): BoolLit = smt_bvslt(t, s)

  /** (bvsge (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed greater than or equal
    */
  def smt_bvsge(s: Literal, t: Literal): BoolLit = smt_bvsle(t, s)

  def smt_bvashr(s: Literal, t: Literal): BitVecLiteral =
    if (!isNegative(s)) {
      smt_bvlshr(s, t)
    } else {
      smt_bvnot(smt_bvlshr(smt_bvnot(s), t))
    }

  def smt_concat(s: Literal, t: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])
    require(t.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]
    val tb = t.asInstanceOf[BitVecLiteral]

    BitVecLiteral((sb.value << tb.size) + tb.value, sb.size + tb.size)
  }

  def smt_sign_extend(i: Int, s: Literal): BitVecLiteral = {
    require(s.isInstanceOf[BitVecLiteral])

    val sb = s.asInstanceOf[BitVecLiteral]

    if (isNegative(s)) {
      BitVecLiteral((BigInt(2).pow(sb.size + i) - 1) - smt_bvneg(sb).value + 1, sb.size + i)
    } else {
      smt_zero_extend(i, sb)
    }
  }

  /*
  def extract(i: Int, j: Int, s: Literal): BitVecLiteral = {
    val size = i - j + 1
    if (size > s.size) {
      if (j == 0) {
        smt_zero_extend(size - s.size, s)
      } else {
        smt_extract(i + 1, j, smt_zero_extend(size - s.size, s))
      }
    } else {
      smt_extract(i + 1, j, s)
    }
  }

  def sign_extend(i: Int, s: Literal): BitVecLiteral = {
    if (i > s.size) {
      smt_sign_extend(i - s.size, s)
    } else {
      smt_extract(i - 1, 0, s)
    }
  }

  def zero_extend(i: Int, s: Literal): BitVecLiteral = {
    if (i > s.size) {
      smt_zero_extend(i - s.size, s)
    } else {
      smt_extract(i - 1, 0, s)
    }
  }
   */

  /*
  def bvshl(s: Literal, t: Literal): BitVecLiteral = (s, t) match
    case (s: BitVecLiteral, t: BitVecLiteral) =>
      if (s.size == t.size) {
        smt_bvshl(s, t)
      } else {
        smt_bvshl(s, smt_zero_extend(s.size - t.size, t))
      }
    case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

  def bvlshr(s: Literal, t: Literal): BitVecLiteral = (s, t) match
    case (s: BitVecLiteral, t: BitVecLiteral) =>
      if (s.size == t.size) {
        smt_bvlshr(s, t)
      } else {
        smt_bvlshr(s, smt_zero_extend(s.size - t.size, t))
      }
    case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

  def bvashr(s: Literal, t: Literal): BitVecLiteral = (s, t) match
    case (s: BitVecLiteral, t: BitVecLiteral) =>
      if (s.size == t.size) {
        smt_bvashr(s, t)
      } else {
        smt_bvashr(s, smt_zero_extend(s.size - t.size, t))
      }
    case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")
   */
}
