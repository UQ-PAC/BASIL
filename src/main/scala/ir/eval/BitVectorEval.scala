package ir.eval
import ir.*

object BitVectorEval {

  /** nat2bv[m], with 0 < m, which takes a non-negative integer n and returns the (unique) bitvector b: [0, m) → {0, 1}
    * such that b(m-1)*2^{m-1} + ⋯ + b(0)*2^0 = n rem 2^m
    */
  def nat2bv(bitSize: Int, n: BigInt): BitVecLiteral =
    require(bitSize > 0, "length of bitvector must be positive")
    require(n >= 0, s"input must be non-negative : ($n, $bitSize)")

    BitVecLiteral(n % BigInt(2).pow(bitSize), bitSize)

  /** Converts a boolean value into the BoolLit enumerated type
    */
  private def bool2BoolLit(value: Boolean): BoolLit = if (value) TrueLiteral else FalseLiteral

  /** bv2nat, which takes a bitvector b: [0, m) → {0, 1} with 0 < m, and returns an integer in the range [0, 2^m), and
    * is defined as follows: bv2nat(b) := b(m-1)*2^{m-1} + b(m-2)*2^{m-2} + ⋯ + b(0)*2^0
    */
  def bv2nat(b: BitVecLiteral): BigInt = b.value

  /** Converts a bitvector value to its corresponding signed integer
    */
  def bv2SignedInt(b: BitVecLiteral): BigInt =
    if isNegative(b) then b.value - BigInt(2).pow(b.size)
    else b.value

  /** Converts a signed integer value to its corresponding Bitvector,
   *  assuming it falls within the representable range of the bitvector
   *  with size [[size]].
   *
    */
  def signedInt2BV(size: Int, i: BigInt): BitVecLiteral =
    if (i > 0) then BitVecLiteral(i, size) else smt_bvneg(BitVecLiteral(-i, size))

  /** (bvadd (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - addition modulo 2^m
    *
    * ```
    * [[(bvadd s t)]] := nat2bv[m](bv2nat([[s]]) + bv2nat([[t]]))
    * ```
    */
  def smt_bvadd(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size)
    nat2bv(s.size, bv2nat(s) + bv2nat(t))
  }

  /** (bvmul (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - multiplication modulo 2^m
    *
    * ```
    * [[(bvmul s t)]] := nat2bv[m](bv2nat([[s]]) * bv2nat([[t]]))
    * ```
    */
  def smt_bvmul(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    nat2bv(s.size, bv2nat(s) * bv2nat(t))
  }

  /** (bvneg (_ BitVec m) (_ BitVec m))
    *   - 2's complement unary minus
    *
    * ```
    * [[(bvneg s)]] := nat2bv[m](2^m - bv2nat([[s]]))
    * ```
    */
  def smt_bvneg(s: BitVecLiteral): BitVecLiteral = {
    nat2bv(s.size, BigInt(2).pow(s.size) - bv2nat(s))
  }

  /** (bvsub (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement subtraction modulo 2^m
    */
  def smt_bvsub(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = smt_bvadd(s, smt_bvneg(t))

  /** (bvand (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise and
    */
  def smt_bvand(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    BitVecLiteral(s.value & t.value, s.size)
  }

  /** [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
    */

  /** (bvor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise or
    *
    * ```
    * [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
    * ```
    */
  def smt_bvor(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    BitVecLiteral(s.value | t.value, s.size)
  }

  /** (bvnot (_ BitVec m) (_ BitVec m))
    *   - bitwise negation
    *
    * ```
    * [[(bvnot s)]] := λx:[0, m). if [[s]](x) = 0 then 1 else 0
    * ```
    */
  def smt_bvnot(s: BitVecLiteral): BitVecLiteral = {
    BitVecLiteral(BigInt(2).pow(s.size) - (s.value + 1), s.size)
  }

  /** (bvudiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - unsigned division, truncating towards 0
    *
    * ```
    * [[(bvudiv s t)]] := if bv2nat([[t]]) = 0 then λx:[0, m). 1 else nat2bv[m](bv2nat([[s]]) div bv2nat([[t]]))
    * ```
    */
  def smt_bvudiv(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    if bv2nat(t) == 0 then BitVecLiteral(BigInt(2).pow(s.size) - 1, s.size)
    else nat2bv(s.size, bv2nat(s) / bv2nat(t))
  }

  /** (bvxor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise exclusive or
    */
  def smt_bvxor(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral =
    smt_bvor(smt_bvand(s, smt_bvnot(t)), smt_bvand(smt_bvnot(s), t))

  /** (bvnand (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise nand (negation of and)
    */
  def smt_bvnand(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = smt_bvnot(smt_bvand(s, t))

  /** (bvxor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - bitwise exclusive or
    */
  def smt_bvnor(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = smt_bvnot(smt_bvor(s, t))

  /** (bvxnor (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *
    * bitwise equivalence (equivalently, negation of bitwise exclusive or)
    */
  def smt_bvxnor(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = smt_bvnot(smt_bvxor(s, t))

  /** ((_ extract i j) (_ BitVec m) (_ BitVec n))
    *
    * extraction of bits i down to j from a bitvector of size m to yield a new bitvector of size n, where n = i - j + 1
    * ```
    * [[((_ extract i j) s))]] := λx:[0, i-j+1). [[s]](j + x)
    * where s is of sort (_ BitVec l), 0 ≤ j ≤ i < l.
    * ```
    *
    * That is, `smt_extract(hi, lo, e)` where `hi` and `lo` are both *inclusive*.
    */
  def smt_extract(i: Int, j: Int, s: BitVecLiteral): BitVecLiteral = {
    require(i >= j)
    BitVecLiteral((s.value >> j) & ((BigInt(1) << (i - j + 1)) - 1), i - j + 1)
  }

  /** Boogie unintuitively uses a slightly different extract operator to SMT-Lib. We are matching the Boogie semantics.
    *
    * I gather this means `boogie_extract(hi_exclusive, lo, e)`, where `hi_exclusive` is exclusive
    * (unlike [[smt_extract]]).
    */
  def boogie_extract(i: Int, j: Int, s: BitVecLiteral): BitVecLiteral = smt_extract(i - 1, j, s)

  /** ((_ zero_extend i) (_ BitVec m) (_ BitVec m+i))
    *   - ((_ zero_extend i) x) means extend x with zeroes to the (unsigned) equivalent bitvector of size m+i
    */
  def smt_zero_extend(i: Int, s: BitVecLiteral): BitVecLiteral = {
    require(i >= 0, "bits to be extended must be non-negative")

    BitVecLiteral(s.value, s.size + i)
  }

  /** (bvcomp (_ BitVec m) (_ BitVec m) (_ BitVec 1))
    *   - bit comparator: equals #b1 iff all bits are equal
    */
  def smt_bvcomp(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral =
    if s == t then BitVecLiteral(1, 1)
    else BitVecLiteral(0, 1)

  /** (bveq (_ BitVec m) (_ BitVec m))
    *   - equal to
    */
  def smt_bveq(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    s == t
  }

  /** (bvneq (_ BitVec m) (_ BitVec m))
    *   - not equal to
    */
  def smt_bvneq(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    s != t
  }

  /** (bvshl (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - shift left (equivalent to multiplication by 2^x where x is the value of the second argument)
    */
  def smt_bvshl(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    nat2bv(s.size, bv2nat(s) * BigInt(2).pow(bv2nat(t).toInt))
  }

  /** (bvlshr (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - logical shift right (equivalent to unsigned division by 2^x where x is the value of the second argument)
    */
  def smt_bvlshr(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    nat2bv(s.size, bv2nat(s) / BigInt(2).pow(bv2nat(t).toInt))
  }

  def isNegative(s: BitVecLiteral): Boolean = {
    s.value >= BigInt(2).pow(s.size - 1)
  }

  /** (bvsdiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement signed division
    */
  def smt_bvsdiv(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
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
  def smt_bvurem(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    require(s.size == t.size, "bitvector sizes must be the same")

    if (bv2nat(t) == BigInt(0)) {
      s
    } else {
      nat2bv(s.size, bv2nat(s) % bv2nat(t))
    }
  }

  /** (bvsrem (_ BitVec m) (_ BitVec m) (_ BitVec m))
    *   - 2's complement signed remainder (sign follows dividend)
    */
  def smt_bvsrem(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
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

  def smt_bvsmod(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    val msb_s = isNegative(s)
    val msb_t = isNegative(t)
    val abs_s = if msb_s then smt_bvneg(s) else s
    val abs_t = if msb_t then smt_bvneg(t) else t
    val u = smt_bvurem(abs_s, abs_t)
    if (u.value == 0) {
      u
    } else if (!msb_s && !msb_t) {
      u
    } else if (msb_s && !msb_t) {
      smt_bvadd(smt_bvneg(u), t)
    } else if (!msb_s && msb_t) {
      smt_bvadd(u, t)
    } else {
      smt_bvneg(u)
    }
  }

  /** (bvult (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned less-than
    */
  def smt_bvult(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    bv2nat(s) < bv2nat(t)
  }

  /** (bvule (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned less than or equal
    */
  def smt_bvule(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    bv2nat(s) <= bv2nat(t)
  }

  /** (bvugt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned greater than
    */
  def smt_bvugt(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    smt_bvult(t, s)
  }

  /** (bvuge (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for unsigned greater than or equal
    */
  def smt_bvuge(s: BitVecLiteral, t: BitVecLiteral): Boolean = smt_bvule(t, s)

  /** (bvslt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed less than
    */
  def smt_bvslt(s: BitVecLiteral, t: BitVecLiteral): Boolean = {
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    (sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvult(s, t)))
  }

  /** (bvsle (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed less than or equal
    */
  def smt_bvsle(s: BitVecLiteral, t: BitVecLiteral): Boolean =
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    (sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvule(s, t)))

  /** (bvsgt (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed greater than
    */
  def smt_bvsgt(s: BitVecLiteral, t: BitVecLiteral): Boolean = smt_bvslt(t, s)

  /** (bvsge (_ BitVec m) (_ BitVec m) Bool)
    *   - binary predicate for signed greater than or equal
    */
  def smt_bvsge(s: BitVecLiteral, t: BitVecLiteral): Boolean = smt_bvsle(t, s)

  def smt_bvashr(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral =
    if (!isNegative(s)) {
      smt_bvlshr(s, t)
    } else {
      smt_bvnot(smt_bvlshr(smt_bvnot(s), t))
    }

  def smt_concat(s: BitVecLiteral, t: BitVecLiteral): BitVecLiteral = {
    BitVecLiteral((s.value << t.size) + t.value, s.size + t.size)
  }

  def smt_sign_extend(i: Int, s: BitVecLiteral): BitVecLiteral = {
    if (isNegative(s)) {
      BitVecLiteral((BigInt(2).pow(s.size + i) - 1) - smt_bvneg(s).value + 1, s.size + i)
    } else {
      smt_zero_extend(i, s)
    }
  }

  def repeat_bits(i: Int, s: BitVecLiteral) = {
    var x = s
    for (_ <- 1 until i) {
      x = smt_concat(x, s)
    }
    x
  }
}
