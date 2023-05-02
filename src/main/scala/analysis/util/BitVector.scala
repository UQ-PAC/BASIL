package analysis.util
import ir._

import scala.math.pow

/** nat2bv[m], with 0 < m, which takes a non-negative integer n and returns the (unique) bitvector b: [0, m) → {0, 1}
  * such that b(m-1)*2^{m-1} + ⋯ + b(0)*2^0 = n rem 2^m
  */
def nat2bv(m: Int, n: BigInt): BitVecLiteral =
  require(m > 0, "length of bitvector must be positive")
  require(n >= 0, "input must be non-negative")

  BitVecLiteral(n % BigInt(2).pow(m), m)

/** bv2nat, which takes a bitvector b: [0, m) → {0, 1} with 0 < m, and returns an integer in the range [0, 2^m), and is
  * defined as follows: bv2nat(b) := b(m-1)*2^{m-1} + b(m-2)*2^{m-2} + ⋯ + b(0)*2^0
  */
def bv2nat(b: BitVecLiteral): BigInt = b.value

/** [[(bvadd s t)]] := nat2bv[m](bv2nat([[s]]) + bv2nat([[t]]))
  */
def smt_bvadd(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    nat2bv(s.size, bv2nat(s) + bv2nat(t))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[(bvmul s t)]] := nat2bv[m](bv2nat([[s]]) * bv2nat([[t]]))
  */
def smt_bvmul(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    nat2bv(s.size, bv2nat(s) * bv2nat(t))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[(bvneg s)]] := nat2bv[m](2^m - bv2nat([[s]]))
  */
def smt_bvneg(s: Literal): BitVecLiteral = s match
  case s: BitVecLiteral => nat2bv(s.size, BigInt(2).pow(s.size) - bv2nat(s))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")

/** (bvsub s t) abbreviates (bvadd s (bvneg t))
  */
def smt_bvsub(s: Literal, t: Literal): BitVecLiteral = smt_bvadd(s, smt_bvneg(t))

/** [[(bvand s t)]] := λx:[0, m). if [[s]](x) = 0 then 0 else [[t]](x)
  */
def smt_bvand(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    BitVecLiteral(s.value & t.value, s.size)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
  */
def smt_bvor(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    BitVecLiteral(s.value | t.value, s.size)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[(bvnot s)]] := λx:[0, m). if [[s]](x) = 0 then 1 else 0
  */
def smt_bvnot(s: Literal): BitVecLiteral = s match
  case s: BitVecLiteral => BitVecLiteral(BigInt(2).pow(s.size) - (s.value + 1), s.size)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")

/** [[(bvudiv s t)]] := if bv2nat([[t]]) = 0 then λx:[0, m). 1 else nat2bv[m](bv2nat([[s]]) div bv2nat([[t]]))
  */
def smt_bvudiv(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")

    if bv2nat(t) == 0 then BitVecLiteral(BigInt(2).pow(s.size) - 1, s.size)
    else nat2bv(s.size, bv2nat(s) / bv2nat(t))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** (bvxor s t) abbreviates (bvor (bvand s (bvnot t)) (bvand (bvnot s) t))
  */
def smt_bvxor(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    smt_bvor(smt_bvand(s, smt_bvnot(t)), smt_bvand(smt_bvnot(s), t))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[((_ extract i j) s))]] := λx:[0, i-j+1). [[s]](j + x) where s is of sort (_ BitVec l), 0 ≤ j ≤ i < l.
  */
def smt_extract(i: Int, j: Int, s: Literal): BitVecLiteral = s match
  case s: BitVecLiteral => BitVecLiteral((s.value >> j - 1) & (1 << (i - j + 1)) - 1, i - j + 1)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")

/** Boogie unintuitively uses a slightly different extract operator to SMT-Lib.
  * We are matching the Boogie semantics */
def boogie_extract(i: Int, j: Int, s: Literal): BitVecLiteral = s match
  case s: BitVecLiteral => smt_extract(i - 1, j, s)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")


  /** ((_ zero_extend i) t) abbreviates (concat ((_ repeat i) #b0) t)
  */
def smt_zero_extend(i: Int, s: Literal): BitVecLiteral = s match
  case s: BitVecLiteral =>
    require(i >= 0, "bits to be extended must be non-negative")
    BitVecLiteral(s.value, s.size + i)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")

/** bit comparator: equals 1 iff all bits are equal otherwise 0
  */
def smt_bvcomp(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) => if s == t then BitVecLiteral(1, 1) else BitVecLiteral(0, 1)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bveq(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) => if s == t then TrueLiteral else FalseLiteral
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvneq(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) => if s != t then TrueLiteral else FalseLiteral
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** [[smt_bvult s t]] := true iff bv2nat([[s]]) < bv2nat([[t]])
  */
def smt_bvult(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    if bv2nat(s) < bv2nat(t) then TrueLiteral
    else FalseLiteral
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** (bvule s t) abbreviates (or (bvult s t) (= s t))
  */
def smt_bvule(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    if bv2nat(s) <= bv2nat(t) then TrueLiteral
    else FalseLiteral
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** shift left (equivalent to multiplication by 2^x where x is the value of the second argument)
  */
def smt_bvshl(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    nat2bv(s.size, bv2nat(s) * BigInt(2).pow(bv2nat(t).toInt))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

/** logical shift right (equivalent to unsigned division by 2^x where x is the value of the second argument)
  */
def smt_bvlshr(s: Literal, t: Literal): BitVecLiteral = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    nat2bv(s.size, bv2nat(s) / BigInt(2).pow(bv2nat(t).toInt))
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def isNegative(s: BitVecLiteral): Boolean = if (s.value >= BigInt(2).pow(s.size - 1)) true else false

def smt_bvsdiv(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
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
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvurem(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    require(s.size == t.size, "bitvector sizes must be the same")
    if (bv2nat(t) == BigInt(0)) {
      s
    } else {
      nat2bv(s.size, bv2nat(s) % bv2nat(t))
    }
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvsrem(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
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
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvslt(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    if ((sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvult(s, t) == TrueLiteral))) {
      TrueLiteral
    } else {
      FalseLiteral
    }
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvsle(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    val sNeg = isNegative(s)
    val tNeg = isNegative(t)
    if ((sNeg && !tNeg) || ((sNeg == tNeg) && (smt_bvule(s, t) == TrueLiteral))) {
      TrueLiteral
    } else {
      FalseLiteral
    }
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_bvashr(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    if (!isNegative(s)) {
      smt_bvlshr(s, t)
    } else {
      smt_bvnot(smt_bvlshr(smt_bvnot(s), t))
    }
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_concat(s: Literal, t: Literal) = (s, t) match
  case (s: BitVecLiteral, t: BitVecLiteral) =>
    BitVecLiteral((s.value << t.size) + t.value, s.size + t.size)
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvectors")

def smt_sign_extend(i: Int, s: Literal) = s match
  case s: BitVecLiteral =>
    if (isNegative(s)) {
      BitVecLiteral(s.value + (BigInt(2).pow(i + 1) - 1), s.size + i)
    } else {
      smt_zero_extend(i, s)
    }
  case _ => throw new Exception("cannot apply bitvector operator to non-bitvector")

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