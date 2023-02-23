package analysis.util
import bap._

import scala.math.pow

/** nat2bv[m], with 0 < m, which takes a non-negative integer n and returns the (unique) bitvector b: [0, m) → {0, 1}
  * such that b(m-1)*2^{m-1} + ⋯ + b(0)*2^0 = n rem 2^m
  */
def nat2bv(m: Int, n: BigInt): BAPLiteral =
  require(m > 0, "length of bitvector must be positive")
  require(n >= 0, "input must be non-negative")

  BAPLiteral(n % BigInt(2).pow(m), m)

/** bv2nat, which takes a bitvector b: [0, m) → {0, 1} with 0 < m, and returns an integer in the range [0, 2^m), and is
  * defined as follows: bv2nat(b) := b(m-1)*2^{m-1} + b(m-2)*2^{m-2} + ⋯ + b(0)*2^0
  */
def bv2nat(b: BAPLiteral): BigInt = b.value

/** [[(bvadd s t)]] := nat2bv[m](bv2nat([[s]]) + bv2nat([[t]]))
  */
def bvadd(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) + bv2nat(t))

/** [[(bvmul s t)]] := nat2bv[m](bv2nat([[s]]) * bv2nat([[t]]))
  */
def bvmul(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) * bv2nat(t))

/** [[(bvneg s)]] := nat2bv[m](2^m - bv2nat([[s]]))
  */
def bvneg(s: BAPLiteral): BAPLiteral = nat2bv(s.size, BigInt(2).pow(s.size) - bv2nat(s))

/** (bvsub s t) abbreviates (bvadd s (bvneg t))
  */
def bvsub(s: BAPLiteral, t: BAPLiteral): BAPLiteral = bvadd(s, bvneg(t))

/** [[(bvand s t)]] := λx:[0, m). if [[s]](x) = 0 then 0 else [[t]](x)
  */
def bvand(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")
  BAPLiteral(s.value & t.value, s.size)

/** [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
  */
def bvor(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")
  BAPLiteral(s.value | t.value, s.size)

/** [[(bvnot s)]] := λx:[0, m). if [[s]](x) = 0 then 1 else 0
  */
def bvnot(s: BAPLiteral): BAPLiteral = BAPLiteral(BigInt(2).pow(s.size) - (s.value + 1), s.size)

/** [[(bvudiv s t)]] := if bv2nat([[t]]) = 0 then λx:[0, m). 1 else nat2bv[m](bv2nat([[s]]) div bv2nat([[t]]))
  */
def bvudiv(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")

  if bv2nat(t) == 0 then BAPLiteral(BigInt(2).pow(s.size) - 1, s.size)
  else nat2bv(s.size, bv2nat(s) / bv2nat(t))

/** (bvxor s t) abbreviates (bvor (bvand s (bvnot t)) (bvand (bvnot s) t))
  */
def bvxor(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")

  bvor(bvand(s, bvnot(t)), bvand(bvnot(s), t))

/** [[((_ extract i j) s))]] := λx:[0, i-j+1). [[s]](j + x) where s is of sort (_ BitVec l), 0 ≤ j ≤ i < l.
  */
def smt_extract(i: Int, j: Int, s: BAPLiteral): BAPLiteral =
  BAPLiteral((s.value >> j - 1) & (1 << (i - j + 1)) - 1, i - j + 1)

/** ((_ zero_extend i) t) abbreviates (concat ((_ repeat i) #b0) t)
  */
def smt_zero_extend(i: Int, s: BAPLiteral): BAPLiteral =
  require(i >= 0, "bits to be extended must be non-negative")
  BAPLiteral(s.value, s.size + i)

/** bit comparator: equals 1 iff all bits are equal otherwise 0
  */
def bvcomp(s: BAPLiteral, t: BAPLiteral) = if s == t then BAPLiteral(1, 1) else BAPLiteral(0, 1)

/** bit comparator: equals 0 iff all bits are equal otherwise 1
  */
def bvneq(s: BAPLiteral, t: BAPLiteral) = if s != t then BAPLiteral(1, 1) else BAPLiteral(0, 1)

/** [[bvult s t]] := true iff bv2nat([[s]]) < bv2nat([[t]])
  */
def bvult(s: BAPLiteral, t: BAPLiteral) =
  if bv2nat(s) < bv2nat(t) then BAPLiteral(1, 1)
  else BAPLiteral(0, 1)

/** (bvule s t) abbreviates (or (bvult s t) (= s t))
  */
def bvule(s: BAPLiteral, t: BAPLiteral) =
  if bv2nat(s) <= bv2nat(t) then BAPLiteral(1, 1)
  else BAPLiteral(0, 1)

/** shift left (equivalent to multiplication by 2^x where x is the value of the second argument)
  */
def smt_bvshl(s: BAPLiteral, t: BAPLiteral) =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) * BigInt(2).pow(bv2nat(t).toInt))

/** logical shift right (equivalent to unsigned division by 2^x where x is the value of the second argument)
  */
def smt_bvlshr(s: BAPLiteral, t: BAPLiteral): BAPLiteral =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) / BigInt(2).pow(bv2nat(t).toInt))

def isNegative(s: BAPLiteral): Boolean = if (s.value >= BigInt(2).pow(s.size - 1)) true else false

def bvsdiv(s: BAPLiteral, t: BAPLiteral) = {
  val msb_s = isNegative(s)
  val msb_t = isNegative(t)
  if (!msb_s && !msb_t) {
    bvudiv(s, t)
  } else if (msb_s && !msb_t) {
    bvneg(bvudiv(bvneg(s), t))
  } else if (!msb_s && msb_t) {
    bvneg(bvudiv(s, bvneg(t)))
  } else {
    bvudiv(bvneg(s), bvneg(t))
  }
}

def bvurem(s: BAPLiteral, t: BAPLiteral) = {
  require(s.size == t.size, "bitvector sizes must be the same")
  if (bv2nat(t) == BigInt(0)) {
    s
  } else {
    nat2bv(s.size, bv2nat(s) % bv2nat(t))
  }
}

def bvsrem(s: BAPLiteral, t: BAPLiteral) = {
  val msb_s = isNegative(s)
  val msb_t = isNegative(t)
  if (!msb_s && !msb_t) {
    bvurem(s, t)
  } else if (msb_s && !msb_t) {
    bvneg(bvurem(bvneg(s), t))
  } else if (!msb_s && msb_t) {
    bvurem(s, bvneg(t))
  } else {
    bvneg(bvurem(bvneg(s), bvneg(t)))
  }
}
def bvslt(s: BAPLiteral, t: BAPLiteral) = {
  val sNeg = isNegative(s)
  val tNeg = isNegative(t)
  if ((sNeg && !tNeg) || ((sNeg == tNeg) && (bvult(s, t) == BAPLiteral(1, 1)))) {
    BAPLiteral(1, 1)
  } else {
    BAPLiteral(0, 0)
  }
}

def bvsle(s: BAPLiteral, t: BAPLiteral) = {
  val sNeg = isNegative(s)
  val tNeg = isNegative(t)
  if ((sNeg && !tNeg) || ((sNeg == tNeg) && (bvule(s, t) == BAPLiteral(1, 1)))) {
    BAPLiteral(1, 1)
  } else {
    BAPLiteral(0, 0)
  }
}

def smt_bvashr(s: BAPLiteral, t: BAPLiteral) = {
  if (!isNegative(s)) {
    bvlshr(s, t)
  } else {
    bvnot(bvlshr(bvnot(s), t))
  }
}

def concat(s: BAPLiteral, t: BAPLiteral) = BAPLiteral((s.value << t.size) + t.value, s.size + t.size)

def smt_sign_extend(i: Int, s: BAPLiteral) = {
  if (isNegative(s)) {
    BAPLiteral(s.value + (BigInt(2).pow(i + 1) - 1), s.size + i)
  } else {
    smt_zero_extend(i, s)
  }
}

def extract(i: Int, j: Int, s: BAPLiteral): BAPLiteral = {
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

def sign_extend(i: Int, s: BAPLiteral): BAPLiteral = {
  if (i > s.size) {
    smt_sign_extend(i - s.size, s)
  } else {
    smt_extract(i - 1, 0, s)
  }
}

def zero_extend(i: Int, s: BAPLiteral): BAPLiteral = {
  if (i > s.size) {
    smt_zero_extend(i - s.size, s)
  } else {
    smt_extract(i - 1, 0, s)
  }
}

def bvshl(s: BAPLiteral, t: BAPLiteral): BAPLiteral = {
  if (s.size == t.size) {
    smt_bvshl(s, t)
  } else {
    smt_bvshl(s, smt_zero_extend(s.size - t.size, t))
  }
}

def bvlshr(s: BAPLiteral, t: BAPLiteral): BAPLiteral = {
  if (s.size == t.size) {
    smt_bvlshr(s, t)
  } else {
    smt_bvlshr(s, smt_zero_extend(s.size - t.size, t))
  }
}

def bvashr(s: BAPLiteral, t: BAPLiteral): BAPLiteral = {
  if (s.size == t.size) {
    smt_bvashr(s, t)
  } else {
    smt_bvashr(s, smt_zero_extend(s.size - t.size, t))
  }
}