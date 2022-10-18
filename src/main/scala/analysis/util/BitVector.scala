package analysis.util
import astnodes._

import scala.math.pow

/** nat2bv[m], with 0 < m, which takes a non-negative integer n and returns the (unique) bitvector b: [0, m) → {0, 1}
  * such that b(m-1)*2^{m-1} + ⋯ + b(0)*2^0 = n rem 2^m
  */
def nat2bv(m: Int, n: BigInt): Literal =
  require(m > 0, "length of bitvector must be positive")
  require(n >= 0, "input must be non-negative")

  Literal(n % pow(2, m).toInt, m)

/** bv2nat, which takes a bitvector b: [0, m) → {0, 1} with 0 < m, and returns an integer in the range [0, 2^m), and is
  * defined as follows: bv2nat(b) := b(m-1)*2^{m-1} + b(m-2)*2^{m-2} + ⋯ + b(0)*2^0
  */
def bv2nat(b: Literal): BigInt = b.value

/** [[(bvadd s t)]] := nat2bv[m](bv2nat([[s]]) + bv2nat([[t]]))
  */
def bvadd(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  nat2bv(s.size, bv2nat(s) + bv2nat(t))

/** [[(bvmul s t)]] := nat2bv[m](bv2nat([[s]]) * bv2nat([[t]]))
  */
def bvmul(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  nat2bv(s.size, bv2nat(s) * bv2nat(t))

/** [[(bvneg s)]] := nat2bv[m](2^m - bv2nat([[s]]))
  */
def bvneg(s: Literal): Literal = nat2bv(s.size, pow(2, s.size).toInt - bv2nat(s))

/** (bvsub s t) abbreviates (bvadd s (bvneg t))
  */
def bvsub(s: Literal, t: Literal): Literal = bvadd(s, bvneg(t))

/** [[(bvand s t)]] := λx:[0, m). if [[s]](x) = 0 then 0 else [[t]](x)
  */
def bvand(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  Literal(s.value & t.value, s.size)

/** [[(bvor s t)]] := λx:[0, m). if [[s]](x) = 1 then 1 else [[t]](x)
  */
def bvor(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  Literal(s.value | t.value, s.size)

/** [[(bvnot s)]] := λx:[0, m). if [[s]](x) = 0 then 1 else 0
  */
def bvnot(s: Literal): Literal = Literal(~s.value, s.size)

/** [[(bvudiv s t)]] := if bv2nat([[t]]) = 0 then λx:[0, m). 1 else nat2bv[m](bv2nat([[s]]) div bv2nat([[t]]))
  */
def bvudiv(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  if bv2nat(t) == 0 then Literal(pow(2, s.size).toInt - 1, s.size)
  else nat2bv(s.size, bv2nat(s) / bv2nat(t))

/** (bvxor s t) abbreviates (bvor (bvand s (bvnot t)) (bvand (bvnot s) t))
  */
def bvxor(s: Literal, t: Literal): Literal =
  require(s.size == t.size, "bitvector sizes must be the same")

  bvor(bvand(s, bvnot(t)), bvand(bvnot(s), t))

/** [[((_ extract i j) s))]] := λx:[0, i-j+1). [[s]](j + x) where s is of sort (_ BitVec l), 0 ≤ j ≤ i < l.
  */
def bvextract(i: Int, j: Int, s: Literal): Literal =
  Literal((s.value >> j - 1) & (1 << (i - j + 1)) - 1, i - j + 1)

def zero_extend(i: Int, s: Literal): Literal =
  require(i >= 0, "bits to be extended must be non-negative")
  Literal(s.value, s.size + i)

def bvcomp(s: Literal, t: Literal) =
  if s.size != t.size then Literal(0, 1)

  if bv2nat(s) == bv2nat(t) then Literal(1, 1)
  else Literal(0, 1)

def bvneq(s: Literal, t: Literal) =
  if s.size != t.size then Literal(1, 1)

  if bv2nat(s) != bv2nat(t) then Literal(1, 1)
  else Literal(0, 1)

def bvult(s: Literal, t: Literal) =
  if bv2nat(s) < bv2nat(t) then Literal(1, 1)
  else Literal(0, 1)

def bvulte(s: Literal, t: Literal) =
  if bv2nat(s) < bv2nat(t) then Literal(1, 1)
  else if bv2nat(s) == bv2nat(t) && s.size == t.size then Literal(1, 1)
  else Literal(0, 1)

def bvshl(s: Literal, t: Literal) =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) * pow(2, bv2nat(t).toDouble).toInt)

def bvlshr(s: Literal, t: Literal) =
  require(s.size == t.size, "bitvector sizes must be the same")
  nat2bv(s.size, bv2nat(s) / pow(2, bv2nat(t).toDouble).toInt)
