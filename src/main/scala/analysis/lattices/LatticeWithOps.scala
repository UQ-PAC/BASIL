package analysis.lattices

import ir._
import analysis.util._

/** Lattice with abstract operators.
  */
trait LatticeWithOps extends Lattice:

    def apply(op: (Literal, Literal) => Literal, a: Element, b: Element): Element
    def apply(op: Literal => Literal, a: Element): Element

    def literal(l: Literal): Element
    def bvadd(a: Element, b: Element): Element
    def bvsub(a: Element, b: Element): Element
    def bvmul(a: Element, b: Element): Element
    def bvudiv(a: Element, b: Element): Element
    def bvsdiv(a: Element, b: Element): Element
    def bvsrem(a: Element, b: Element): Element
    def bvurem(a: Element, b: Element): Element
    // smod
    def bvshl(a: Element, b: Element): Element
    def bvlshr(a: Element, b: Element): Element
    def bvashr(a: Element, b: Element): Element
    def bvand(a: Element, b: Element): Element
    def bvor(a: Element, b: Element): Element
    def bvxor(a: Element, b: Element): Element
    def bvnand(a: Element, b: Element): Element
    def bvnor(a: Element, b: Element): Element
    def bvxnor(a: Element, b: Element): Element
    def bvule(a: Element, b: Element): Element
    def bvuge(a: Element, b: Element): Element
    def bvult(a: Element, b: Element): Element
    def bvugt(a: Element, b: Element): Element
    def bvsle(a: Element, b: Element): Element
    def bvsge(a: Element, b: Element): Element
    def bvslt(a: Element, b: Element): Element
    def bvsgt(a: Element, b: Element): Element
    def bvcomp(a: Element, b: Element): Element
    def zero_extend(width: Int, a: Element): Element
    def sign_extend(width: Int, a: Element): Element
    def extract(high: Int, low: Int, a: Element): Element
    def bvnot(a: Element): Element
    def bvneg(a: Element): Element
    def bvneq(a: Element, b: Element): Element
    def bveq(a: Element, b: Element): Element
    def concat(a: Element, b: Element): Element

trait LatticeWithDefaultOps extends LatticeWithOps:

    override def bvadd(a: Element, b: Element): Element = apply(smt_bvadd, a, b)
    override def bvsub(a: Element, b: Element): Element = apply(smt_bvsub, a, b)
    override def bvmul(a: Element, b: Element): Element = apply(smt_bvmul, a, b)
    override def bvudiv(a: Element, b: Element): Element = apply(smt_bvudiv, a, b)
    override def bvsdiv(a: Element, b: Element): Element = apply(smt_bvsdiv, a, b)
    override def bvsrem(a: Element, b: Element): Element = apply(smt_bvsrem, a, b)
    override def bvurem(a: Element, b: Element): Element = apply(smt_bvurem, a, b)
    override def bvand(a: Element, b: Element): Element = apply(smt_bvand, a, b)
    override def bvor(a: Element, b: Element): Element = apply(smt_bvor, a, b)
    override def bvnand(a: Element, b: Element): Element = apply(smt_bvnand, a, b) 
    override def bvnor(a: Element, b: Element): Element = apply(smt_bvnor, a, b) 
    override def bvxnor(a: Element, b: Element): Element = apply(smt_bvxnor, a, b)
    override def bvxor(a: Element, b: Element): Element = apply(smt_bvxor, a, b)
    override def bvnot(a: Element): Element = apply(smt_bvnot, a)
    override def bvneg(a: Element): Element = apply(smt_bvneg, a)
    override def bvshl(a: Element, b: Element): Element = apply(smt_bvshl, a, b)
    override def bvlshr(a: Element, b: Element): Element = apply(smt_bvlshr, a, b)
    override def bvashr(a: Element, b: Element): Element = apply(smt_bvashr, a, b)
    override def bvcomp(a: Element, b: Element): Element = apply(smt_bvcomp, a, b)
    override def bvule(a: Element, b: Element): Element = apply(smt_bvule, a, b)
    override def bvuge(a: Element, b: Element): Element = apply(smt_bvuge, a, b)
    override def bvult(a: Element, b: Element): Element = apply(smt_bvult, a, b)
    override def bvugt(a: Element, b: Element): Element = apply(smt_bvugt, a, b)
    override def bvsle(a: Element, b: Element): Element = apply(smt_bvsle, a, b)
    override def bvsge(a: Element, b: Element): Element = apply(smt_bvsge, a, b)
    override def bvslt(a: Element, b: Element): Element = apply(smt_bvslt, a, b)
    override def bvsgt(a: Element, b: Element): Element = apply(smt_bvsgt, a, b)
    override def zero_extend(width: Int, a: Element): Element = apply(smt_zero_extend(width, _: Literal), a)
    override def sign_extend(width: Int, a: Element): Element = apply(smt_sign_extend(width, _: Literal), a)
    override def extract(high: Int, low: Int, a: Element): Element = apply(boogie_extract(high, low, _: Literal), a)
    override def concat(a: Element, b: Element): Element = apply(smt_concat, a, b)
    override def bvneq(a: Element, b: Element): Element = apply(smt_bvneq, a, b)
    override def bveq(a: Element, b: Element): Element = apply(smt_bveq, a, b)
