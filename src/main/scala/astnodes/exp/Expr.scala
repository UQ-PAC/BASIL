package astnodes.exp

import astnodes.exp.`var`.{MemLoad, Register, Var}

import java.util

/** Abstract class of an expression fact
  */
trait Expr {
  def vars: List[Var] // TODO rework this
  def subst(v: Var, w: Var): Expr
  def toBoogieString = toString
  def size: Option[Int]
}

