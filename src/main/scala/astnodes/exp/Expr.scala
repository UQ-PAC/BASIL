package astnodes.exp

import astnodes.exp.`var`.{MemLoad, Register, Var}

import java.util

/** Expression
 */
trait Expr {
  /*  All of the variables in a given expression */
  def vars: List[Var] 

  /* Substitute a given variable for another variable */
  def subst(v: Var, w: Var): Expr
  def toBoogieString: String = toString

  def fold (old: Expr, sub: Expr): Expr
  /* 
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Option[Int] 
}

