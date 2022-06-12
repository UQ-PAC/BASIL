package astnodes.exp

import astnodes.exp.variable.Variable

/** Expression
  */
trait Expr {
  /*  All of the variables in a given expression */
  def vars: List[Variable]

  /* Substitute a given variable for another variable */
  def subst(v: Variable, w: Variable): Expr
  def toBoogieString: String = toString

  def fold(old: Expr, sub: Expr): Expr
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Option[Int]
}
