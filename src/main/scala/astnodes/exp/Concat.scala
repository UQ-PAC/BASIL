package astnodes.exp

import analysis.tools.SimplificationUtil
import astnodes.exp.variable.Variable

/**
 *  Concatenation of two bitvectors
 */
case class Concat (left: Expr, right: Expr) extends Expr{
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Option[Int] = (left.size, right.size) match {
    case (Some(x), Some(y)) => Some(x + y)
    case _ => None
  }

  override def vars: List[Variable] = left.vars ++ right.vars
  override def subst(v: Variable, w: Variable): Expr = {
    this.copy(left = left.subst(v,w), right = right.subst(v,w))
  }

  override def fold(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.bitvecConcat(this.copy(left = left.fold(old,sub), right = right.fold(old,sub)))
  }
}

/**
 * Construct a Concatenation from a Bil extend operation
 */
case object Extend {
  // FIXME: This implementation is wrong. Extend is the same as Pad, but it uses the most significant bit, rather than
  //  always adding 0's. This can be thought of as an signed pad.
  def apply(expr: Expr, size: Int): Expr =
    if (size - expr.size.get > 0) Concat(expr, Literal("0", Some(size - expr.size.get)))
    else expr

}

/**
 * Construct a Concatenation from a Bil pad operation
 */
case object Pad {
  def apply(expr: Expr, size: Int): Expr =
    if (size - expr.size.get > 0) Concat(Literal("0", Some(size - expr.size.get)), expr)
    else expr

}
