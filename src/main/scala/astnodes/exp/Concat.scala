package astnodes.exp
import astnodes.exp.`var`.Var

import java.util
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import analysis.tools.SimplificationUtil

/**
 *  Concatenation of two bitvectors
 */
case class Concat (left: Expr, right: Expr) extends Expr{
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Option[Int] = (left.size, right.size) match {
    case (Some(x), Some(y)) => Some(x + y)
  }

  override def vars = left.vars ++ right.vars
  override def subst(v: Var, w: Var): Expr = {
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
  // FIXME: This implementation is wrong. Extend is the same as Pad, but it uses the most signficant bit, rather than
  //  always adding 0's. This can be thought of as an signed pad.
  def apply(expr: Expr, size: Int) =
    if (size - expr.size.get > 0) Concat(expr, Literal("0", Some(size - expr.size.get)))
    else expr

}

/**
 * Construct a Concatenation from a Bil pad operation
 */
case object Pad {
  def apply(expr: Expr, size: Int) =
    if (size - expr.size.get > 0) Concat(Literal("0", Some(size - expr.size.get)), expr)
    else expr

}
