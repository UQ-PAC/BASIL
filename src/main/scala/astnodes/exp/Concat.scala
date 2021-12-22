package astnodes.exp
import astnodes.exp.`var`.Var

import java.util
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/**
 *  Concatenation of two bitvectors
 */
case class Concat (left: Expr, right: Expr) extends Expr{
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Option[Int] = (left.size, right.size) match {
    case (Some(x), Some(y)) => Some(x + y)
  }

  override def vars = left.vars ++ right.vars
  override def subst(v: Var, w: Var): Expr = this.copy(left = left.subst(v,w), right = right.subst(v,w))
}

/**
 * Construct a Concatenation from a Bil extend operation
 */
case object Extend {
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