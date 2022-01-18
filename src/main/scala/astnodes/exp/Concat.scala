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

  def getLhs = left
  def getRhs = right

  override def vars = left.vars ++ right.vars
  override def subst(v: Expr, w: Expr): Expr = {
    val newConcat = this.copy(left = left.subst(v,w), right = right.subst(v,w))
    // println(s"New concat: $newConcat")
    newConcat
  }

  def simplify: Expr = {
    if (left.isInstanceOf[Literal] && left.asInstanceOf[Literal].toString.equals("0")) {
      return this //right
    }

    return this
  }
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
