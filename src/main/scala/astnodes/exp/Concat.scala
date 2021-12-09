package astnodes.exp
import java.util
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

case class Concat (left: Expr, right: Expr) extends Expr{
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Option[Int] = (left.size, right.size) match {
    case (Some(x), Some(y)) => Some(x + y)
  }

  override def vars: List[Var] = left.vars ++ right.vars
  override def replace(oldExpr: Expr, newExpr: Expr): Unit = {} // TODO
  override def getChildren: util.List[Expr] = ArrayBuffer(left, right).asJava

}

case object Extend {
  def apply(expr: Expr, size: Int) = Concat(expr, Literal("0", Some(size - expr.size.get)))

}

case object Pad {
  def apply(expr: Expr, size: Int) =
    // println(s"$expr:${expr.size.get} $size")
    Concat(Literal("0", Some(size - expr.size.get)), expr)

}