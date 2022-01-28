package astnodes.exp

import astnodes.exp.`var`.{Register, Var}

import java.util
import java.util.Objects

/** Literal expression (e.g. 4, 5, 10)
  */
case class Literal(value: String, override val size: Option[Int] = None) extends Expr {
  /** Value of literal */
  override def toString = String.format("%s", value)
  override def toBoogieString: String = value + s"bv${if (size.isDefined) size.get else 64}"
  def asLong: Long = {
    try {
      println(value.toLong)
      value.toLong 
    } catch {
      case e: _ => -1 
    }
  }

  override def vars: List[Register] = List()
  override def subst(v: Expr, w: Expr): Expr = this
  override def fold(old: Expr, sub: Expr) = this
}

case object Literal {
  def apply(value: String, size: Option[Int] = None) = new Literal(parseHex(value), size)

  private def parseHex(value: String): String = {
    if (value.length < 3 || !(value.substring(0, 2) == "0x")) value
    else java.lang.Long.toUnsignedString(java.lang.Long.parseUnsignedLong(value.substring(2), 16))
  }
}
