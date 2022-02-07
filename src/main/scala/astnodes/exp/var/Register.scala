package astnodes.exp.`var`

import astnodes.exp.{Expr, `var`}
import astnodes.pred
import astnodes.sec.SecVar

/** A register
  */
case class Register(name: String, override val size: Option[Int]) extends Var {
  override def toString = name
  override def vars: List[Register] = List(this)
  override def toGamma = SecVar(name, true)
  override def fold(old: Expr, sub: Expr): Expr = if (old == this) sub else this
}

case object Register {
  def apply(name: String, size: Int) = new Register(name, Some(size))

}
