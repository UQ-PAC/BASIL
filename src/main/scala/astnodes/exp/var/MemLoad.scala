package astnodes.exp.`var`

import astnodes.exp.*
import astnodes.pred
import astnodes.sec.SecMemLoad

/** A load from memory at location exp
 */
case class MemLoad(var exp: Expr, override val size: Some[Int]) extends Var {

  override def toString = s"${if (this.onStack) "stack" else "heap"}[$exp]"

  // TODO this is a mess
  def toBoogieString(exp: Expr) = s"${if (this.onStack) "stack" else "heap"}[${exp.toBoogieString}]"
  override def toBoogieString: String =
    (0 until size.get / 8)
      .map(n => s"${toBoogieString(BinOp(BinOperator.Addition, exp, Literal(n.toString, Some(64))))}")
      .mkString(" ++ ")

  override def vars = List(this) // TOOD also exp.vars????

  /** Assumes: anything on the stack is represented as SP + val (where val is an int etc)
    */
  val onStack: Boolean = onStackMatch(exp)

  def onStackMatch(expr: Expr): Boolean = expr match {
    case v: Register              => v.name == "R31"
    case BinOp(_, e1: Expr, e2: Expr) => onStackMatch(e1) || onStackMatch(e2)
    case UniOp(_, e1: Expr) => onStackMatch(e1)
    case _                   => false
  }
  
  override def fold(old: Expr, sub: Expr): Expr = {
    if (!this.onStack) this.copy(exp.fold(old, sub))
    else if (this.onStack && old == this) sub
    else this
  }

  def toL: SecMemLoad = SecMemLoad(false, true, exp)
  override def toGamma: SecMemLoad = SecMemLoad(true, false, exp)
}
