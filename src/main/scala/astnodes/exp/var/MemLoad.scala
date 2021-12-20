package astnodes.exp.`var`

import astnodes.exp.*
import astnodes.pred

import java.util.Collections

/** Memory expression e.g. mem[10]
  */
case class MemLoad(var exp: Expr, override val size: Some[Int]) extends Var {

  override def toString = s"${if (this.onStack) "stack" else "heap"}[$exp]"

  // TODO this is a mess
  def toBoogieString(exp: Expr) = s"${if (this.onStack) "stack" else "heap"}[${exp.toBoogieString}]"
  override def toBoogieString: String =
    (0 to size.get / 8 - 1)
      .map(n => s"${toBoogieString(BinOp(BinOperator.Addition, exp, Literal(n.toString)))}")
      .mkString(" ++ ")

  override def vars = List(this) // TOOD also exp.vars????

  /** Assumes: anything on the stack is represented as SP + val (where val is an int etc)
    */
  def onStack = exp match {
    case v: Register              => v.name == "R31"
    case BinOp(_, v: Register, _) => v.name == "R31"
    case _                   => false
  }

  // TODO need to rework memload .....
  def toL = new pred.MemLoad(false, true, exp)
  def toGamma = new pred.MemLoad(true, false, exp)
}
