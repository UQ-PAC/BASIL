package astnodes.exp

import java.util.Collections
import java.util
import java.util.Objects
import astnodes.pred
import astnodes.pred.conjunct

/** Memory expression e.g. mem[10]
  */
case class MemLoad(var exp: Expr) extends Expr {
  override def toString = s"${if (this.onStack) "stack" else "heap"}[$exp]"
  override def toBoogieString = s"${if (this.onStack) "stack" else "heap"}[${exp.toBoogieString}]"
  override def getChildren = Collections.singletonList(exp)
  override def replace(oldExp: Expr, newExp: Expr) = if (exp == oldExp) exp = newExp

  override def vars: List[Var] = exp.vars

  /** Assumes: anything on the stack is represented as SP + val (where val is an int etc)
    */
  def onStack = exp match {
    // TODO imporve once everything is immutable
    case BinOp(_, v: Var, _) => v.name == "SP"
    case _ => false
  }

  // TODO need to rework memload .....
  def toL = new pred.MemLoad(false, true, exp)
  override def size = Some(32)
}
