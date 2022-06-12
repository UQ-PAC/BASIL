package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.Variable
import astnodes.pred.Pred

case class Assert(override val pc: String, pred: Pred) extends Stmt(pc) {
  override def subst(v: Variable, w: Variable): Stmt = ???

  // TODO is there a way to merge these two methods
  override def toString: String = s"assert $pred;"
  override def toBoogieString: String = s"assert ${pred.toBoogieString};"
}
