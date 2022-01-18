package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.pred.Pred
import astnodes.Label

import java.util

class Assert(pc: String, var pred: Pred) extends Stmt(Label(pc)) {
  override def subst(v: Expr, w: Expr): Stmt = ???

// main vers:
// case class Assert(pc: String, var pred: Pred) extends Stmt(Label(pc)) {
//   override def subst(v: Var, w: Var): Stmt = ???

  // TODO is there a way to merge these two methods
  override def toString: String = s"assert $pred;"
  override def toBoogieString: String = s"assert ${pred.toBoogieString};"
}
