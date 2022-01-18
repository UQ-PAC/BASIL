package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.Label

/** No instruction fact
  */
case class SkipStmt(val pc: String) extends Stmt(Label(pc)) {
  override def toString = String.format("%sskip;", label)

  // main
  // override def subst(v: Var, w: Var): Stmt = this

  override def subst(v: Expr, w: Expr): Stmt = this
}
