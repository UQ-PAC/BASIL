package astnodes.pred

import astnodes.exp.Expr
import astnodes.exp.`var`.Var

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  override def vars = firstExpr.vars ++ secondExpr.vars
  override def toString: String = s"(${firstExpr.toBoogieString} $op ${secondExpr.toBoogieString})"
  override def substExpr(v: Var, w: Var) =
    this.copy(firstExpr = firstExpr.subst(v, w), secondExpr = secondExpr.subst(v, w))
}
