package astnodes.pred

import astnodes.exp.Expr

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  override def vars = firstExpr.vars ++ secondExpr.vars

  override def toString: String = s"(${firstExpr.toBoogieString} $op ${secondExpr.toBoogieString})"
}
