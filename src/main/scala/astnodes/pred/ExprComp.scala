package astnodes.pred

import astnodes.exp.Expr
import astnodes.exp.variable.Variable

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  override def vars: List[Variable] = firstExpr.vars ++ secondExpr.vars
  override def toString: String = s"(${firstExpr.toBoogieString} $op ${secondExpr.toBoogieString})"
  override def substExpr(v: Variable, w: Variable): Pred = this.copy(firstExpr = firstExpr.subst(v, w), secondExpr = secondExpr.subst(v,w))
}
