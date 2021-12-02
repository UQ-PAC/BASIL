package astnodes.pred

import astnodes.exp.{Expr, Var}

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  override def vars: List[Var] = firstExpr.vars ++ secondExpr.vars
}
