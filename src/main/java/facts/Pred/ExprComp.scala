package facts.Pred

import facts.exp.Expr

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred
