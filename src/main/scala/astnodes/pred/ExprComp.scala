package astnodes.pred

import astnodes.exp.Expr

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  override def vars = firstExpr.vars ++ secondExpr.vars
<<<<<<< HEAD

  override def toString: String = s"(${firstExpr.toBoogieString} $op ${secondExpr.toBoogieString})"
=======
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
}
