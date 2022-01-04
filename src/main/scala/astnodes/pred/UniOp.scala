package astnodes.pred

import astnodes.exp.`var`.Register
import astnodes.exp.`var`.Var

case class UniOp(op: String, pred: Pred) extends Pred {
  override def vars = pred.vars
  override def toString = String.format("%s (%s)", op, pred)
  override def substExpr(v: Var, w: Var) = copy(pred = pred.substExpr(v,w))
}
