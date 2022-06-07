package astnodes.pred

import astnodes.exp.`var`.Register
import astnodes.exp.`var`.Var

case class UniOp(op: String, pred: Pred) extends Pred {
  override def vars: List[Var] = pred.vars
  override def toString: String = String.format("%s (%s)", op, pred)
  override def substExpr(v: Var, w: Var): Pred = copy(pred = pred.substExpr(v,w))
}
