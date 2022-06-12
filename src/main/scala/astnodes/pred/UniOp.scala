package astnodes.pred

import astnodes.exp.variable.{Register, Variable}

case class UniOp(op: String, pred: Pred) extends Pred {
  override def vars: List[Variable] = pred.vars
  override def toString: String = String.format("%s (%s)", op, pred)
  override def substExpr(v: Variable, w: Variable): Pred = copy(pred = pred.substExpr(v,w))
}
