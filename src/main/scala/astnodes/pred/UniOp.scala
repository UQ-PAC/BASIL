package astnodes.pred

import astnodes.exp.`var`.Register

case class UniOp(op: String, pred: Pred) extends Pred {
  override def vars = pred.vars
  override def toString = String.format("%s (%s)", op, pred)
}
