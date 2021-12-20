package astnodes.pred

import astnodes.exp.`var`.Register

case class ITE(cond: Pred, firstPred: Pred, secondPred: Pred) extends Pred {
  override def vars = cond.vars ++ firstPred.vars ++ secondPred.vars
  override def toString = s"if ($cond) then $firstPred else $secondPred"
}
