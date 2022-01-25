package astnodes.sec

import astnodes.pred.Pred

// TODO not used
case class SecITE(cond: Pred, first: Sec, second: Sec) extends Sec {
  override def toString = s"secITE($cond, $first, $second)"
  override def vars = cond.vars ++ first.vars ++ second.vars
}
