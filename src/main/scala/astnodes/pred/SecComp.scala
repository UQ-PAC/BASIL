package astnodes.pred

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.sec.Sec

/**
 *  
 */
case class SecComp(first: Sec, second: Sec) extends Pred {
  override def vars: List[Var] = ???
  override def toString: String = s"($first <: $second)"
  override def substExpr(v: Var, w: Var): Pred = ???
}

