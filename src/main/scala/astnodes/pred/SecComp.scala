package astnodes.pred

import astnodes.exp.Expr
import astnodes.exp.variable.Variable
import astnodes.sec.Sec

/**
 *  
 */
case class SecComp(first: Sec, second: Sec) extends Pred {
  override def vars: List[Variable] = ???
  override def toString: String = s"($first <: $second)"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}

