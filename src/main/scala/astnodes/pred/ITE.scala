package astnodes.pred

import astnodes.exp.`var`.Register
import astnodes.exp.`var`.Var

/** If then else expression
 */
case class ITE(cond: Pred, firstPred: Pred, secondPred: Pred) extends Pred {
  override def vars: List[Var] = cond.vars ++ firstPred.vars ++ secondPred.vars
  override def toString = s"if ($cond) then $firstPred else $secondPred"
  override def substExpr(v: Var, w: Var): Pred = ???
}
