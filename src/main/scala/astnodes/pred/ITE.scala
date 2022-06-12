package astnodes.pred

import astnodes.exp.variable.{Register, Variable}

/** If then else expression
 */
case class ITE(cond: Pred, firstPred: Pred, secondPred: Pred) extends Pred {
  override def vars: List[Variable] = cond.vars ++ firstPred.vars ++ secondPred.vars
  override def toString = s"if ($cond) then $firstPred else $secondPred"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}
