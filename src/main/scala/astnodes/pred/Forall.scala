package astnodes.pred
import astnodes.exp.variable.Variable

// TODO fix types
case class Forall(ids: String, pred: String) extends Pred {
  override def vars: List[Variable] = ???
  override def toString = s"(forall $ids :: $pred)"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}
