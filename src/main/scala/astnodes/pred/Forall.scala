package astnodes.pred
import astnodes.exp.`var`.Var

// TODO fix types
case class Forall(ids: String, pred: String) extends Pred {
  override def vars = ???
  override def toString = s"(forall $ids :: $pred)"
  override def substExpr(v: Var, w: Var) = ???
}
