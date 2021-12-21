package astnodes.pred

// TODO fix types
case class Forall(ids: String, pred: String) extends Pred {
  override def vars = ???
  override def toString = s"(forall $ids :: $pred)"
}
