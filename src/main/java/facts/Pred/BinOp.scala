package facts.Pred

case class BinOp(op: String, firstPred: Pred, secondPred: Pred) extends Pred
