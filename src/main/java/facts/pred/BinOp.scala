package facts.pred

case class BinOp(op: String, firstPred: Pred, secondPred: Pred) extends Pred
