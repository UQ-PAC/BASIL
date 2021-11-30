package facts.pred

trait Pred {}

/** Define custom methods for List[Expr]
  */
extension (preds: List[Pred]) {
  def conjunct: Pred = preds match {
    case pred :: Nil => pred
    case pred :: rest => BinOp("&&", pred, rest.conjunct)
    case Nil => Bool.True
  }
}
