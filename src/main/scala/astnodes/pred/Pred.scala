package astnodes.pred

import astnodes.exp.Var

trait Pred {
  def vars: List[Var]
  def toBoogieString = toString
}

/** Define custom methods for List[Expr]
  */
extension (preds: List[Pred]) {
  def conjunct: Pred = preds match {
    case pred :: Nil => pred
    case pred :: rest => BinOp(BinOperator.Conjuction, pred, rest.conjunct)
    case Nil => Bool.True
  }
}
