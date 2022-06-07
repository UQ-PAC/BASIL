package astnodes.pred

import astnodes.exp.`var`.{MemLoad, Register, Var}

trait Pred {
  def vars: List[Var]
  def substExpr(v: Var, w: Var): Pred
  def toBoogieString: String = toString
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
