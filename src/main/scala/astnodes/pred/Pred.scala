package astnodes.pred

import astnodes.exp.variable.{MemLoad, Register, Variable}

trait Pred {
  def vars: List[Variable]
  def substExpr(v: Variable, w: Variable): Pred
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
