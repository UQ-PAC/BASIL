package astnodes.pred

<<<<<<< HEAD
import astnodes.exp.`var`.{MemLoad, Register, Var}
=======
import astnodes.exp.{Var, MemLoad}
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65

trait Pred {
  def vars: List[Var | MemLoad]  // TODO
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
