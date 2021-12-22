package astnodes.pred

import astnodes.exp.`var`.Register

case class BinOp(op: BinOperator, firstPred: Pred, secondPred: Pred) extends Pred {
  def this(operatorStr: String, firstPred: Pred, secondPred: Pred) = this(BinOperator.valueOf(operatorStr), firstPred, secondPred)
  override def vars = firstPred.vars ++ secondPred.vars
<<<<<<< HEAD
  override def toString = s"($firstPred $op $secondPred)"
=======
  override def toString = s"$firstPred $op $secondPred"
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
}

enum BinOperator (val boogieRepr: String) {
  case Implication extends BinOperator("==>")
  case Conjuction extends BinOperator("&&")
  case Disjunction extends BinOperator("||")
  case Equality extends BinOperator("==")
  case InEquality extends BinOperator("!=")

  override def toString: String = boogieRepr
}
