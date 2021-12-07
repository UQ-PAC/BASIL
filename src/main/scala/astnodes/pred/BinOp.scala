package astnodes.pred

import astnodes.exp.Var

case class BinOp(op: BinOperator, firstPred: Pred, secondPred: Pred) extends Pred {
  def this(operatorStr: String, firstPred: Pred, secondPred: Pred) = this(BinOperator.valueOf(operatorStr), firstPred, secondPred)
  override def vars: List[Var] = firstPred.vars ++ secondPred.vars
  override def toString = s"$firstPred $op $secondPred"
}

enum BinOperator (val boogieRepr: String) {
  case Implication extends BinOperator("==>")
  case Conjuction extends BinOperator("&&")
  case Disjunction extends BinOperator("||")
  case Equality extends BinOperator("==")
  case InEquality extends BinOperator("!=")

  override def toString: String = boogieRepr
}
