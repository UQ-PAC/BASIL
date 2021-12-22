package astnodes.pred

import astnodes.exp.`var`.{Register, Var}

case class BinOp(op: BinOperator, firstPred: Pred, secondPred: Pred) extends Pred {
  def this(operatorStr: String, firstPred: Pred, secondPred: Pred) = this(BinOperator.valueOf(operatorStr), firstPred, secondPred)
  override def vars = firstPred.vars ++ secondPred.vars
  override def toString = s"($firstPred $op $secondPred)"
  override def substExpr(v: Var, w: Var) = this.copy(firstPred = firstPred.substExpr(v,w), secondPred = secondPred.substExpr(v,w))
}

enum BinOperator (val boogieRepr: String) {
  case Implication extends BinOperator("==>")
  case Conjuction extends BinOperator("&&")
  case Disjunction extends BinOperator("||")
  case Equality extends BinOperator("==")
  case InEquality extends BinOperator("!=")

  override def toString: String = boogieRepr
}
