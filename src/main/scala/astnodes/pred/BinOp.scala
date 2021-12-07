package astnodes.pred
import astnodes.exp.Var

case class BinOp(op: String, firstPred: Pred, secondPred: Pred) extends Pred {
  override def vars: List[Var] = firstPred.vars ++ secondPred.vars
  override def toString = f"$firstPred $op $secondPred"
}
