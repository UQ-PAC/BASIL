package astnodes.pred.secLattice
import astnodes.exp
import astnodes.pred.Pred

// TODO extends pred??
case class SecLatticeElemId (name: String) extends Pred {
  override def vars: List[exp.`var`.Register] = ???

  override def toString: String = s"s_$name"
  override def substExpr(v: exp.`var`.Var, w: exp.`var`.Var): Pred = this
}
