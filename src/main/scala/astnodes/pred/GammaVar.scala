package astnodes.pred
import astnodes.exp.Var

case class GammaVar(name: String) extends Pred {
  override def vars: List[Var] = List()
}
