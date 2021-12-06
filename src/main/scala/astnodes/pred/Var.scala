package astnodes.pred
import astnodes.exp

case class Var (name: String, gamma: Boolean, L: Boolean) extends Pred {
  override def vars: List[exp.Var] = List(new exp.Var(name))
}
