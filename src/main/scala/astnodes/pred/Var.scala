package astnodes.pred
import astnodes.exp

// TODO are all predicate variables gamma?
case class Var (name: String, gamma: Boolean) extends Pred {
  override def vars: List[exp.Var] = List(new exp.Var(name))

  override def toString: String = (if (gamma) "Gamma_" else "") + name
}
