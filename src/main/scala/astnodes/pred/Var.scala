package astnodes.pred
import astnodes.exp
import astnodes.exp.`var`.Register

// TODO are all predicate variables gamma?
case class Var (name: String, gamma: Boolean) extends Pred {
<<<<<<< HEAD
  override def vars: List[Register] = ??? // List(new exp.Var(name))
=======
  override def vars: List[exp.Var] = ??? // List(new exp.Var(name))
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65

  override def toString: String = (if (gamma) "Gamma_" else "") + name
}
