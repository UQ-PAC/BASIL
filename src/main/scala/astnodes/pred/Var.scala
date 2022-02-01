package astnodes.pred
import astnodes.exp
import astnodes.exp.`var`.{Register, Var}

// TODO are all predicate variables gamma?
case class Var (name: String) extends Pred {
  override def vars: List[Register] = ??? // List(new exp.Var(name))

  override def toString: String = name
  override def substExpr(v: exp.`var`.Var, w: exp.`var`.Var): Pred = this
}
