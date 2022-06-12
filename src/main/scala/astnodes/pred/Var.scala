package astnodes.pred

import astnodes.exp
import astnodes.exp.variable.{Register, Variable}

// TODO are all predicate variables gamma?
case class Var (name: String) extends Pred {
  override def vars: List[Register] = ??? // List(new exp.Var(name))

  override def toString: String = name
  override def substExpr(v: Variable, w: Variable): Pred = this
}
