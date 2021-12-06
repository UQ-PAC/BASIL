package astnodes.pred

import astnodes.exp.{Expr, Var}

case class MemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Pred {
  override def vars: List[Var] = ??? // TODO we cant handle this atm
}
