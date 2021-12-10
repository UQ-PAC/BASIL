package astnodes.pred

import astnodes.exp.{BinOp, Expr, Var}

case class MemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Pred {
  override def vars: List[Var] = ??? // TODO we cant handle this atm
  override def toString = String.format("%s%s[%s]", if (this.gamma) "Gamma_" else if (L) "L_" else "", if (this.onStack) "stack" else "heap", loc)

  def onStack = loc match {
    // TODO imporve once everything is immutable
    case BinOp(_, v: Var, _) => v.name == "SP"
    case _ => false
  }
}
