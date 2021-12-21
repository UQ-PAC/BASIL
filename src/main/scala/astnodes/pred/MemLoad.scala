package astnodes.pred

import astnodes.exp
import astnodes.exp.{BinOp, Expr}

case class MemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Pred {
  override def vars: List[exp.`var`.Register] = ??? // TODO we cant handle this atm
  override def toString =
    if (L) s"L(${loc.toBoogieString}, heap)"
    else String.format("%s%s[%s]", if (this.gamma) "Gamma_" else "", if (this.onStack) "stack" else "heap", loc.toBoogieString)

  def onStack = loc match {
    // TODO imporve
    case v: exp.`var`.Register => v.name == "R31"
    case BinOp(_, v: exp.`var`.Register, _) => v.name == "R31"
    case _ => false
  }
}
