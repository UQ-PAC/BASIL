package astnodes.pred

import astnodes.exp
import astnodes.exp.{BinOp, Expr}

case class MemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Pred {
<<<<<<< HEAD
  override def vars: List[exp.`var`.Register] = ??? // TODO we cant handle this atm
  override def toString =
    if (L) s"L(${loc.toBoogieString}, heap)"
    else String.format("%s%s[%s]", if (this.gamma) "Gamma_" else "", if (this.onStack) "stack" else "heap", loc.toBoogieString)

  def onStack = loc match {
    // TODO imporve
    case v: exp.`var`.Register => v.name == "R31"
    case BinOp(_, v: exp.`var`.Register, _) => v.name == "R31"
=======
  override def vars: List[Var] = ??? // TODO we cant handle this atm
  override def toString = String.format("%s%s[%s]", if (this.gamma) "Gamma_" else if (L) "L_" else "", if (this.onStack) "stack" else "heap", loc.toBoogieString)

  def onStack = loc match {
    // TODO imporve
    case v: Var => v.name == "R31"
    case BinOp(_, v: Var, _) => v.name == "R31"
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
    case _ => false
  }
}
