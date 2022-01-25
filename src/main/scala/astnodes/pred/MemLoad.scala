package astnodes.pred

import astnodes.exp
import astnodes.exp.{BinOp, Expr}
import astnodes.exp.`var`.Var

case class MemLoad(loc: Expr) extends Pred {
  override def vars: List[exp.`var`.Register] = ??? // TODO we cant handle this atm
  override def toString = s"${if (this.onStack) "stack" else "heap"}[${loc.toBoogieString}]"

  def onStack = loc match {
    // TODO imporve
    case v: exp.`var`.Register              => v.name == "R31"
    case BinOp(_, v: exp.`var`.Register, _) => v.name == "R31"
    case _                                  => false
  }

  override def substExpr(v: Var, w: Var) = ???
}
