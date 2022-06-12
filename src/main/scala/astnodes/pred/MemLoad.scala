package astnodes.pred

import astnodes.exp.{BinOp, Expr}
import astnodes.exp
import astnodes.exp.variable.Variable

case class MemLoad (loc: Expr) extends Pred {
  override def vars: List[astnodes.exp.variable.Register] = ??? // TODO we cant handle this atm
  override def toString = s"${if (this.onStack) "stack" else "heap"}[${loc.toBoogieString}]"

  def onStack: Boolean = loc match {
    // TODO improve
    case v: astnodes.exp.variable.Register => v.name == "R31"
    case BinOp(_, v: astnodes.exp.variable.Register, _) => v.name == "R31"
    case _ => false
  }


  override def substExpr(v: Variable, w: Variable): Pred = ???
}
