package astnodes.sec

import astnodes.exp.{BinOp, Expr}
import astnodes.exp
import astnodes.exp.variable.Variable

case class SecMemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Sec {
  override def toString: String =
    if (L) s"L(${loc.toBoogieString}, heap)"
    else String.format("%s%s[%s]", if (this.gamma) "Gamma_" else "", if (this.onStack) "stack" else "heap", loc.toBoogieString)

  def onStack: Boolean = loc match {
    // TODO improve
    case v: astnodes.exp.variable.Register => v.name == "R31"
    case BinOp(_, v: astnodes.exp.variable.Register, _) => v.name == "R31"
    case _ => false
  }

  override def vars: List[Variable] = List()
}
