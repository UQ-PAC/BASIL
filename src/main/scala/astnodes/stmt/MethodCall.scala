package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.Variable
import astnodes.pred.Pred

case class MethodCall(override val pc: String, name: String) extends Stmt(pc) {
  override def subst(v: Variable, w: Variable): Stmt = ???
  override def toString = s"call $name();"
}
