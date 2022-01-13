package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.pred.Pred

class MethodCall(pc: String, name: String) extends Stmt(pc) {
  override def subst(v: Expr, w: Expr): Stmt = ???

  override def toString = s"call $name();"
}
