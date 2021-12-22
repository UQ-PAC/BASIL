package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.pred.Pred

class MethodCall(pc: String, name: String) extends Stmt(pc) {
  override def subst(v: Var, w: Var): Stmt = ???

  override def toString = s"call $name();"
}
