package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.pred.Pred
import astnodes.Label

class MethodCall(pc: String, name: String) extends Stmt(Label(pc)) {
  override def subst(v: Var, w: Var): Stmt = ???

// My vers
// case class MethodCall(pc: String, name: String) extends Stmt(Label(pc)) {
//   override def subst(v: Var, w: Var): Stmt = ???

  override def toString = s"call $name();"
}
