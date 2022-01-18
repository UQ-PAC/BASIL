package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.Label

/** Jump
  */
case class JmpStmt(val pc: String, val target: String) extends Stmt(Label(pc)) {
  override def toString = String.format("%sgoto label%s;", label, target)
  
  // override def subst(v: Var, w: Var): Stmt = this

  override def subst(v: Expr, w: Expr): Stmt = this
}
