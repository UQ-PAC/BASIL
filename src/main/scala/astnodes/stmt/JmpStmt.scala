package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.Label

/** Jump
  */
case class JmpStmt(pc: String, target: String) extends Stmt(Label(pc)) {
  override def toString: String = String.format("%sgoto label%s;", label, target)
  override def subst(v: Var, w: Var): Stmt = this
}
