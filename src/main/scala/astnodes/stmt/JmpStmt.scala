package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.Var

/** Jump
  */
class JmpStmt(override val pc: String, val target: String) extends Stmt(pc) {
  override def toString = String.format("%sgoto label%s;", getLabel, target)
  override def subst(v: Var, w: Var): Stmt = this
}
