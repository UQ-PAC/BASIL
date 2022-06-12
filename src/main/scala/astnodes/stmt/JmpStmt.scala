package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.Variable

/** Jump
  */
case class JmpStmt(override val pc: String, target: String) extends Stmt(pc) {
  override def toString: String = String.format("%sgoto label%s;", labelString, target)
  override def subst(v: Variable, w: Variable): Stmt = this
}
