package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.Variable

/** No instruction fact
  */
case class SkipStmt(override val pc: String) extends Stmt(pc) {
  override def toString: String = String.format("%sskip;", labelString)
  override def subst(v: Variable, w: Variable): Stmt = this

  //override def withVisibleLabel: Stmt = copy(labelVisible = true)
}
