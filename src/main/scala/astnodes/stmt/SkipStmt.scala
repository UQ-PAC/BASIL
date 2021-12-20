package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.`var`.Var

import java.util

/** No instruction fact
  */
class SkipStmt(override val pc: String) extends Stmt(pc) {
  override def toString = String.format("%sskip;", getLabel)
  override def subst(v: Var, w: Var): Stmt = this
}
