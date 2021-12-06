package astnodes.stmt

import astnodes.exp.Expr
import java.util

/** No instruction fact
  */
class SkipStmt(override val pc: String) extends Stmt(pc) {
  override def toString = String.format("%sskip;", getLabel)
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}
}
