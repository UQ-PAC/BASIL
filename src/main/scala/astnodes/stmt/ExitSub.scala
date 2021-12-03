package astnodes.stmt

import java.util
import astnodes.exp.Expr

class ExitSub(override val pc: String) extends Stmt(pc) {
  override def toString = "return;"
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}
}
