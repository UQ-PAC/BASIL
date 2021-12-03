package astnodes.stmt

import java.util
import astnodes.exp.Expr

/** Jump
  */
class JmpStmt(override val pc: String, var target: String) extends Stmt(pc) {
  def getTarget = target
  def setTarget(target: String) = this.target = target
  override def toString = String.format("%sgoto label%s;", getLabel, target)
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}
}
