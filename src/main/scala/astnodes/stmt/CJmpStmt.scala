package astnodes.stmt

import java.util.Collections
import java.util
import astnodes.exp.Expr
import astnodes.exp.Var

/** Conditional Jump fact
  */
class CJmpStmt(
    override val pc: String,
    var target: String,
    var condition: Expr // TODO changed this to ExpFact but should check this
) extends Stmt(pc) {
  def getTarget = target
  def getCondition = condition
  override def toString = String.format("%sif (%s) goto label%s;", getLabel, condition, target)
  override def getChildren = Collections.singletonList(condition)
  override def replace(oldExp: Expr, newExp: Expr) = if (condition == oldExp) condition = newExp
}
