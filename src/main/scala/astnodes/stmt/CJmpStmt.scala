package astnodes.stmt

import java.util.Collections
import java.util
import astnodes.exp.Expr
import astnodes.exp.Var

/** Conditional Jump fact
  */
case class CJmpStmt(
                     override val pc: String,
                     val trueTarget: String,
                     val falseTarget: String,
                     var condition: Expr,
) extends Stmt(pc) {
  def getCondition = condition
  override def toString = f"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = f"if (${condition.toBoogieString}) { goto label$trueTarget; } goto label$falseTarget;"
  override def getChildren = Collections.singletonList(condition)
  override def replace(oldExp: Expr, newExp: Expr) = if (condition == oldExp) condition = newExp
}
