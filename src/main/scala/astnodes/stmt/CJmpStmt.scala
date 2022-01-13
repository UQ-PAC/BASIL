package astnodes.stmt

import java.util.Collections
import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}

/** Conditional Jump fact
  */
case class CJmpStmt(
                     override val pc: String,
                     val trueTarget: String,
                     val falseTarget: String,
                     var condition: Expr,
) extends Stmt(pc) {
  def getCondition = condition
  override def toString = s"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = s"if (bv1tobool(${condition.toBoogieString})) { goto label$trueTarget; } goto label$falseTarget;"

  override def subst(v: Expr, w: Expr): Stmt = this.copy(condition = condition.subst(v,w))
}
