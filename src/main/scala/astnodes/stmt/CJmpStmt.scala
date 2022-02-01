package astnodes.stmt

import java.util.Collections
import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}
import astnodes.Label

/** Conditional Jump fact
  */
case class CJmpStmt(
                     val pc: String,
                     val trueTarget: String,
                     val falseTarget: String,
                     var condition: Expr,
) extends Stmt(Label(pc)) {
  def getCondition = condition
  override def toString = s"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = s"if (bv1tobool(${condition.toBoogieString})) { goto label$trueTarget; } goto label$falseTarget;"

  override def subst(v: Var, w: Var): Stmt = this.copy(condition = condition.subst(v,w))
  def fold(old: Expr, sub: Expr): Stmt = this.copy(condition = condition.fold(old, sub))
}
