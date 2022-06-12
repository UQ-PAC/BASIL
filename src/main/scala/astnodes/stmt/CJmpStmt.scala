package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.{Register, Variable}

/** Conditional Jump fact
  */
case class CJmpStmt(override val pc: String,
  trueTarget: String,
  falseTarget: String,
  var condition: Expr,
) extends Stmt(pc) {
  def getCondition: Expr = condition
  override def toString = s"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = s"if (bv1tobool(${condition.toBoogieString})) { goto label$trueTarget; } goto label$falseTarget;"

  override def subst(v: Variable, w: Variable): Stmt = this.copy(condition = condition.subst(v,w))
  def fold(old: Expr, sub: Expr): Stmt = this.copy(condition = condition.fold(old, sub))
}
