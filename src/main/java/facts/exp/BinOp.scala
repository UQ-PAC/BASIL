package facts.exp

import java.util
import java.util.Objects

/** Binary operation fact
  */
case class BinOp(
    var operator: String,
    var firstExp: Expr,
    var secondExp: Expr
) extends Expr {
  override def toString = String.format("(%s) %s (%s)", firstExp, operator, secondExp)
  override def getChildren = util.Arrays.asList(firstExp, secondExp)

  // TODO update so the member vars can be vals
  override def replace(oldExp: Expr, newExp: Expr) = {
    if (firstExp == oldExp) firstExp = newExp
    if (secondExp == oldExp) secondExp = newExp
  }

  override def vars = firstExp.vars ++ secondExp.vars
}
