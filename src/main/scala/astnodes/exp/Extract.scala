package astnodes.exp

import java.util.Collections
import java.util
import java.util.Objects

class Extract(firstInt: Int, secondInt: Int, var variable: Expr) extends Expr {
  override def toString = String.format("%s[%d:%d]", variable, firstInt, secondInt)
  override def getChildren = Collections.singletonList(variable)
  override def replace(oldExp: Expr, newExp: Expr) = if (variable == oldExp) variable = newExp
  override def vars: List[Var] = variable.vars
}
