package astnodes.exp

import java.util.Collections
import java.util
import java.util.Objects

case class Extract(firstInt: Int, secondInt: Int, var variable: Expr) extends Expr {
  override def toString = String.format("%s[%d:%d]", variable, firstInt + 1, secondInt)
  override def getChildren = Collections.singletonList(variable)
  override def replace(oldExp: Expr, newExp: Expr) = if (variable == oldExp) variable = newExp
  override def vars: List[Var] = variable.vars

  override def size = Some(firstInt - secondInt + 1)  // + 1 as extracts are inclusive (e.g. 0:31 has 32 bits)
}
