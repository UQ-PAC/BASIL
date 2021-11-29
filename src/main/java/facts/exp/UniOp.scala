package facts.exp

import java.util.Collections
import java.util
import java.util.Objects

/** Unary operator fact
  */
class UniOp(var operator: String, var exp: Expr) extends Expr {
  override def toString = String.format("%s %s", operator, exp)
  override def getChildren = Collections.singletonList(exp)
  override def replace(oldExp: Expr, newExp: Expr) = if (exp == oldExp) exp = newExp

  override def vars: List[Var] = exp.vars
}
