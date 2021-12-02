package astnodes.exp

import java.util.Collections
import java.util
import java.util.Objects

/** Memory expression e.g. mem[10]
  */
class MemLoad(var exp: Expr) extends Expr {
  override def toString = String.format("mem[%s]", exp)
  override def getChildren = Collections.singletonList(exp)
  override def replace(oldExp: Expr, newExp: Expr) = if (exp == oldExp) exp = newExp

  override def vars: List[Var] = exp.vars
}
