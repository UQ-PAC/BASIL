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

  object Operator extends Enumeration {
    type Operator = Value
    val UnaryNegation: Operator = Value("!")
    val BitwiseComplement: Operator = Value("~") // todo

    def fromBil(bilStr: String): Value = bilStr match {
      case "-" => UnaryNegation
      case "~" => BitwiseComplement
    }
  }
}
