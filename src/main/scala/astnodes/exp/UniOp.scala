package astnodes.exp

import analysis.tools.SimplificationUtil
import astnodes.exp.variable.Variable

/** Unary operator fact
  */
case class UniOp(var operator: UniOperator.Value, exp: Expr) extends Expr {
  override def toString: String = String.format("%s %s", operator, exp)
  override def toBoogieString: String = s"${UniOperator.toBoogie(operator, size)}(${exp.toBoogieString})"
  override def subst(v: Variable, w: Variable): Expr = this.copy(exp = exp.subst(v, w))
  override def fold (old: Expr, sub: Expr): Expr = SimplificationUtil.uniArithmetic(this.copy(exp = exp.fold(old, sub)))

  override def vars: List[Variable] = exp.vars

  override def size: Option[Int] = exp.size

}

case object UniOperator extends Enumeration {
  type Operator = Value
  val UnaryNegation: Operator = Value("!")
  val BitwiseComplement: Operator = Value("~") // todo

  def fromBil(bilStr: String): Value = bilStr match {
    case "-" => UnaryNegation
    case "~" => BitwiseComplement
  }

  def fromAdt(bilStr: String): Value = bilStr match {
    case "NEG" => UnaryNegation
    case "NOT" => BitwiseComplement
  }

  def toBoogie(value: Value, size: Option[Int]): String = {
    val size1 = size.getOrElse(64)
    value match {
      case UnaryNegation => s"bv${size1}not" 
      case BitwiseComplement => s"bv${size1}neg"
    }
  }

  def changesSize(op: Value): Boolean = false
}
