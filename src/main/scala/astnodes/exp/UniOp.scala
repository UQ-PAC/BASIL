package astnodes.exp

import java.util.Collections
import java.util
import java.util.Objects

/** Unary operator fact
  */
case class UniOp(var operator: UniOperator.Value, var exp: Expr) extends Expr {
  def this(operatorStr: String, exp: Expr) = this(UniOperator.fromBil(operatorStr), exp)
  override def toString = String.format("%s %s", operator, exp)
  override def toBoogieString: String = s"${UniOperator.toBoogie(operator, size)}(${exp.toBoogieString})"
  override def getChildren = Collections.singletonList(exp)
  override def replace(oldExp: Expr, newExp: Expr) = if (exp == oldExp) exp = newExp

  override def vars: List[Var] = exp.vars

  override def size = exp.size

}

case object UniOperator extends Enumeration {
  type Operator = Value
  val UnaryNegation: Operator = Value("!")
  val BitwiseComplement: Operator = Value("~") // todo

  def fromBil(bilStr: String): Value = bilStr match {
    case "-" => UnaryNegation
    case "~" => BitwiseComplement
  }

  def toBoogie(value: Value, size: Option[Int]): String =
    val size1 = size.getOrElse(64) // TODO
    value match {
    // TODO !!!!!
    case UnaryNegation => s"bv${size1}not" // TODO this is unarynecation right?
    case BitwiseComplement => s"bv${size1}neg"
  }
}
