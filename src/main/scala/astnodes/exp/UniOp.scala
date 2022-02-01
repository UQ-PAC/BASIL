package astnodes.exp

import astnodes.exp.`var`.Var

import java.util.Collections
import java.util
import java.util.Objects

/** Unary operator fact
  */
case class UniOp(var operator: UniOperator.Value, val exp: Expr) extends Expr {
  def this(operatorStr: String, exp: Expr) = this(UniOperator.fromBil(operatorStr), exp)
  override def toString = String.format("%s %s", operator, exp)
  override def toBoogieString: String = s"${UniOperator.toBoogie(operator, size)}(${exp.toBoogieString})"
  override def subst(v: Var, w: Var): Expr = this.copy(exp = exp.subst(v, w))

  override def vars = exp.vars

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

  def toBoogie(value: Value, size: Option[Int]): String = {
    val size1 = size.getOrElse(64)
    value match {
      case UnaryNegation => s"bv${size1}not" 
      case BitwiseComplement => s"bv${size1}neg"
    }
  }

  def changesSize(op: Value): Boolean = false
}
