package astnodes.exp

import jdk.javadoc.doclet.DocletEnvironment.ModuleMode
import java.util
import java.util.Objects

/** Binary operation fact
  */
case class BinOp(
    operator: BinOperator.Value,
    var firstExp: Expr,
    var secondExp: Expr
) extends Expr {
  def this(operatorStr: String, firstExp: Expr, secondExp: Expr) = this(BinOperator.fromBil(operatorStr), firstExp, secondExp)
  def getOp(): String = {
    return operator.toString
  }
  
  // TODO: this can be simplified a lot, but atm it seems to work allg
  def compute(): Double = {
    if (firstExp.isInstanceOf[Literal] && secondExp.isInstanceOf[Literal]) {
      val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      return performArithmetic(firstOperand, secondOperand, operator.toString)
    } else if (firstExp.isInstanceOf[Literal] && secondExp.isInstanceOf[BinOp]) {
      val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      val secondOperand = secondExp.asInstanceOf[BinOp].compute()
      return performArithmetic(firstOperand, secondOperand, operator.toString)
    } else if (firstExp.isInstanceOf[BinOp] && secondExp.isInstanceOf[Literal]) {
      val firstOperand = firstExp.asInstanceOf[BinOp].compute()
      val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      return performArithmetic(firstOperand, secondOperand, operator.toString)
    } else {
      val firstOperand = firstExp.asInstanceOf[BinOp].compute()
      val secondOperand = secondExp.asInstanceOf[BinOp].compute()
      return performArithmetic(firstOperand, secondOperand, operator.toString)
    }
  }
  
  def canCompute(): Boolean = {
    if (firstExp.isInstanceOf[Literal]) {
      try {
        val firstOperand = firstExp.asInstanceOf[Literal].toString.toDouble
      } catch {
        case ex: NumberFormatException => return false
      }
    } else if (firstExp.isInstanceOf[Var] || firstExp.isInstanceOf[Extract]) {
      return false
    } else if (!firstExp.asInstanceOf[BinOp].canCompute()) {
      return false
    }
    
    if (secondExp.isInstanceOf[Literal]) {
      try {
        val secondOperand = secondExp.asInstanceOf[Literal].toString.toDouble
      } catch {
        case ex: NumberFormatException => return false
      }
    } else if (secondExp.isInstanceOf[Var] || secondExp.isInstanceOf[Extract]) {
      return false
    } else if (!secondExp.asInstanceOf[BinOp].canCompute()) {
      return false
    }
    
    return true
  }
  
  /**
    * Helper method for compute()
   */
  private def performArithmetic(firstOperand : Double, secondOperand : Double, operator : String)
    : Double = {
    var result : Double = 0.0
    operator match {
      case "+" => result = firstOperand + secondOperand
      case "-" => result = firstOperand - secondOperand
      case "*" => result = firstOperand * secondOperand
      case "/" => result = firstOperand / secondOperand
      case "%" => result = firstOperand % secondOperand
      case _ => 
    }
    return result
  }
  
  override def toString = String.format("(%s) %s (%s)", firstExp, operator, secondExp)
  override def toBoogieString = s"${BinOperator.toBoogie(operator)}(${firstExp.toBoogieString}, ${secondExp.toBoogieString})"
  override def getChildren = util.Arrays.asList(firstExp, secondExp)

  // TODO update so the member vars can be vals
  override def replace(oldExp: Expr, newExp: Expr) = {
    if (firstExp == oldExp) firstExp = newExp
    if (secondExp == oldExp) secondExp = newExp
  }

  override def vars = firstExp.vars ++ secondExp.vars
}

// TODO look at scala 3 enums
case object BinOperator extends Enumeration {
  type Operator = Value
  // arithmetic operators
  val Addition: Operator = Value("+")
  val Subtraction: Operator = Value("-")
  val Multiplication: Operator = Value("*")
  val Division: Operator = Value("/")
  val Modulo: Operator = Value("%")
  val BitwiseAnd: Operator = Value("&") // todo
  val BitwiseOr: Operator = Value("|") // todo
  val BitwiseXor: Operator = Value("xor") // todo
  val LogicalShiftLeft: Operator = Value("<<") // todo
  val LogicalShiftRight: Operator = Value(">>") // todo
  val ArithmeticShiftRight: Operator = Value(">>>") // todo
  // logical operators
  val Equality: Operator = Value("==")
  val NonEquality: Operator = Value("!=")
  val LessThan: Operator = Value("<")
  val LessThanOrEqual: Operator = Value("<=")

  def fromBil(bilStr: String): Value = bilStr match {
    // note at the moment we do not distinguish between signed and unsigned
    // arithmetic operators
    case "+" => Addition
    case "-" => Subtraction
    case "*" => Multiplication
    case "/" => Division
    case "%" => Modulo
    case "&" => BitwiseAnd
    case "|" => BitwiseOr
    case "xor" => BitwiseXor
    case "<<" => LogicalShiftLeft
    case ">>" => LogicalShiftRight
    case ">>>" => ArithmeticShiftRight
    // logical operators
    case "=" => Equality
    case "<>" => NonEquality
    case "<" => LessThan
    case "<=" => LessThanOrEqual
  }

  def toBoogie(value: Value): String = value match {
    case Addition => "bv64add"
    case Subtraction => "bv64sub"
    case Multiplication => "bv64mul"
    case Division => "bv64udiv"
    case Modulo => "bv64mod"
    case BitwiseAnd => "bv64and"
    case BitwiseOr => "bv64or"
    case BitwiseXor => "bv64xor"
    case Equality => "bv64comp"
    // TODO !!!!!!!!!!! case NonEquality => ??? // TODO need to do this as !(a = b) i think
    case NonEquality => "bv64comp"
  }
}
