package astnodes.exp

import astnodes.exp.`var`.Var
import util.AssumptionViolationException

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Binary operation of two expressions
 */
case class BinOp(
    operator: BinOperator.Value,
    firstExp: Expr,
    secondExp: Expr
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
  override def toBoogieString = BinOperator.toBoogie(operator, inputSize).fold(s"${firstExp.toBoogieString}, ${secondExp.toBoogieString}")((inner, fun) => s"$fun($inner)")
  //s"${BinOperator.toBoogie(operator, size)}(${firstExp.toBoogieString}, ${secondExp.toBoogieString})"

  override def subst(v: Var, w: Var): Expr = this.copy(firstExp = firstExp.subst(v,w), secondExp = secondExp.subst(v, w))

  override def vars = firstExp.vars ++ secondExp.vars

  // Finish resolveTypes and then remove thsi
  override def size = BinOperator.size(operator, inputSize)

  def inputSize = (firstExp.size, secondExp.size) match {
    case (a: Some[Int], b: Some[int]) if (a == b) => a
    case (a: Some[Int], b: Some[int]) if (a != b) => throw new AssumptionViolationException(s"Both sides of binop should have the same size $firstExp: ${firstExp.size}, $secondExp: ${secondExp.size}")
    case (x: Some[Int], None) => x
    case (None, x: Some[Int]) => x
    case (None, None) => None
  }
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
  val BitwiseAnd: Operator = Value("&")
  val BitwiseOr: Operator = Value("|")
  val BitwiseXor: Operator = Value("xor")
  val LogicalShiftLeft: Operator = Value("<<")
  val LogicalShiftRight: Operator = Value(">>")
  val ArithmeticShiftRight: Operator = Value(">>>")
  // logical operators
  val Equality: Operator = Value("==")
  val NonEquality: Operator = Value("!=")
  val LessThan: Operator = Value("<")
  val LessThanOrEqual: Operator = Value("<=")
  val UnknownOperator: Operator = Value("???") // currently just ~>>

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
    case "~>>" => UnknownOperator
  }

  def toBoogie(value: Value, size: Option[Int]): List[String] = {
    val size1 = size.get
    value match {
      case Addition => List(s"bv${size1}add")
      case Subtraction => List(s"bv${size1}sub")
      case Multiplication => List(s"bv${size1}mul")
      case Division => List(s"bv${size1}udiv")
      case Modulo => List(s"bv${size1}mod")
      case BitwiseAnd => List(s"bv${size1}and")
      case BitwiseOr => List(s"bv${size1}or")
      case BitwiseXor => List(s"bv${size1}xor")
      case LogicalShiftLeft => List(s"bv${size1}shl")
      case LogicalShiftRight => List(s"bv${size1}lshr")
      case ArithmeticShiftRight => List(s"bv${size1}ashr")
      case Equality => List(s"bv${size1}eq", "booltobv1")
      case NonEquality => List(s"bv${size1}neq", "booltobv1")
    }
  }
  
  def changesSize(value: Value) = value match {
    case Equality | NonEquality => true
    case _ => false
  }

  def size(value: Value, size: Option[Int]): Option[Int] = value match {
    case Equality | NonEquality => Some(1)
    case _ => size
  }
}
