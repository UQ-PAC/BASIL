package astnodes.exp

import astnodes.exp.`var`.Var
import util.AssumptionViolationException

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import analysis.tools.SimplificationUtil

/** Binary operation of two expressions
 */
case class BinOp(
    operator: BinOperator.Value,
    firstExp: Expr,
    secondExp: Expr
) extends Expr {

  override def toString = String.format("(%s) %s (%s)", firstExp, operator, secondExp)
  override def toBoogieString = BinOperator.toBoogie(operator, inputSize).fold(s"${firstExp.toBoogieString}, ${secondExp.toBoogieString}")((inner, fun) => s"$fun($inner)")

  override def subst(v: Var, w: Var): Expr = {
    this.copy(firstExp = firstExp.subst(v,w), secondExp = secondExp.subst(v, w))
  }

  override def fold(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.binArithmetic(this.copy(firstExp = firstExp.fold(old,sub), secondExp = secondExp.fold(old, sub)))
  }

  override def vars = firstExp.vars ++ secondExp.vars

  // Finish resolveTypes and then remove thsi
  override def size = BinOperator.size(operator, inputSize)

  def inputSize: Option[Int] = (firstExp.size, secondExp.size) match {
    case (Some(a: Int), Some(b: Int)) if (a == b) => Some(a)
    case (Some(a: Int), Some(b: Int)) if (a != b) => throw new AssumptionViolationException(s"Both sides of binop should have the same size $firstExp: ${firstExp.size}, $secondExp: ${secondExp.size}")
    case (Some(x: Int), None) => Some(x)
    case (None, Some(x: Int)) => Some(x)
    case _ => None
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

  /*
  Using the ADT could make it easier to differentiate between signed and unsigned operators,
  if we need it.

  Currently it does not differentiate between signed and unsigned operators
  */
  def fromAdt(adtStr: String) : Value = adtStr match {
    case "PLUS"    => Addition
    case "MINUS"   => Subtraction
    case "TIMES"   => Multiplication
    case "DIVIDE"  => Division
    case "SDIVIDE" => Division
    case "MOD"     => Modulo
    case "SMOD"    => Modulo
    case "LSHIFT"  => LogicalShiftLeft
    case "RSHIFT"  => LogicalShiftRight
    case "ARSHIFT" => ArithmeticShiftRight
    case "AND"     => BitwiseAnd
    case "OR"      => BitwiseOr
    case "XOR"     => BitwiseXor
    case "EQ"      => Equality
    case "NEQ"     => NonEquality
    case "LT"      => LessThan
    case "LE"      => LessThanOrEqual
    case "SLT"     => LessThan
    case "SLE"     => LessThanOrEqual
  }

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
    val size1 = size.getOrElse("64")
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
