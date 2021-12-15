package astnodes.exp

import util.{AssumptionViolationException}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/** Binary operation fact
  */
case class BinOp(
    operator: BinOperator.Value,
    var firstExp: Expr,
    var secondExp: Expr
) extends Expr {
  def this(operatorStr: String, firstExp: Expr, secondExp: Expr) = this(BinOperator.fromBil(operatorStr), firstExp, secondExp)
  override def toString = String.format("(%s) %s (%s)", firstExp, operator, secondExp)
  override def toBoogieString = BinOperator.toBoogie(operator, inputSize).fold(s"${firstExp.toBoogieString}, ${secondExp.toBoogieString}")((inner, fun) => s"$fun($inner)")
  //s"${BinOperator.toBoogie(operator, size)}(${firstExp.toBoogieString}, ${secondExp.toBoogieString})"
  override def getChildren = ArrayBuffer(firstExp, secondExp).asJava

  // TODO update so the member vars can be vals
  override def replace(oldExp: Expr, newExp: Expr) = {
    if (firstExp == oldExp) firstExp = newExp
    if (secondExp == oldExp) secondExp = newExp
  }

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

  // TODO getOrElse ??
  def toBoogie(value: Value, size: Option[Int]): List[String] = {
    val size1 = size.getOrElse(64)
    value match {
      case Addition => List(s"bv${size1}add")
      case Subtraction => List(s"bv${size1}sub")
      case Multiplication => List(s"bv${size1}mul")
      case Division => List(s"bv${size1}udiv")
      case Modulo => List(s"bv${size1}mod")
      case BitwiseAnd => List(s"bv${size1}and")
      case BitwiseOr => List(s"bv${size1}or")
      case BitwiseXor => List(s"bv${size1}xor")
      case Equality => List(s"bv${size1}comp", "booltobv1")
      case NonEquality => List(s"bv${size1}comp", "!", "booltobv1")
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
