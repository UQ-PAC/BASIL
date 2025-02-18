package bap

import ir._

/** Expression
  */
trait BAPExpr {
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  val size: Int
}

/** Concatenation of two bitvectors
  */
case class BAPConcat(left: BAPExpr, right: BAPExpr) extends BAPExpr {
  override val size: Int = left.size + right.size
}

/** Signed extend - extend in BIL
  */

case class BAPSignedExtend(width: Int, body: BAPExpr) extends BAPExpr {
  override val size: Int = width
}

/** Unsigned extend - pad in BIL
  */

case class BAPUnsignedExtend(width: Int, body: BAPExpr) extends BAPExpr {
  override val size: Int = width
}

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class BAPExtract(high: Int, low: Int, body: BAPExpr) extends BAPExpr {
  override def toString: String = String.format("%s[%d:%d]", body, high, low)

  // + 1 as extracts are inclusive (e.g. [31:0] has 32 bits)
  override val size: Int = high - low + 1
}

case object BAPHighCast {
  def apply(width: Int, body: BAPExpr): BAPExtract = BAPExtract(body.size - 1, body.size - width, body)
}

case object BAPLowCast {
  def apply(width: Int, body: BAPExpr): BAPExtract = BAPExtract(width - 1, 0, body)
}

/** Literal expression (e.g. 4, 5, 10)
  */
case class BAPLiteral(value: BigInt, size: Int) extends BAPExpr {

  /** Value of literal */
  override def toString: String = s"${value}bv$size"
}

/** Unary operator
  */
case class BAPUnOp(operator: BAPUnOperator, exp: BAPExpr) extends BAPExpr {
  override val size: Int = exp.size
}

sealed trait BAPUnOperator(op: String) {
  override def toString: String = op
}

case object NOT extends BAPUnOperator("NOT")
case object NEG extends BAPUnOperator("NEG")

object BAPUnOperator {
  def apply(op: String): BAPUnOperator = op match {
    case "NOT" => NOT
    case "NEG" => NEG
  }
}

/** Binary operation of two expressions
  */
case class BAPBinOp(operator: BAPBinOperator, lhs: BAPExpr, rhs: BAPExpr) extends BAPExpr {
  override val size: Int = operator match {
    case EQ | NEQ | LT | LE | SLT | SLE => 1
    case _ => lhs.size
  }
}

sealed trait BAPBinOperator(op: String) {
  override def toString: String = op
}

object BAPBinOperator {
  def apply(op: String): BAPBinOperator = op match {
    case "PLUS" => PLUS
    case "MINUS" => MINUS
    case "TIMES" => TIMES
    case "DIVIDE" => DIVIDE
    case "SDIVIDE" => SDIVIDE
    case "MOD" => MOD
    case "SMOD" => SMOD
    case "LSHIFT" => LSHIFT
    case "RSHIFT" => RSHIFT
    case "ARSHIFT" => ARSHIFT
    case "AND" => AND
    case "OR" => OR
    case "XOR" => XOR
    case "EQ" => EQ
    case "NEQ" => NEQ
    case "LT" => LT
    case "LE" => LE
    case "SLT" => SLT
    case "SLE" => SLE
  }
}

case object PLUS extends BAPBinOperator("PLUS")
case object MINUS extends BAPBinOperator("MINUS")
case object TIMES extends BAPBinOperator("TIMES")
case object DIVIDE extends BAPBinOperator("DIVIDE")
case object SDIVIDE extends BAPBinOperator("SDIVIDE")
case object MOD extends BAPBinOperator("MOD")
case object SMOD extends BAPBinOperator("SMOD")
case object LSHIFT extends BAPBinOperator("LSHIFT")
case object RSHIFT extends BAPBinOperator("RSHIFT")
case object ARSHIFT extends BAPBinOperator("ARSHIFT")
case object AND extends BAPBinOperator("AND")
case object OR extends BAPBinOperator("OR")
case object XOR extends BAPBinOperator("XOR")
case object EQ extends BAPBinOperator("EQ")
case object NEQ extends BAPBinOperator("NEQ")
case object LT extends BAPBinOperator("LT")
case object LE extends BAPBinOperator("LE")
case object SLT extends BAPBinOperator("SLT")
case object SLE extends BAPBinOperator("SLE")

trait BAPVar extends BAPExpr {
  val name: String
  override val size: Int
  override def toString: String = name
}
case class BAPRegister(override val name: String, override val size: Int) extends BAPVar

case class BAPLocalVar(override val name: String, override val size: Int) extends BAPVar

/** A load from memory at location exp
  */
case class BAPMemAccess(memory: BAPMemory, index: BAPExpr, endian: Endian, override val size: Int) extends BAPExpr {
  override def toString: String = s"${memory.name}[$index]"
}

case class BAPMemory(name: String, addressSize: Int, valueSize: Int)

case class BAPStore(memory: BAPMemory, index: BAPExpr, value: BAPExpr, endian: Endian, size: Int) extends BAPExpr {
  override def toString: String = s"${memory.name}[$index] := $value"
}
