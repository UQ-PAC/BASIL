package bap

import ir._

/** Expression
  */
trait BAPExpr {
  def toIR: Expr

  /* def toGamma: Expr = {
    val gammaVars: Set[Expr] = gammas.map(_.toGamma)
    if (gammaVars.isEmpty) {
      TrueLiteral
    } else if (gammaVars.size == 1) {
      gammaVars.head
    } else {
      gammaVars.tail.foldLeft(gammaVars.head) { (join: Expr, next: Expr) =>
        BinaryExpr(BoolAND, next, join)
      }
    }
  }
  */
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  val size: Int
  //def gammas: Set[BAPVariable]

  def locals: Set[BAPLocalVar]
}

/** Concatenation of two bitvectors
  */
case class BAPConcat(left: BAPExpr, right: BAPExpr) extends BAPExpr {
  def toIR: BinaryExpr = BinaryExpr(BVCONCAT, left.toIR, right.toIR)

  override val size: Int = left.size + right.size
  //override def gammas: Set[BAPVariable] = left.gammas ++ right.gammas
  override def locals: Set[BAPLocalVar] = left.locals ++ right.locals
}

/** Signed extend - extend in BIL
  */

case class BAPSignedExtend(width: Int, body: BAPExpr) extends BAPExpr {
  override def locals: Set[BAPLocalVar] = body.locals
  override val size: Int = width

  override def toIR: Expr = {
    if (width > body.size) {
      SignExtend(width - body.size, body.toIR)
    } else {
      BAPExtract(width - 1, 0, body).toIR
    }
  }
  //override def gammas: Set[BAPVariable] = body.gammas
}

/** Unsigned extend - pad in BIL
  */

case class BAPUnsignedExtend(width: Int, body: BAPExpr) extends BAPExpr {
  override def locals: Set[BAPLocalVar] = body.locals

  override val size: Int = width
  
  override def toIR: Expr = {
    if (width > body.size) {
      ZeroExtend(width - body.size, body.toIR)
    } else {
      BAPExtract(width - 1, 0, body).toIR
    }

  }
  //override def gammas: Set[BAPVariable] = body.gammas
}

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class BAPExtract(high: Int, low: Int, body: BAPExpr) extends BAPExpr {
  override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[BAPLocalVar] = body.locals

  // + 1 as extracts are inclusive (e.g. [31:0] has 32 bits)
  override val size: Int = high - low + 1
  
  override def toIR: Expr = {
    val bodySize = body.size
    if (size > bodySize) {
      if (low == 0) {
        ZeroExtend(size - bodySize, body.toIR)
      } else {
        Extract(high + 1, low, ZeroExtend(size - bodySize, body.toIR))
      }
    } else {
      Extract(high + 1, low, body.toIR)
    }
  }
  //override def gammas: Set[BAPVariable] = body.gammas
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

  override def locals: Set[BAPLocalVar] = Set()
  override def toIR: BitVecLiteral = BitVecLiteral(value, size)
  //override def gammas: Set[BAPVariable] = Set()
}

/** Unary operator
  */
case class BAPUnOp(operator: BAPUnOperator, exp: BAPExpr) extends BAPExpr {
  override def locals: Set[BAPLocalVar] = exp.locals
  override val size: Int = exp.size
  
  override def toIR: UnaryExpr = operator match {
    case NOT => UnaryExpr(BVNOT, exp.toIR)
    case NEG => UnaryExpr(BVNEG, exp.toIR)
  }
  //override def gammas: Set[BAPVariable] = exp.gammas
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
  override def locals: Set[BAPLocalVar] = lhs.locals ++ rhs.locals
  //override def gammas: Set[BAPVariable] = lhs.gammas ++ rhs.gammas

  override val size: Int = operator match {
    case EQ | NEQ | LT | LE | SLT | SLE => 1
    case _                              => lhs.size
  }
  
  override def toIR: Expr = operator match {
    case PLUS    => BinaryExpr(BVADD, lhs.toIR, rhs.toIR)
    case MINUS   => BinaryExpr(BVSUB, lhs.toIR, rhs.toIR)
    case TIMES   => BinaryExpr(BVMUL, lhs.toIR, rhs.toIR)
    case DIVIDE  => BinaryExpr(BVUDIV, lhs.toIR, rhs.toIR)
    case SDIVIDE => BinaryExpr(BVSDIV, lhs.toIR, rhs.toIR)
    // counterintuitive but correct according to BAP source
    case MOD => BinaryExpr(BVSREM, lhs.toIR, rhs.toIR)
    // counterintuitive but correct according to BAP source
    case SMOD => BinaryExpr(BVUREM, lhs.toIR, rhs.toIR)
    case LSHIFT => // BAP says caring about this case is necessary?
      if (lhs.size == rhs.size) {
        BinaryExpr(BVSHL, lhs.toIR, rhs.toIR)
      } else {
        BinaryExpr(BVSHL, lhs.toIR, ZeroExtend(lhs.size - rhs.size, rhs.toIR))
      }
    case RSHIFT =>
      if (lhs.size == rhs.size) {
        BinaryExpr(BVLSHR, lhs.toIR, rhs.toIR)
      } else {
        BinaryExpr(BVLSHR, lhs.toIR, ZeroExtend(lhs.size - rhs.size, rhs.toIR))
      }
    case ARSHIFT =>
      if (lhs.size == rhs.size) {
        BinaryExpr(BVASHR, lhs.toIR, rhs.toIR)
      } else {
        BinaryExpr(BVASHR, lhs.toIR, ZeroExtend(lhs.size - rhs.size, rhs.toIR))
      }
    case AND => BinaryExpr(BVAND, lhs.toIR, rhs.toIR)
    case OR  => BinaryExpr(BVOR, lhs.toIR, rhs.toIR)
    case XOR => BinaryExpr(BVXOR, lhs.toIR, rhs.toIR)
    case EQ  => BinaryExpr(BVCOMP, lhs.toIR, rhs.toIR)
    case NEQ => UnaryExpr(BVNOT, BinaryExpr(BVCOMP, lhs.toIR, rhs.toIR))
    case LT  => BinaryExpr(BVULT, lhs.toIR, rhs.toIR)
    case LE  => BinaryExpr(BVULE, lhs.toIR, rhs.toIR)
    case SLT => BinaryExpr(BVSLT, lhs.toIR, rhs.toIR)
    case SLE => BinaryExpr(BVSLE, lhs.toIR, rhs.toIR)
  }
  
}

sealed trait BAPBinOperator(op: String) {
  override def toString: String = op
}

object BAPBinOperator {
  def apply(op: String): BAPBinOperator = op match {
    case "PLUS"    => PLUS
    case "MINUS"   => MINUS
    case "TIMES"   => TIMES
    case "DIVIDE"  => DIVIDE
    case "SDIVIDE" => SDIVIDE
    case "MOD"     => MOD
    case "SMOD"    => SMOD
    case "LSHIFT"  => LSHIFT
    case "RSHIFT"  => RSHIFT
    case "ARSHIFT" => ARSHIFT
    case "AND"     => AND
    case "OR"      => OR
    case "XOR"     => XOR
    case "EQ"      => EQ
    case "NEQ"     => NEQ
    case "LT"      => LT
    case "LE"      => LE
    case "SLT"     => SLT
    case "SLE"     => SLE
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

trait BAPVariable extends BAPExpr

case class BAPLocalVar(name: String, override val size: Int) extends BAPVariable {
  override def toString: String = name
  override def toIR: Variable = Variable(s"$name", BitVecType(size))

  override def locals: Set[BAPLocalVar] = Set(this)
  /*
override def gammas: Set[BAPVariable] = Set(this)
override def toGamma: BVar = BVariable(s"Gamma_$name", BoolType, Scope.Local)
override def toIR: BVar = BVariable(s"$name", BitVecType(size), Scope.Local)
*/
}

/** A load from memory at location exp
  */
case class BAPMemAccess(memory: BAPMemory, index: BAPExpr, endian: Endian, override val size: Int) extends BAPVariable {
  override def toString: String = s"${memory.name}[$index]"
  override def locals: Set[BAPLocalVar] = index.locals
  override def toIR: MemoryLoad = MemoryLoad(memory.toIR, index.toIR, endian, size)
  /*

override def gammas: Set[BAPVariable] = Set(this)
override def toGamma: Expr = if (memory.name == "stack") {
  GammaLoad(memory.toGamma, index.toIR, size, size / memory.valueSize)
} else {
  BinaryExpr(
    BoolOR,
    GammaLoad(memory.toGamma, index.toIR, size, size / memory.valueSize),
    L(memory.toIR, index.toIR)
  )
}
*/
}

object BAPMemAccess {
  // initialise to replace stack references
  def init(memory: BAPMemory, index: BAPExpr, endian: Endian, size: Int): BAPMemAccess = {
    if (index.locals.contains(BAPLocalVar("R31", 64))) {
      BAPMemAccess(memory.copy(name = "stack"), index, endian, size)
    } else {
      BAPMemAccess(memory, index, endian, size)
    }
  }
}

case class BAPMemory(name: String, addressSize: Int, valueSize: Int) extends BAPVariable {
  override val size: Int = valueSize // should reconsider
  override def toIR: Memory = Memory(name, addressSize, valueSize)
  override def locals: Set[BAPLocalVar] = Set()
  /*
override def gammas: Set[BAPVariable] = Set()
override def toGamma: BMapVar = BMapVar(s"Gamma_$name", MapType(BitVec(addressSize), BoolType), Scope.Global)
*/
}

case class BAPStore(memory: BAPMemory, index: BAPExpr, value: BAPExpr, endian: Endian, size: Int) extends BAPExpr {
  override def locals: Set[BAPLocalVar] = index.locals ++ value.locals
  override def toIR: MemoryStore = MemoryStore(memory.toIR, index.toIR, value.toIR, endian, size)
  /*
  override def gammas: Set[BAPVariable] = Set()
  override def toIR: BMemoryStore = BMemoryStore(memory.toIR, index.toIR, value.toIR, endian, size)
  override def toGamma: GammaStore =
    GammaStore(memory.toGamma, index.toIR, value.toGamma, size, size / memory.valueSize)
  */
}

object BAPStore {
  // initialise to replace stack references
  def init(memory: BAPMemory, index: BAPExpr, value: BAPExpr, endian: Endian, size: Int): BAPStore = {
    if (index.locals.contains(BAPLocalVar("R31", 64))) {
      BAPStore(memory.copy(name = "stack"), index, value, endian, size)
    } else {
      BAPStore(memory, index, value, endian, size)
    }
  }
}