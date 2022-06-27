package astnodes

//import analysis.tools.SimplificationUtil
import boogie._
import util.AssumptionViolationException

/** Expression
  */
trait Expr {

  /* Substitute a given variable for another variable */
  def subst(v: Variable, w: Variable): Expr

  def toBoogie: BExpr
  //def toBoogieString: String = toString

  //def simplify(old: Expr, sub: Expr): Expr
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Int

  def locals: Set[LocalVar]
}

/** Concatenation of two bitvectors
  */
case class Concat(left: Expr, right: Expr) extends Expr {
  override def toBoogie: BinaryBExpr = BinaryBExpr(BVCONCAT, left.toBoogie, right.toBoogie)

  override def size: Int = left.size + right.size

  override def locals: Set[LocalVar] = left.locals ++ right.locals
  override def subst(v: Variable, w: Variable): Expr = {
    copy(left = left.subst(v, w), right = right.subst(v, w))
  }

  /*
  override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.bitvecConcat(copy(left = left.simplify(old,sub), right = right.simplify(old,sub)))
  }
   */
}

/** Signed extend - extend in BIL
  */

case class SignedExtend(width: Int, body: Expr) extends Expr {
  //override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals
  override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def size: Int = width

  override def toBoogie: BExpr = {
    val extend = width - body.size
    if (extend == 0) {
      body.toBoogie
    } else {
      BVSignExtend(extend, body.toBoogie)
    }
  }
}

/** Unsigned extend - pad in BIL
  */

case class UnsignedExtend(width: Int, body: Expr) extends Expr {
  //override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals
  override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def size: Int = width

  override def toBoogie: BExpr = {
    val extend = width - body.size
    if (extend == 0) {
      body.toBoogie
    } else {
      BVZeroExtend(extend, body.toBoogie)
    }
  }
}

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class Extract(high: Int, low: Int, body: Expr) extends Expr {
  override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals

  override def subst(v: Variable, w: Variable): Expr = this.copy(body = body.subst(v, w))

  /*
  override def simplify(old: Expr, sub: Expr): Expr =
    SimplificationUtil.bitvecExtract(copy(body = body.simplify(old, sub)))
   */
  // + 1 as extracts are inclusive (e.g. [31:0] has 32 bits)
  override def size: Int = high - low + 1

  override def toBoogie: BVExtract = {
    val boogieBody = body.toBoogie
    boogieBody match {
      case extract: BVExtract => BVExtract(high + 1 + extract.start, low + extract.start, extract.body)
      case _                  => BVExtract(high + 1, low, body.toBoogie)
    }
  }
}

case object HighCast {
  def apply(width: Int, body: Expr): Extract = Extract(body.size - 1, body.size - width, body)
}

case object LowCast {
  def apply(width: Int, body: Expr): Extract = Extract(width - 1, 0, body)
}

/** Literal expression (e.g. 4, 5, 10)
  */
case class Literal(value: BigInt, size: Int) extends Expr {

  /** Value of literal */
  override def toString: String = s"${value}bv$size"

  override def locals: Set[LocalVar] = Set()
  override def subst(v: Variable, w: Variable): Expr = this
  //override def simplify(old: Expr, sub: Expr): Expr = this

  override def toBoogie: BitVecLiteral = BitVecLiteral(value, size)
}

/** Unary operator
  */
case class UnOp(operator: UnOperator, exp: Expr) extends Expr {
  override def subst(v: Variable, w: Variable): Expr = copy(exp = exp.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = SimplificationUtil.uniArithmetic(copy(exp = exp.simplify(old, sub)))

  override def locals: Set[LocalVar] = exp.locals
  override def size: Int = exp.size

  override def toBoogie: UnaryBExpr = operator match {
    case NOT => UnaryBExpr(BVNOT, exp.toBoogie)
    case NEG => UnaryBExpr(BVNEG, exp.toBoogie)
  }
}

sealed trait UnOperator(op: String) {
  override def toString: String = op
}

case object NOT extends UnOperator("NOT")
case object NEG extends UnOperator("NEG")

object UnOperator {
  def apply(op: String): UnOperator = op match {
    case "NOT" => NOT
    case "NEG" => NEG
  }
}

/** Binary operation of two expressions
  */
case class BinOp(operator: BinOperator, lhs: Expr, rhs: Expr) extends Expr {
  override def subst(v: Variable, w: Variable): Expr = {
    copy(lhs = lhs.subst(v, w), rhs = rhs.subst(v, w))
  }

  /* override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.binArithmetic(copy(lhs = lhs.simplify(old,sub), rhs = rhs.simplify(old, sub)))
  } */

  override def locals: Set[LocalVar] = lhs.locals ++ rhs.locals

  override def size: Int = operator match {
    case EQ | NEQ | LT | LE | SLT | SLE => 1
    case _                              => lhs.size
  }

  override def toBoogie: BExpr = operator match {
    case PLUS    => BinaryBExpr(BVADD, lhs.toBoogie, rhs.toBoogie)
    case MINUS   => BinaryBExpr(BVSUB, lhs.toBoogie, rhs.toBoogie)
    case TIMES   => BinaryBExpr(BVMUL, lhs.toBoogie, rhs.toBoogie)
    case DIVIDE  => BinaryBExpr(BVUDIV, lhs.toBoogie, rhs.toBoogie)
    case SDIVIDE => BinaryBExpr(BVUDIV, lhs.toBoogie, rhs.toBoogie)
    // counterintuitive but correct according to BAP source
    case MOD => BinaryBExpr(BVSREM, lhs.toBoogie, rhs.toBoogie)
    // counterintuitive but correct according to BAP source
    case SMOD => BinaryBExpr(BVUREM, lhs.toBoogie, rhs.toBoogie)
    case LSHIFT => // BAP says caring about this case is necessary?
      if (lhs.size == rhs.size) {
        BinaryBExpr(BVSHL, lhs.toBoogie, rhs.toBoogie)
      } else {
        BinaryBExpr(BVSHL, lhs.toBoogie, BVZeroExtend(lhs.size - rhs.size, rhs.toBoogie))
      }
    case RSHIFT =>
      if (lhs.size == rhs.size) {
        BinaryBExpr(BVLSHR, lhs.toBoogie, rhs.toBoogie)
      } else {
        BinaryBExpr(BVLSHR, lhs.toBoogie, BVZeroExtend(lhs.size - rhs.size, rhs.toBoogie))
      }
    case ARSHIFT =>
      if (lhs.size == rhs.size) {
        BinaryBExpr(BVASHR, lhs.toBoogie, rhs.toBoogie)
      } else {
        BinaryBExpr(BVASHR, lhs.toBoogie, BVZeroExtend(lhs.size - rhs.size, rhs.toBoogie))
      }
    case AND => BinaryBExpr(BVAND, lhs.toBoogie, rhs.toBoogie)
    case OR  => BinaryBExpr(BVOR, lhs.toBoogie, rhs.toBoogie)
    case XOR => BinaryBExpr(BVXOR, lhs.toBoogie, rhs.toBoogie)
    case EQ  => BinaryBExpr(BVCOMP, lhs.toBoogie, rhs.toBoogie)
    case NEQ => UnaryBExpr(BVNOT, BinaryBExpr(BVCOMP, lhs.toBoogie, rhs.toBoogie))
    case LT  => BinaryBExpr(BVULT, lhs.toBoogie, rhs.toBoogie)
    case LE  => BinaryBExpr(BVULE, lhs.toBoogie, rhs.toBoogie)
    case SLT => BinaryBExpr(BVSLT, lhs.toBoogie, rhs.toBoogie)
    case SLE => BinaryBExpr(BVSLE, lhs.toBoogie, rhs.toBoogie)
  }
}

sealed trait BinOperator(op: String) {
  override def toString: String = op
}

object BinOperator {
  def apply(op: String): BinOperator = op match {
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

case object PLUS extends BinOperator("PLUS")
case object MINUS extends BinOperator("MINUS")
case object TIMES extends BinOperator("TIMES")
case object DIVIDE extends BinOperator("DIVIDE")
case object SDIVIDE extends BinOperator("SDIVIDE")
case object MOD extends BinOperator("MOD")
case object SMOD extends BinOperator("SMOD")
case object LSHIFT extends BinOperator("LSHIFT")
case object RSHIFT extends BinOperator("RSHIFT")
case object ARSHIFT extends BinOperator("ARSHIFT")
case object AND extends BinOperator("AND")
case object OR extends BinOperator("OR")
case object XOR extends BinOperator("XOR")
case object EQ extends BinOperator("EQ")
case object NEQ extends BinOperator("NEQ")
case object LT extends BinOperator("LT")
case object LE extends BinOperator("LE")
case object SLT extends BinOperator("SLT")
case object SLE extends BinOperator("SLE")

/** Variable
  *
  * Variables can be registers (e.g. R1, SP, #31) or loads from memory (e.g. mem[10])
  */
trait Variable extends Expr {
  override def subst(v: Variable, w: Variable): Variable = if (v == this) w else this
  //def toGamma: SecVar | SecMemLoad
}

/** A register
  */
case class LocalVar(name: String, override val size: Int) extends Variable {
  override def toString: String = name
  override def locals: Set[LocalVar] = Set(this)
  //override def toGamma: SecVar = SecVar(name, true)
  override def toBoogie: BVar = BVariable(name, BitVec(size), Scope.Local)
  //override def simplify(old: Expr, sub: Expr): Expr = if (old == this) sub else this
}

/** A load from memory at location exp
  */
case class MemAccess(memory: Memory, index: Expr, endian: Endian, override val size: Int) extends Variable {
  override def toString: String = s"${memory.name}[$index]"
  override def locals: Set[LocalVar] = index.locals

  /*
  override def simplify(old: Expr, sub: Expr): Expr = {
    if (!onStack) copy(index = index.simplify(old, sub))
    else if (onStack && old == this) sub
    else this
  }
   */

  //def toL: SecMemLoad = SecMemLoad(false, true, index)
  //override def toGamma: SecMemLoad = SecMemLoad(true, false, index)

  def boogieAccesses: Seq[MapAccess] = {
    val boogieMap = memory.toBoogie
    val boogieIndex = index.toBoogie
    val accesses = for (i <- 0 until (size / memory.valueSize)) yield {
      if (i == 0) {
        MapAccess(boogieMap, boogieIndex)
      } else {
        MapAccess(boogieMap, BinaryBExpr(BVADD, index.toBoogie, BitVecLiteral(i, memory.addressSize)))
      }
    }
    endian match {
      case Endian.BigEndian    => accesses.reverse
      case Endian.LittleEndian => accesses
    }
  }

  override def toBoogie: BExpr = {
    boogieAccesses.tail.foldLeft(boogieAccesses.head) { (concat: BExpr, next: MapAccess) =>
      BinaryBExpr(BVCONCAT, next, concat)
    }
  }
}

object MemAccess {
  // initialise to replace stack references
  def init(memory: Memory, index: Expr, endian: Endian, size: Int): MemAccess = {
    if (index.locals.contains(LocalVar("R31", 64))) {
      MemAccess(memory.copy(name = "stack"), index, endian, size)
    } else {
      MemAccess(memory, index, endian, size)
    }
  }
}

case class Memory(name: String, addressSize: Int, valueSize: Int) extends Expr {
  override def size: Int = valueSize
  override def subst(v: Variable, w: Variable): Expr = ???
  override def locals: Set[LocalVar] = Set()

  override def toBoogie: MapVar = MapVar(name, MapType(BitVec(addressSize), BitVec(valueSize)))
}
