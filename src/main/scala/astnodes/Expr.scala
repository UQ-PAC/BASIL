package astnodes

//import analysis.tools.SimplificationUtil
import boogie._
import util.AssumptionViolationException

/** Expression
  */
trait Expr {

  /* Substitute a given variable for another variable */
  //def subst(v: Variable, w: Variable): Expr

  def toBoogie: BExpr
  def toGamma: BExpr = {
    val gammaVars: Set[BExpr] = gammas.map(_.toGamma)
    if (gammaVars.isEmpty) {
      TrueLiteral
    } else if (gammaVars.size == 1) {
      gammaVars.head
    } else {
      gammaVars.tail.foldLeft(gammaVars.head) {
        (join: BExpr, next: BExpr) => BinaryBExpr(BoolAND, next, join)
      }
    }
  }
  //def toBoogieString: String = toString

  //def simplify(old: Expr, sub: Expr): Expr
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Int
  def gammas: Set[Variable]

  def locals: Set[LocalVar]
}

/** Concatenation of two bitvectors
  */
case class Concat(left: Expr, right: Expr) extends Expr {
  override def toBoogie: BinaryBExpr = BinaryBExpr(BVCONCAT, left.toBoogie, right.toBoogie)

  override def size: Int = left.size + right.size
  override def gammas: Set[Variable] = left.gammas ++ right.gammas
  override def locals: Set[LocalVar] = left.locals ++ right.locals
  //override def subst(v: Variable, w: Variable): Expr = {
  //  copy(left = left.subst(v, w), right = right.subst(v, w))
  //}

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
  //override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def size: Int = width

  override def toBoogie: BExpr = {
    if (width > body.size) {
      BVSignExtend(width - body.size, body.toBoogie)
    } else {
      Extract(width - 1, 0, body).toBoogie
    }
    /*
    val extend = width - body.size
    if (extend == 0) {
      body.toBoogie
    } else {
      BVSignExtend(extend, body.toBoogie)
    }*/
  }

  override def gammas: Set[Variable] = body.gammas
}

/** Unsigned extend - pad in BIL
  */

case class UnsignedExtend(width: Int, body: Expr) extends Expr {
  //override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals
  //override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def size: Int = width

  override def toBoogie: BExpr = {
    if (width > body.size) {
      BVZeroExtend(width - body.size, body.toBoogie)
    } else {
      Extract(width - 1, 0, body).toBoogie
    }
    /*
    val extend = width - body.size
    if (extend == 0) {
      body.toBoogie
    } else {
      BVZeroExtend(extend, body.toBoogie)
    }
    */
  }
  override def gammas: Set[Variable] = body.gammas
}

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class Extract(high: Int, low: Int, body: Expr) extends Expr {
  override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals

  //override def subst(v: Variable, w: Variable): Expr = this.copy(body = body.subst(v, w))

  /*
  override def simplify(old: Expr, sub: Expr): Expr =
    SimplificationUtil.bitvecExtract(copy(body = body.simplify(old, sub)))
   */
  // + 1 as extracts are inclusive (e.g. [31:0] has 32 bits)
  override val size: Int = high - low + 1

  override def toBoogie: BExpr = {
    val bodySize = body.size
    if (size > bodySize) {
      if (low == 0) {
        BVZeroExtend(size - bodySize, body.toBoogie)
      } else {
        BVExtract(high + 1, low, BVZeroExtend(size - bodySize, body.toBoogie))
      }
    } else {
      BVExtract(high + 1, low, body.toBoogie)
    }
    /*
    val boogieBody = body.toBoogie
    boogieBody match {
      case extract: BVExtract => BVExtract(high + 1 + extract.start, low + extract.start, extract.body)
      case _                  => BVExtract(high + 1, low, body.toBoogie)
    }
    */
  }
  override def gammas: Set[Variable] = body.gammas
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
  //override def subst(v: Variable, w: Variable): Expr = this
  //override def simplify(old: Expr, sub: Expr): Expr = this

  override def toBoogie: BitVecLiteral = BitVecLiteral(value, size)
  override def gammas: Set[Variable] = Set()
}

/** Unary operator
  */
case class UnOp(operator: UnOperator, exp: Expr) extends Expr {
  //override def subst(v: Variable, w: Variable): Expr = copy(exp = exp.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = SimplificationUtil.uniArithmetic(copy(exp = exp.simplify(old, sub)))

  override def locals: Set[LocalVar] = exp.locals
  override def size: Int = exp.size

  override def toBoogie: UnaryBExpr = operator match {
    case NOT => UnaryBExpr(BVNOT, exp.toBoogie)
    case NEG => UnaryBExpr(BVNEG, exp.toBoogie)
  }
  override def gammas: Set[Variable] = exp.gammas
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
  //override def subst(v: Variable, w: Variable): Expr = {
  //  copy(lhs = lhs.subst(v, w), rhs = rhs.subst(v, w))
  //}

  /* override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.binArithmetic(copy(lhs = lhs.simplify(old,sub), rhs = rhs.simplify(old, sub)))
  } */

  override def locals: Set[LocalVar] = lhs.locals ++ rhs.locals
  override def gammas: Set[Variable] = lhs.gammas ++ rhs.gammas

  override def size: Int = operator match {
    case EQ | NEQ | LT | LE | SLT | SLE => 1
    case _                              => lhs.size
  }

  override def toBoogie: BExpr = operator match {
    case PLUS    => BinaryBExpr(BVADD, lhs.toBoogie, rhs.toBoogie)
    case MINUS   => BinaryBExpr(BVSUB, lhs.toBoogie, rhs.toBoogie)
    case TIMES   => BinaryBExpr(BVMUL, lhs.toBoogie, rhs.toBoogie)
    case DIVIDE  => BinaryBExpr(BVUDIV, lhs.toBoogie, rhs.toBoogie)
    case SDIVIDE => BinaryBExpr(BVSDIV, lhs.toBoogie, rhs.toBoogie)
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

trait Variable extends Expr {
  //override def subst(v: Variable, w: Variable): Variable = if (v == this) w else this
  //def toGamma: SecVar | SecMemLoad
}

case class LocalVar(name: String, override val size: Int) extends Variable {
  override def toString: String = name
  override def locals: Set[LocalVar] = Set(this)
  override def gammas: Set[Variable] = Set(this)
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolType, Scope.Local)
  override def toBoogie: BVar = BVariable(name, BitVec(size), Scope.Local)
  //override def simplify(old: Expr, sub: Expr): Expr = if (old == this) sub else this
}

/** A load from memory at location exp
  */
case class MemAccess(memory: Memory, index: Expr, endian: Endian, override val size: Int) extends Variable {
  override def toString: String = s"${memory.name}[$index]"
  override def locals: Set[LocalVar] = index.locals
  override def toBoogie: MemoryLoad = MemoryLoad(memory.toBoogie, index.toBoogie, endian, size)
  override def gammas: Set[Variable] = Set(this)
  override def toGamma: BExpr = if (memory.name == "stack") {
    GammaLoad(memory.toGamma, index.toBoogie, size, size / memory.valueSize)
  } else {
    BinaryBExpr(BoolOR, GammaLoad(memory.toGamma, index.toBoogie, size, size / memory.valueSize), L(memory.toBoogie, index.toBoogie))
  }


  /*
  override def simplify(old: Expr, sub: Expr): Expr = {
    if (!onStack) copy(index = index.simplify(old, sub))
    else if (onStack && old == this) sub
    else this
  }
   */

  //def toL: SecMemLoad = SecMemLoad(false, true, index)
  //override def toGamma: SecMemLoad = SecMemLoad(true, false, index)

  /*
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
  */
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

case class Memory(name: String, addressSize: Int, valueSize: Int) extends Variable {
  override def size: Int = valueSize // should reconsider
  //override def subst(v: Variable, w: Variable): Expr = ???
  override def locals: Set[LocalVar] = Set()
  override def gammas: Set[Variable] = Set()
  override def toBoogie: MapVar = MapVar(name, MapType(BitVec(addressSize), BitVec(valueSize)), Scope.Global)
  override def toGamma: MapVar = MapVar(s"Gamma_$name", MapType(BitVec(addressSize), BoolType), Scope.Global)
}

case class Store(memory: Memory, index: Expr, value: Expr, endian: Endian, size: Int) extends Expr {
  //override def subst(v: Variable, w: Variable): Expr = ???
  override def locals: Set[LocalVar] = index.locals ++ value.locals
  override def gammas: Set[Variable] = Set()
  override def toBoogie: MemoryStore = MemoryStore(memory.toBoogie, index.toBoogie, value.toBoogie, endian, size)
  override def toGamma: GammaStore = GammaStore(memory.toGamma, index.toBoogie, value.toGamma, size, size / memory.valueSize)
  //def toBoogieProc: String
}

object Store {
  // initialise to replace stack references
  def init(memory: Memory, index: Expr, value: Expr, endian: Endian, size: Int): Store = {
    if (index.locals.contains(LocalVar("R31", 64))) {
      Store(memory.copy(name = "stack"), index, value, endian, size)
    } else {
      Store(memory, index, value, endian, size)
    }
  }
}

enum Endian {
  case LittleEndian
  case BigEndian
}