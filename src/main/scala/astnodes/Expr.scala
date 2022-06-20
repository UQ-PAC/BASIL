package astnodes

//import analysis.tools.SimplificationUtil
import util.AssumptionViolationException

/** Expression
  */
trait Expr {

  /* Substitute a given variable for another variable */
  def subst(v: Variable, w: Variable): Expr
  def toBoogieString: String = toString

  //def simplify(old: Expr, sub: Expr): Expr
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Int

  def locals: Set[LocalVar]
}

/**
  *  Concatenation of two bitvectors
  */
case class Concat(left: Expr, right: Expr) extends Expr {
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Int = left.size + right.size

  override def locals: Set[LocalVar] = left.locals ++ right.locals
  override def subst(v: Variable, w: Variable): Expr = {
    copy(left = left.subst(v,w), right = right.subst(v,w))
  }

  /*
  override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.bitvecConcat(copy(left = left.simplify(old,sub), right = right.simplify(old,sub)))
  }
  */
}

/**
  * Signed extend - extend in BIL
  */

case class SignExtend(width: Int, body: Expr) extends Expr {
  //override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals
  override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def toString: String = toBoogieString
  override def toBoogieString: String = s"sign_extend${width}_${body.size}(${body.toBoogieString})"

  override def size: Int = width
}

/**
  * Unsigned extend - pad in BIL
  */

case class ZeroExtend(width: Int, body: Expr) extends Expr {
  //override def toString: String = String.format("%s[%d:%d]", body, high, low)
  override def locals: Set[LocalVar] = body.locals
  override def subst(v: Variable, w: Variable): Expr = copy(body = body.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = ???

  override def toString: String = toBoogieString
  override def toBoogieString: String = s"zero_extend${width}_${body.size}(${body.toBoogieString})"

  override def size: Int = width
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

  override def toBoogieString: String = s"${body.toBoogieString}[${high + 1}:$low]"
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
  override def toString: String = toBoogieString //String.format("%s", value)
  override def toBoogieString: String = value.toString + s"bv$size"

  override def locals: Set[LocalVar] = Set()
  override def subst(v: Variable, w: Variable): Expr = this
  //override def simplify(old: Expr, sub: Expr): Expr = this
}

// TODO: Literal that has BigInt instead of string, handle size properly

/** Function call
  */
/*
case class FunctionCall(funcName: String, args: List[Expr], override val size: Option[Int] = None) extends Expr {

  /** Value of literal */
  override def toBoogieString = s"$funcName(${args.map(a => a.toBoogieString).mkString(", ")})"

  override def vars: List[Variable] = args.flatMap(a => a.vars)
  //override def subst(v: Variable, w: Variable): Expr = copy(args = args.map(a => a.subst(v, w)))
  override def simplify(old: Expr, sub: Expr): Expr = this
}
*/

/** Unary operator
  */
case class UniOp(operator: UniOperator, exp: Expr) extends Expr {
  override def toString: String = toBoogieString //String.format("%s %s", operator, exp)
  override def toBoogieString: String = s"${operator.toBoogie(size)}(${exp.toBoogieString})"
  override def subst(v: Variable, w: Variable): Expr = copy(exp = exp.subst(v, w))
  //override def simplify(old: Expr, sub: Expr): Expr = SimplificationUtil.uniArithmetic(copy(exp = exp.simplify(old, sub)))

  override def locals: Set[LocalVar] = exp.locals

  override def size: Int = exp.size

}

enum UniOperator {
  case NOT
  case NEG

  def toBoogie(size: Int): String = {
    //val size1 = size.getOrElse(64)
    this match {
      case NOT => s"bv${size}not"
      case NEG => s"bv${size}neg"
    }
  }

  def changesSize: Boolean = false
}

object UniOperator {
  def apply(bilStr: String): UniOperator = bilStr match {
    case "NEG" => NEG
    case "NOT" => NOT
  }
}


/** Binary operation of two expressions
  */
case class BinOp(operator: BinOperator, lhs: Expr, rhs: Expr) extends Expr {

  override def toString: String = toBoogieString
  //override def toString String: String = String.format("(%s) %s (%s)", lhs, operator, rhs)
  override def toBoogieString: String = operator match {
    case BinOperator.ADD => s"bv${size}add(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.SUB => s"bv${size}sub(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.MUL => s"bv${size}mul(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.DIV => s"bv${size}udiv(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.SDIV => s"bv${size}sdiv(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.MOD => s"bv${size}mod(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.SMOD => s"bv${size}srem(${lhs.toBoogieString}, ${rhs.toBoogieString})" // TODO: check BIL semantics here, it could be smod instead?
    case BinOperator.AND => s"bv${size}and(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.OR => s"bv${size}or(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.XOR => s"bv${size}xor(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.LSL => s"bv${size}shl(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.LSR => s"bv${size}lshr(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.ASR => s"bv${size}ashr(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.LT => s"bv${size}ult(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.LE => s"bv${size}ule(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.SLT => s"bv${size}slt(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.SLE => s"bv${size}sle(${lhs.toBoogieString}, ${rhs.toBoogieString})"
    case BinOperator.EQ => s"if (${lhs.toBoogieString} == ${rhs.toBoogieString}) then 0bv1 else 0bv0"
    case BinOperator.NEQ => s"if (${lhs.toBoogieString} != ${rhs.toBoogieString}) then 0bv1 else 0bv0"
  }

    //s"${operator.toBoogie(size)}(${lhs.toBoogieString}, ${rhs.toBoogieString})"

    //operator.toBoogie(operator.size(lhs.size, rhs.size)).fold(s"${lhs.toBoogieString}, ${rhs.toBoogieString}")((inner, fun) => s"$fun($inner)")

  override def subst(v: Variable, w: Variable): Expr = {
    copy(lhs = lhs.subst(v,w), rhs = rhs.subst(v, w))
  }

  /* override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.binArithmetic(copy(lhs = lhs.simplify(old,sub), rhs = rhs.simplify(old, sub)))
  } */

  override def locals: Set[LocalVar] = lhs.locals ++ rhs.locals

  // Finish resolveTypes and then remove this
  override def size: Int = operator.size(lhs.size, rhs.size)
  /*
  if (lhs.size != rhs.size) {
    throw new AssumptionViolationException(s"Both sides of binop should have the same size: $lhs: ${lhs.size}, $rhs: ${rhs.size}")
  } else {
    operator.size(lhs.size, rhs.size)
  }
  */

}

enum BinOperator {
  case ADD
  case SUB
  case MUL
  case DIV
  case SDIV
  case MOD
  case SMOD
  case AND
  case OR
  case XOR
  case LSL
  case LSR
  case ASR
  case EQ
  case NEQ
  case LT
  case LE
  case SLT
  case SLE

  def changesSize: Boolean = this match {
    case EQ | NEQ => true
    case _ => false
  }

  def size(lhsSize: Int, rhsSize: Int): Int = this match {
    case EQ | NEQ | LT | LE | SLT | SLE => 1
    case _ => lhsSize
  }
}

object BinOperator {
  def apply(adtStr: String): BinOperator = adtStr match {
    case "PLUS"    => ADD
    case "MINUS"   => SUB
    case "TIMES"   => MUL
    case "DIVIDE"  => DIV
    case "SDIVIDE" => SDIV
    case "MOD"     => MOD
    case "SMOD"    => SMOD
    case "LSHIFT"  => LSL
    case "RSHIFT"  => LSR
    case "ARSHIFT" => ASR
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

/** Variable
  *
  *  Variables can be registers (e.g. R1, SP, #31) or loads from memory (e.g. mem[10])
  */
trait Variable extends Expr {
  override def subst(v: Variable, w: Variable): Variable = if (v == this) w else this
  def toGamma: SecVar | SecMemLoad
}

/** A register
  */
case class LocalVar(name: String, override val size: Int) extends Variable {
  override def toString: String = name
  override def locals: Set[LocalVar] = Set(this)
  override def toGamma: SecVar = SecVar(name, true)
  //override def simplify(old: Expr, sub: Expr): Expr = if (old == this) sub else this
}

/** A load from memory at location exp
  */
case class MemAccess(memory: Memory, index: Expr, endian: Endian, override val size: Int) extends Variable {
  override def toString: String = s"${memory.name}[$index]"

  /*override def toString = s"${if (this.onStack) "stack" else "heap"}[$index]"

  // TODO this is a mess
  def toBoogieString(exp: Expr) = s"${if (this.onStack) "stack" else "heap"}[${exp.toBoogieString}]"
  override def toBoogieString: String =
    (0 until size / 8)
      .map(n => s"${toBoogieString(BinOp(BinOperator.ADD, index, Literal(n, 64)))}")
      .mkString(" ++ ")
  */

  override def locals: Set[LocalVar] = index.locals

  /*
  override def simplify(old: Expr, sub: Expr): Expr = {
    if (!onStack) copy(index = index.simplify(old, sub))
    else if (onStack && old == this) sub
    else this
  }
  */

  def toL: SecMemLoad = SecMemLoad(false, true, index)
  override def toGamma: SecMemLoad = SecMemLoad(true, false, index)
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
}
