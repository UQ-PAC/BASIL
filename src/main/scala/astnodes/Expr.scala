package astnodes

import analysis.tools.SimplificationUtil
import util.AssumptionViolationException

/** Expression
  */
trait Expr {
  /*  All of the variables in a given expression */
  def vars: List[Variable]

  /* Substitute a given variable for another variable */
  def subst(v: Variable, w: Variable): Expr
  def toBoogieString: String = toString

  def simplify(old: Expr, sub: Expr): Expr
  /*
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Option[Int]
}

/**
  *  Concatenation of two bitvectors
  */
case class Concat(left: Expr, right: Expr) extends Expr {
  override def toBoogieString: String = s"${left.toBoogieString} ++ ${right.toBoogieString}"

  override def size: Option[Int] = (left.size, right.size) match {
    case (Some(x), Some(y)) => Some(x + y)
    case _ => None
  }

  override def vars: List[Variable] = left.vars ++ right.vars
  override def subst(v: Variable, w: Variable): Expr = {
    copy(left = left.subst(v,w), right = right.subst(v,w))
  }

  override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.bitvecConcat(copy(left = left.simplify(old,sub), right = right.simplify(old,sub)))
  }
}

/**
  * Construct a Concatenation from a Bil extend operation
  */
case object Extend {
  // FIXME: This implementation is wrong. Extend is the same as Pad, but it uses the most significant bit, rather than
  //  always adding 0's. This can be thought of as an signed pad.
  def apply(expr: Expr, size: Int): Expr =
    if (size - expr.size.get > 0) Concat(expr, Literal("0", Some(size - expr.size.get)))
    else expr

}

/**
  * Construct a Concatenation from a Bil pad operation
  */
case object Pad {
  def apply(expr: Expr, size: Int): Expr =
    if (size - expr.size.get > 0) Concat(Literal("0", Some(size - expr.size.get)), expr)
    else expr

}

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class Extract(firstInt: Int, secondInt: Int, body: Expr) extends Expr {
  override def toString: String = String.format("%s[%d:%d]", body, firstInt, secondInt)
  override def vars: List[Variable] = body.vars

  override def subst(v: Variable, w: Variable): Expr = this.copy(body = body.subst(v, w))

  override def simplify(old: Expr, sub: Expr): Expr =
    SimplificationUtil.bitvecExtract(this.copy(body = body.simplify(old, sub)))

  override def size: Option[Int] = Some(
    firstInt - secondInt + 1
  ) // + 1 as extracts are inclusive (e.g. 0:31 has 32 bits)

  override def toBoogieString: String = s"${body.toBoogieString}[${firstInt + 1}:$secondInt]"
}

/** Literal expression (e.g. 4, 5, 10)
  */
case class Literal(value: String, override val size: Option[Int] = None) extends Expr {
  /** Value of literal */
  override def toString: String = String.format("%s", value)
  override def toBoogieString: String = value + s"bv${if (size.isDefined) size.get else 64}"

  override def vars: List[Register] = List()
  override def subst(v: Variable, w: Variable): Expr = this
  override def simplify(old: Expr, sub: Expr): Expr = this
}

case object Literal {
  def apply(value: String, size: Option[Int] = None) = new Literal(parseHex(value), size)

  private def parseHex(value: String): String = {
    if (value == null) "0"
    else if (value.length < 3 || !(value.substring(0, 2) == "0x")) value
    else java.lang.Long.toUnsignedString(java.lang.Long.parseUnsignedLong(value.substring(2), 16))
  }
}

// TODO: Literal that has BigInt instead of string, handle size properly

/** Function call
  */
case class FunctionCall(funcName: String, args: List[Expr], override val size: Option[Int] = None) extends Expr {

  /** Value of literal */
  override def toBoogieString = s"$funcName(${args.map(a => a.toBoogieString).mkString(", ")})"

  override def vars: List[Variable] = args.flatMap(a => a.vars)
  override def subst(v: Variable, w: Variable): Expr = copy(args = args.map(a => a.subst(v, w)))
  override def simplify(old: Expr, sub: Expr): Expr = this
}

case class MemStore(loc: Expr, expr: Expr, override val size: Some[Int]) extends Expr {
  override def vars: List[Register] = ???
  override def subst(v: Variable, w: Variable): Expr = ???
  override def simplify(old: Expr, sub: Expr): Expr = this
}

/** Unary operator
  */
case class UniOp(operator: UniOperator, exp: Expr) extends Expr {
  override def toString: String = String.format("%s %s", operator, exp)
  override def toBoogieString: String = s"${operator.toBoogie(size)}(${exp.toBoogieString})"
  override def subst(v: Variable, w: Variable): Expr = copy(exp = exp.subst(v, w))
  override def simplify(old: Expr, sub: Expr): Expr = SimplificationUtil.uniArithmetic(copy(exp = exp.simplify(old, sub)))

  override def vars: List[Variable] = exp.vars

  override def size: Option[Int] = exp.size

}

enum UniOperator {
  case NOT
  case NEG

  def toBoogie(size: Option[Int]): String = {
    val size1 = size.getOrElse(64)
    this match {
      case NOT => s"bv${size1}not"
      case NEG => s"bv${size1}neg"
    }
  }

  def changesSize: Boolean = false
}

object UniOperator {
  def fromBil(bilStr: String): UniOperator = bilStr match {
    case "-" => NEG
    case "~" => NOT
  }

  def fromAdt(bilStr: String): UniOperator = bilStr match {
    case "NEG" => NEG
    case "NOT" => NOT
  }
}


/** Binary operation of two expressions
  */
case class BinOp(operator: BinOperator, firstExp: Expr, secondExp: Expr) extends Expr {

  override def toString: String = String.format("(%s) %s (%s)", firstExp, operator, secondExp)
  override def toBoogieString: String = operator.toBoogie(inputSize).fold(s"${firstExp.toBoogieString}, ${secondExp.toBoogieString}")((inner, fun) => s"$fun($inner)")

  override def subst(v: Variable, w: Variable): Expr = {
    copy(firstExp = firstExp.subst(v,w), secondExp = secondExp.subst(v, w))
  }

  override def simplify(old: Expr, sub: Expr): Expr = {
    SimplificationUtil.binArithmetic(copy(firstExp = firstExp.simplify(old,sub), secondExp = secondExp.simplify(old, sub)))
  }

  override def vars: List[Variable] = firstExp.vars ++ secondExp.vars

  // Finish resolveTypes and then remove this
  override def size: Option[Int] = operator.size(inputSize)

  def inputSize: Option[Int] = (firstExp.size, secondExp.size) match {
    case (Some(a), Some(b)) =>
      if (a == b) {
        Some(a)
      } else {
        throw new AssumptionViolationException(s"Both sides of binop should have the same size $firstExp: ${firstExp.size}, $secondExp: ${secondExp.size}")
      }
    case (Some(x), None) => Some(x)
    case (None, Some(x)) => Some(x)
    case (None, None) => None
  }
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

  def toBoogie(size: Option[Int]): List[String] = {
    val size1 = size.getOrElse("64")
    this match {
      case ADD => List(s"bv${size1}add")
      case SUB => List(s"bv${size1}sub")
      case MUL => List(s"bv${size1}mul")
      case DIV => List(s"bv${size1}udiv")
      case SDIV => List(s"bv${size1}sdiv")
      case MOD => List(s"bv${size1}mod")
      case SMOD => List(s"bv${size1}srem") // TODO: check BIL semantics here, it could be smod instead?
      case AND => List(s"bv${size1}and")
      case OR => List(s"bv${size1}or")
      case XOR => List(s"bv${size1}xor")
      case LSL => List(s"bv${size1}shl")
      case LSR => List(s"bv${size1}lshr")
      case ASR => List(s"bv${size1}ashr")
      case EQ => List(s"bv${size1}eq", "booltobv1") // TODO: remove booltobv1 which is redundant
      case NEQ => List(s"bv${size1}neq", "booltobv1")
      case LT => List(s"bv${size1}ult")
      case LE => List(s"bv${size1}ule")
      case SLT => List(s"bv${size1}slt")
      case SLE => List(s"bv${size1}sle")
    }
  }

  def changesSize: Boolean = this match {
    case EQ | NEQ => true
    case _ => false
  }

  def size(size: Option[Int]): Option[Int] = this match {
    case EQ | NEQ | LT | LE | SLT | SLE => Some(1)
    case _ => size
  }

}

object BinOperator {
  def fromAdt(adtStr: String): BinOperator = adtStr match {
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

  def fromBil(bilStr: String): BinOperator = bilStr match {
    // note at the moment we do not distinguish between signed and unsigned
    // arithmetic operators
    case "+" => ADD
    case "-" => SUB
    case "*" => MUL
    case "/" => DIV
    case "%" => MOD
    case "&" => AND
    case "|" => OR
    case "xor" => XOR
    case "<<" => LSL
    case ">>" => LSR
    case "~>>" => ASR
    case "=" => EQ
    case "<>" => NEQ
    case "<" => LT
    case "<=" => LE
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
case class Register(name: String, override val size: Option[Int]) extends Variable {
  override def toString: String = name
  override def vars: List[Register] = List(this)
  override def toGamma: SecVar = SecVar(name, true)
  override def simplify(old: Expr, sub: Expr): Expr = if (old == this) sub else this
}

case object Register {
  def apply(name: String, size: Int) = new Register(name, Some(size))
}

/** A load from memory at location exp
  */
case class MemLoad(exp: Expr, override val size: Some[Int]) extends Variable {

  override def toString = s"${if (this.onStack) "stack" else "heap"}[$exp]"

  // TODO this is a mess
  def toBoogieString(exp: Expr) = s"${if (this.onStack) "stack" else "heap"}[${exp.toBoogieString}]"
  override def toBoogieString: String =
    (0 until size.get / 8)
      .map(n => s"${toBoogieString(BinOp(BinOperator.ADD, exp, Literal(n.toString, Some(64))))}")
      .mkString(" ++ ")

  override def vars: List[MemLoad] = List(this) // TOOD also exp.vars????

  /** Assumes: anything on the stack is represented as SP + val (where val is an int etc)
    */
  val onStack: Boolean = onStackMatch(exp)

  def onStackMatch(expr: Expr): Boolean = expr match {
    case v: Register => v.name == "R31"
    case BinOp(_, e1: Expr, e2: Expr) => onStackMatch(e1) || onStackMatch(e2)
    case UniOp(_, e1: Expr) => onStackMatch(e1)
    case _ => false
  }

  override def simplify(old: Expr, sub: Expr): Expr = {
    if (!this.onStack) this.copy(exp.simplify(old, sub))
    else if (this.onStack && old == this) sub
    else this
  }

  def toL: SecMemLoad = SecMemLoad(false, true, exp)
  override def toGamma: SecMemLoad = SecMemLoad(true, false, exp)
}
