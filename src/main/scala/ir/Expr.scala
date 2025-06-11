package ir
import boogie._
import util.CachedHashCode
import scala.collection.mutable
import util.functional.Snoc

sealed trait Expr extends DefaultDeepEquality {
  def toBoogie: BExpr
  def getType: IRType

  /** variables that occur in the expression NOT including those inside a load's index */
  def gammas: Set[Variable] = Set()

  /** all variables that occur in the expression */
  def variables: Set[Variable] = Set()
  def acceptVisit(visitor: Visitor): Expr = throw new Exception("visitor " + visitor + " unimplemented for: " + this)

  override def toString() = translating.PrettyPrinter.pp_expr(this)

  lazy val variablesCached = variables
}

object Sigil {
  object Boogie {
    def block = "b#"
    def proc = "p$"
    def localVar = "#"
    def globalVar = "$"
  }

  object BASIR {
    def block = "#"
    def proc = "@"
    def localVar = "%"
    def globalVar = "$"
    def attrib = "."
  }
}

def unsigilVar(s: String): String = s match {
  case s"#$local" => unsigilVar(local)
  case s"$$$glob" => unsigilVar(glob)
  case o => o
}

def size(e: Expr) = {
  e.getType match {
    case BitVecType(s) => Some(s)
    case _ => None
  }
}

sealed trait Literal extends Expr {
  override def acceptVisit(visitor: Visitor): Literal = visitor.visitLiteral(this)
}

sealed trait BoolLit extends Literal {
  def value: Boolean
}

case object TrueLiteral extends BoolLit {
  override def toBoogie: BoolBLiteral = TrueBLiteral
  override def getType: IRType = BoolType
  override def value = true
}

case object FalseLiteral extends BoolLit {
  override def toBoogie: BoolBLiteral = FalseBLiteral
  override def getType: IRType = BoolType
  override def value = false
}

case class BitVecLiteral(value: BigInt, size: Int) extends Literal with CachedHashCode {
  require(size >= 0)
  require(value <= getType.maxValue, s"bad value: $value for width $size")
  override def toBoogie: BitVecBLiteral = BitVecBLiteral(value, size)
  override def getType: BitVecType = BitVecType(size)
}

case class IntLiteral(value: BigInt) extends Literal with CachedHashCode {
  override def toBoogie: IntBLiteral = IntBLiteral(value)
  override def getType: IRType = IntType
}

/** Extracts a subsequence of bits (end..start) from body.
  *
  * @param end
  *   : high bit exclusive
  * @param start
  *   : low bit inclusive
  * @param body
  *
  * Requires end > start
  */
case class Extract(end: Int, start: Int, body: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = BVExtract(end, start, body.toBoogie)
  override def gammas: Set[Variable] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(end - start)
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitExtract(this)
}

/** Gives repeats copies of body, concatenated.
  *
  * Requires repeats > 0.
  */
case class Repeat(repeats: Int, body: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = BVRepeat(repeats, body.toBoogie)
  override def gammas: Set[Variable] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize * repeats)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of repeat: " + this)
  }
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitRepeat(this)
}

/** Zero-extends by extension extra bits. */
case class ZeroExtend(extension: Int, body: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = BVZeroExtend(extension, body.toBoogie)
  override def gammas: Set[Variable] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitZeroExtend(this)
}

/** Sign-extends by extension extra bits. */
case class SignExtend(extension: Int, body: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = BVSignExtend(extension, body.toBoogie)
  override def gammas: Set[Variable] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign extend: " + this)
  }
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitSignExtend(this)
}

case class UnaryExpr(op: UnOp, arg: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = UnaryBExpr(op, arg.toBoogie)
  override def gammas: Set[Variable] = arg.gammas
  override def variables: Set[Variable] = arg.variables
  override def getType: IRType = (op, arg.getType) match {
    case (BoolToBV1, BoolType) => BitVecType(1)
    case (_: BoolUnOp, BoolType) => BoolType
    case (_: BVUnOp, bv: BitVecType) => bv
    case (_: IntUnOp, IntType) => IntType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def acceptVisit(visitor: Visitor): Expr = visitor.visitUnaryExpr(this)
}

sealed trait UnOp

sealed trait BoolUnOp(op: String) extends UnOp {
  override def toString: String = op
}

case object BoolNOT extends BoolUnOp("!")
case object BoolToBV1 extends BoolUnOp("bool2bv1")

sealed trait IntUnOp(op: String) extends UnOp {
  override def toString: String = op
  def toBV: BVUnOp = BVNEG
}

case object IntNEG extends IntUnOp("-")

sealed trait BVUnOp(op: String) extends UnOp {
  override def toString: String = op
}

case object BVNOT extends BVUnOp("not")
case object BVNEG extends BVUnOp("neg")

case class BinaryExpr(op: BinOp, arg1: Expr, arg2: Expr) extends Expr with CachedHashCode {
  override def toBoogie: BExpr = BinaryBExpr(op, arg1.toBoogie, arg2.toBoogie)
  override def gammas: Set[Variable] = arg1.gammas ++ arg2.gammas
  override def variables: Set[Variable] = arg1.variables ++ arg2.variables
  override def getType: IRType = (op, arg1.getType, arg2.getType) match {
    case (_: BoolBinOp, BoolType, BoolType) => BoolType
    case (EQ, _, _) => BoolType
    case (NEQ, _, _) => BoolType
    case (binOp: BVBinOp, bv1: BitVecType, bv2: BitVecType) =>
      binOp match {
        case BVCONCAT =>
          BitVecType(bv1.size + bv2.size)
        case BVAND | BVOR | BVADD | BVMUL | BVUDIV | BVUREM | BVSHL | BVLSHR | BVNAND | BVNOR | BVXOR | BVXNOR | BVSUB |
            BVSREM | BVSDIV | BVSMOD | BVASHR =>
          if (bv1.size == bv2.size) {
            bv1
          } else {
            throw new Exception(s"bitvector size mismatch $bv1 $bv2")
          }
        case BVCOMP =>
          if (bv1.size == bv2.size) {
            BitVecType(1)
          } else {
            println(this)
            throw new Exception("bitvector size mismatch")
          }
        case BVULT | BVULE | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
          if (bv1.size == bv2.size) {
            BoolType
          } else {
            println(this)
            throw new Exception("bitvector size mismatch")
          }
      }
    case (intOp: IntBinOp, IntType, IntType) =>
      intOp match {
        case IntADD | IntSUB | IntMUL | IntDIV | IntMOD => IntType
        case IntLT | IntLE | IntGT | IntGE => BoolType
      }
    case _ =>
      throw new Exception(
        "type mismatch, operator " + op.getClass.getSimpleName + s" type doesn't match args: (" + arg1 + ", " + arg2 + ")"
      )
  }

  private def inSize = arg1.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def acceptVisit(visitor: Visitor): Expr = visitor.visitBinaryExpr(this)

}

sealed trait BinOp {
  def opName: String
}

sealed trait BoolBinOp(op: String) extends BinOp {
  override def toString: String = op
  def opName = op
}

sealed trait PolyCmp extends BinOp

case object EQ extends PolyCmp {
  override def opName = "=="
  override def toString = opName
}

case object NEQ extends PolyCmp {
  override def opName = "!="
  override def toString = opName
}

sealed trait BoolCmpOp extends BoolBinOp
case object BoolAND extends BoolBinOp("&&")
case object BoolOR extends BoolBinOp("||")
case object BoolIMPLIES extends BoolBinOp("==>") with BoolCmpOp

sealed trait BVBinOp(op: String) extends BinOp {
  override def toString: String = op
  def opName = op
}

sealed trait BVCmpOp extends BVBinOp

case object BVAND extends BVBinOp("and")
case object BVOR extends BVBinOp("or")
case object BVADD extends BVBinOp("add")
case object BVMUL extends BVBinOp("mul")
case object BVUDIV extends BVBinOp("udiv")
case object BVUREM extends BVBinOp("urem")
case object BVSHL extends BVBinOp("shl")
case object BVLSHR extends BVBinOp("lshr")
case object BVULT extends BVBinOp("ult") with BVCmpOp
case object BVNAND extends BVBinOp("nand")
case object BVNOR extends BVBinOp("nor")
case object BVXOR extends BVBinOp("xor")
case object BVXNOR extends BVBinOp("xnor")
case object BVCOMP extends BVBinOp("comp")
case object BVSUB extends BVBinOp("sub")
case object BVSDIV extends BVBinOp("sdiv")
case object BVSREM extends BVBinOp("srem")
case object BVSMOD extends BVBinOp("smod")
case object BVASHR extends BVBinOp("ashr")
case object BVULE extends BVBinOp("ule") with BVCmpOp
case object BVUGT extends BVBinOp("ugt") with BVCmpOp
case object BVUGE extends BVBinOp("uge") with BVCmpOp
case object BVSLT extends BVBinOp("slt") with BVCmpOp
case object BVSLE extends BVBinOp("sle") with BVCmpOp
case object BVSGT extends BVBinOp("sgt") with BVCmpOp
case object BVSGE extends BVBinOp("sge") with BVCmpOp
case object BVCONCAT extends BVBinOp("++")

sealed trait IntBinOp(op: String) extends BinOp {
  override def toString: String = op
  def opName = op
  def toBV: BVBinOp = this match {
    case IntADD => BVADD
    case IntMUL => BVMUL
    case IntSUB => BVSUB
    case IntDIV => BVSDIV
    case IntMOD => BVSMOD
    case IntLT => BVSLT
    case IntLE => BVSLE
    case IntGT => BVSGT
    case IntGE => BVSGE
  }
}

case object IntADD extends IntBinOp("+")
case object IntMUL extends IntBinOp("*")
case object IntSUB extends IntBinOp("-")
case object IntDIV extends IntBinOp("div")
case object IntMOD extends IntBinOp("mod")
case object IntLT extends IntBinOp("<")
case object IntLE extends IntBinOp("<=")
case object IntGT extends IntBinOp(">")
case object IntGE extends IntBinOp(">=")

enum Endian {
  case LittleEndian
  case BigEndian
}

case class UninterpretedFunction(name: String, params: Seq[Expr], returnType: IRType) extends Expr with CachedHashCode {
  override def getType: IRType = returnType
  override def toBoogie: BFunctionCall = BFunctionCall(name, params.map(_.toBoogie).toList, returnType.toBoogie, true)
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitUninterpretedFunction(this)
  override def variables: Set[Variable] = params.flatMap(_.variables).toSet
}

/** Something that has a global scope from the perspective of the IR and Boogie.
  *
  * Not the same as global in the sense of shared memory between threads.
  */
sealed trait Global

/** A variable that is accessible without a memory load/store. */
sealed trait Variable extends Expr {
  val name: String
  val irType: IRType

  override def getType: IRType = irType
  override def variables: Set[Variable] = Set(this)
  override def gammas: Set[Variable] = Set(this)
  override def toBoogie: BVar
  def toGamma: BVar

  override def acceptVisit(visitor: Visitor): Variable =
    throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

object Variable {
  implicit def ordering[V <: Variable]: Ordering[V] = Ordering.by(_.name)
}

/** Hardware registers.
  *
  * These are variables with global scope (in a 'accessible from any procedure' sense), not related to the concurrent
  * shared memory sense.
  */
case class Register(override val name: String, size: Int) extends Variable with Global with CachedHashCode {
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolBType, Scope.Global)
  override def toBoogie: BVar = BVariable(s"$name", irType.toBoogie, Scope.Global)
  override def acceptVisit(visitor: Visitor): Variable = visitor.visitRegister(this)
  override val irType: BitVecType = BitVecType(size)
}

/** Variable with scope local to the procedure, typically a temporary variable created in the lifting process. */
case class LocalVar(varName: String, override val irType: IRType, val index: Int = 0)
    extends Variable
    with CachedHashCode {
  override val name = varName + (if (index > 0) then s"_$index" else "")
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolBType, Scope.Local)
  override def toBoogie: BVar = BVariable(s"$name", irType.toBoogie, Scope.Local)
  override def acceptVisit(visitor: Visitor): Variable = visitor.visitLocalVar(this)
}

object LocalVar {
  def unapply(l: LocalVar): Some[(String, IRType, Int)] = Some((l.name, l.irType, l.index))

  /**
   * Construct a LocalVar by infering its index from the provided name corresponding to [[LocalVar.name]].
   * It matches the value of [[name]] result, dropping an '_0' suffix or otherwise extracting index [[${name}_${index}]].
   * Use only when the [[index]] field has been lost/mangled with the name, e.g. due to serialisation & parsing.
   */
  def ofIndexed(name: String, ty: IRType) = name.split("_").toList match {
    case Snoc(Nil, r) =>
      LocalVar(name, ty, 0)
    case Snoc(_, "0") | Snoc(_, "out") | Snoc(_, "in") =>
      LocalVar(name, ty, 0)
    case Snoc(r, ind) =>
      try LocalVar(r.mkString("_"), ty, (ind.toInt))
      catch
        _ => LocalVar(name, ty, 0)
    case _ => LocalVar(name, ty, 0)
  }
}

/** A global memory section (subject to shared-memory concurrent accesses from multiple threads). */
sealed trait Memory extends Global {
  val name: String
  val addressSize: Int
  val valueSize: Int
  def toBoogie: BMapVar = BMapVar(name, MapBType(BitVecBType(addressSize), BitVecBType(valueSize)), Scope.Global)
  def toGamma: BMapVar = BMapVar(s"Gamma_$name", MapBType(BitVecBType(addressSize), BoolBType), Scope.Global)
  val getType: IRType = MapType(BitVecType(addressSize), BitVecType(valueSize))

  def acceptVisit(visitor: Visitor): Memory =
    throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

/** A stack area of memory, which is local to a thread. */
case class StackMemory(override val name: String, override val addressSize: Int, override val valueSize: Int)
    extends Memory {
  override def acceptVisit(visitor: Visitor): Memory = visitor.visitStackMemory(this)
}

/** A non-stack region of memory, which may be shared between threads. */
case class SharedMemory(override val name: String, override val addressSize: Int, override val valueSize: Int)
    extends Memory {
  override def acceptVisit(visitor: Visitor): Memory = visitor.visitSharedMemory(this)
}

enum QuantifierSort:
  case exists
  case forall

case class LambdaExpr(binds: List[LocalVar], body: Expr) extends Expr {
  override def getType = uncurryFunctionType(binds.map(_.getType), body.getType)
  override def toBoogie = Lambda(binds.map(_.toBoogie), body.toBoogie)
  def returnType = body.getType
}

case class QuantifierExpr(kind: QuantifierSort, body: LambdaExpr) extends Expr {
  require(body.returnType == BoolType, "Type error: quantifier with non-boolean body")
  override def getType: IRType = BoolType
  def toBoogie: BExpr = {
    val b = body.binds.map(_.toBoogie)
    val bdy = body.body.toBoogie
    kind match {
      case QuantifierSort.forall => ForAll(b, bdy)
      case QuantifierSort.exists => Exists(b, bdy)
    }
  }
  override def variables: Set[Variable] = body.variables

}

case class OldExpr(body: Expr) extends Expr {
  override def acceptVisit(visitor: Visitor): Expr = body.acceptVisit(visitor)
  def getType = body.getType
  def toBoogie = Old(body.toBoogie)
}
