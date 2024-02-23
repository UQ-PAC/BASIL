package ir

import boogie._

sealed trait Expr {
  def toBoogie: BExpr
  def toGamma: BExpr = {
    val gammaVars: Set[BExpr] = gammas.map(_.toGamma)
    if (gammaVars.isEmpty) {
      TrueBLiteral
    } else if (gammaVars.size == 1) {
      gammaVars.head
    } else {
      gammaVars.tail.foldLeft(gammaVars.head) { (join: BExpr, next: BExpr) =>
        BinaryBExpr(BoolAND, next, join)
      }
    }
  }
  def loads: Set[MemoryLoad] = Set()
  def getType: IRType
  def gammas: Set[Expr] = Set()
  def variables: Set[Variable] = Set()
  def acceptVisit(visitor: Visitor): Expr = throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

sealed trait Literal extends Expr {
  override def acceptVisit(visitor: Visitor): Literal = visitor.visitLiteral(this)
}

sealed trait BoolLit extends Literal

case object TrueLiteral extends BoolLit {
  override def toBoogie: BoolBLiteral = TrueBLiteral
  override def getType: IRType = BoolType
  override def toString: String = "true"
}

case object FalseLiteral extends BoolLit {
  override def toBoogie: BoolBLiteral = FalseBLiteral
  override def getType: IRType = BoolType
  override def toString: String = "false"
}

case class BitVecLiteral(value: BigInt, size: Int) extends Literal {
  override def toBoogie: BitVecBLiteral = BitVecBLiteral(value, size)
  override def getType: IRType = BitVecType(size)
  override def toString: String = s"${value}bv$size"
}

case class IntLiteral(value: BigInt) extends Literal {
  override def toBoogie: IntBLiteral = IntBLiteral(value)
  override def getType: IRType = IntType
  override def toString: String = value.toString
}

/**
  * @param end : high bit exclusive
  * @param start : low bit inclusive
  * @param body
  */
case class Extract(end: Int, start: Int, body: Expr) extends Expr {
  override def toBoogie: BExpr = BVExtract(end, start, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(end - start)
  override def toString: String = s"$body[$end:$start]"
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitExtract(this)
  override def loads: Set[MemoryLoad] = body.loads
}

case class Repeat(repeats: Int, body: Expr) extends Expr {
  override def toBoogie: BExpr = BVRepeat(repeats, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize * repeats)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of repeat: " + this)
  }
  override def toString: String = s"Repeat($repeats, $body)"
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitRepeat(this)
  override def loads: Set[MemoryLoad] = body.loads
}

case class ZeroExtend(extension: Int, body: Expr) extends Expr {
  override def toBoogie: BExpr = BVZeroExtend(extension, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }
  override def toString: String = s"ZeroExtend($extension, $body)"
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitZeroExtend(this)
  override def loads: Set[MemoryLoad] = body.loads
}

case class SignExtend(extension: Int, body: Expr) extends Expr {
  override def toBoogie: BExpr = BVSignExtend(extension, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def variables: Set[Variable] = body.variables
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign extend: " + this)
  }
  override def toString: String = s"SignExtend($extension, $body)"
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitSignExtend(this)
  override def loads: Set[MemoryLoad] = body.loads
}

case class UnaryExpr(op: UnOp, arg: Expr) extends Expr {
  override def toBoogie: BExpr = UnaryBExpr(op, arg.toBoogie)
  override def gammas: Set[Expr] = arg.gammas
  override def variables: Set[Variable] = arg.variables
  override def loads: Set[MemoryLoad] = arg.loads
  override def getType: IRType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolType)     => BoolType
    case (_: BVUnOp, bv: BitVecType) => bv
    case (_: IntUnOp, IntType)       => IntType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecType => bv.size
    case _              => throw new Exception("type mismatch")
  }

  override def toString: String = op match {
    case uOp: BoolUnOp => s"($uOp$arg)"
    case uOp: BVUnOp   => s"bv$uOp$inSize($arg)"
    case uOp: IntUnOp  => s"($uOp$arg)"
  }

  override def acceptVisit(visitor: Visitor): Expr = visitor.visitUnaryExpr(this)
}

trait UnOp

sealed trait BoolUnOp(op: String) extends UnOp {
  override def toString: String = op
}

case object BoolNOT extends BoolUnOp("!")

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

case class BinaryExpr(op: BinOp, arg1: Expr, arg2: Expr) extends Expr {
  override def toBoogie: BExpr = BinaryBExpr(op, arg1.toBoogie, arg2.toBoogie)
  override def gammas: Set[Expr] = arg1.gammas ++ arg2.gammas
  override def variables: Set[Variable] = arg1.variables ++ arg2.variables
  override def loads: Set[MemoryLoad] = arg1.loads ++ arg2.loads
  override def getType: IRType = (op, arg1.getType, arg2.getType) match {
    case (_: BoolBinOp, BoolType, BoolType) => BoolType
    case (binOp: BVBinOp, bv1: BitVecType, bv2: BitVecType) =>
      binOp match {
        case BVCONCAT =>
          BitVecType(bv1.size + bv2.size)
        case BVAND | BVOR | BVADD | BVMUL | BVUDIV | BVUREM | BVSHL | BVLSHR | BVNAND | BVNOR | BVXOR | BVXNOR | BVSUB |
            BVSREM | BVSDIV | BVSMOD | BVASHR =>
          if (bv1.size == bv2.size) {
            bv1
          } else {
            throw new Exception("bitvector size mismatch")
          }
        case BVCOMP =>
          if (bv1.size == bv2.size) {
            BitVecType(1)
          } else {
            throw new Exception("bitvector size mismatch")
          }
        case BVULT | BVULE | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
          if (bv1.size == bv2.size) {
            BoolType
          } else {
            throw new Exception("bitvector size mismatch")
          }
        case BVEQ | BVNEQ =>
          BoolType
      }
    case (intOp: IntBinOp, IntType, IntType) =>
      intOp match {
        case IntADD | IntSUB | IntMUL | IntDIV | IntMOD     => IntType
        case IntEQ | IntNEQ | IntLT | IntLE | IntGT | IntGE => BoolType
      }
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  private def inSize = arg1.getType match {
    case bv: BitVecType => bv.size
    case _              => throw new Exception("type mismatch")
  }

  override def toString: String = op match {
    case bOp: BoolBinOp => s"($arg1 $bOp $arg2)"
    case bOp: BVBinOp =>
      bOp match {
        case BVEQ | BVNEQ | BVCONCAT =>
          s"($arg1 $bOp $arg2)"
        case _ =>
          s"bv$bOp$inSize($arg1, $arg2)"
      }
    case bOp: IntBinOp => s"($arg1 $bOp $arg2)"
  }

  override def acceptVisit(visitor: Visitor): Expr = visitor.visitBinaryExpr(this)

}

trait BinOp

sealed trait BoolBinOp(op: String) extends BinOp {
  override def toString: String = op
}

case object BoolEQ extends BoolBinOp("==")
case object BoolNEQ extends BoolBinOp("!=")
case object BoolAND extends BoolBinOp("&&")
case object BoolOR extends BoolBinOp("||")
case object BoolIMPLIES extends BoolBinOp("==>")
case object BoolEQUIV extends BoolBinOp("<==>")

sealed trait BVBinOp(op: String) extends BinOp {
  override def toString: String = op
}

case object BVAND extends BVBinOp("and")
case object BVOR extends BVBinOp("or")
case object BVADD extends BVBinOp("add")
case object BVMUL extends BVBinOp("mul")
case object BVUDIV extends BVBinOp("udiv")
case object BVUREM extends BVBinOp("urem")
case object BVSHL extends BVBinOp("shl")
case object BVLSHR extends BVBinOp("lshr")
case object BVULT extends BVBinOp("ult")
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
case object BVULE extends BVBinOp("ule")
case object BVUGT extends BVBinOp("ugt")
case object BVUGE extends BVBinOp("uge")
case object BVSLT extends BVBinOp("slt")
case object BVSLE extends BVBinOp("sle")
case object BVSGT extends BVBinOp("sgt")
case object BVSGE extends BVBinOp("sge")
case object BVEQ extends BVBinOp("==")
case object BVNEQ extends BVBinOp("!=")
case object BVCONCAT extends BVBinOp("++")

sealed trait IntBinOp(op: String) extends BinOp {
  override def toString: String = op
  def toBV: BVBinOp = this match {
    case IntADD => BVADD
    case IntMUL => BVMUL
    case IntSUB => BVSUB
    case IntDIV => BVSDIV
    case IntMOD => BVSMOD
    case IntEQ  => BVEQ
    case IntNEQ => BVNEQ
    case IntLT  => BVSLT
    case IntLE  => BVSLE
    case IntGT  => BVSGT
    case IntGE  => BVSGE
  }
}

case object IntADD extends IntBinOp("+")
case object IntMUL extends IntBinOp("*")
case object IntSUB extends IntBinOp("-")
case object IntDIV extends IntBinOp("div")
case object IntMOD extends IntBinOp("mod")
case object IntEQ extends IntBinOp("==")
case object IntNEQ extends IntBinOp("!=")
case object IntLT extends IntBinOp("<")
case object IntLE extends IntBinOp("<=")
case object IntGT extends IntBinOp(">")
case object IntGE extends IntBinOp(">=")

enum Endian {
  case LittleEndian
  case BigEndian
}

case class MemoryLoad(mem: Memory, index: Expr, endian: Endian, size: Int) extends Expr {
  override def toBoogie: BMemoryLoad = BMemoryLoad(mem.toBoogie, index.toBoogie, endian, size)
  override def toGamma: BExpr = mem match {
    case m: StackMemory =>
      GammaLoad(m.toGamma, index.toBoogie, size, size / m.valueSize)
    case m: SharedMemory =>
      BinaryBExpr(BoolOR, GammaLoad(m.toGamma, index.toBoogie, size, size / m.valueSize), L(m.toBoogie, index.toBoogie))
  }
  override def variables: Set[Variable] = index.variables
  override def gammas: Set[Expr] = Set(this)
  override def loads: Set[MemoryLoad] = Set(this)
  override def getType: IRType = BitVecType(size)
  override def toString: String = s"MemoryLoad($mem, $index, $endian, $size)"
  override def acceptVisit(visitor: Visitor): Expr = visitor.visitMemoryLoad(this)
}

// Means something has a global scope from the perspective of the IR and Boogie
// Not the same as global in the sense of shared memory between threads
sealed trait Global

// A variable that is accessible without a memory load/store
sealed trait Variable extends Expr {
  val name: String
  val irType: IRType
  override def getType: IRType = irType
  override def variables: Set[Variable] = Set(this)
  override def gammas: Set[Expr] = Set(this)
  override def toBoogie: BVar
  // placeholder definition not actually used
  override def toGamma: BVar = BVariable(s"$name", irType.toBoogie, Scope.Global)

  override def toString: String = s"Variable($name, $irType)"

  override def acceptVisit(visitor: Visitor): Variable =
    throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

// Variable with global scope (in a 'accessible from any procedure' sense), not related to the concurrent shared memory sense
// These are all hardware registers
case class Register(override val name: String, size: Int) extends Variable with Global {
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolBType, Scope.Global)
  override def toBoogie: BVar = BVariable(s"$name", irType.toBoogie, Scope.Global)
  override def toString: String = s"Register($name, $irType)"
  override def acceptVisit(visitor: Visitor): Variable = visitor.visitRegister(this)
  override val irType: BitVecType = BitVecType(size)
}

// Variable with scope local to the procedure, typically a temporary variable created in the lifting process
case class LocalVar(override val name: String, override val irType: IRType) extends Variable {
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolBType, Scope.Local)
  override def toBoogie: BVar = BVariable(s"$name", irType.toBoogie, Scope.Local)
  override def toString: String = s"LocalVar($name, $irType)"
  override def acceptVisit(visitor: Visitor): Variable = visitor.visitLocalVar(this)
}

// A memory section
sealed trait Memory extends Global {
  val name: String
  val addressSize: Int
  val valueSize: Int
  def toBoogie: BMapVar = BMapVar(name, MapBType(BitVecBType(addressSize), BitVecBType(valueSize)), Scope.Global)
  def toGamma: BMapVar = BMapVar(s"Gamma_$name", MapBType(BitVecBType(addressSize), BoolBType), Scope.Global)
  val getType: IRType = MapType(BitVecType(addressSize), BitVecType(valueSize))
  override def toString: String = s"Memory($name, $addressSize, $valueSize)"

  def acceptVisit(visitor: Visitor): Memory =
    throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

// A stack section of memory, which is local to a thread
case class StackMemory(override val name: String, override val addressSize: Int, override val valueSize: Int) extends Memory {
  override def acceptVisit(visitor: Visitor): Memory = visitor.visitStackMemory(this)
}

// A non-stack region of memory, which is shared between threads
case class SharedMemory(override val name: String, override val addressSize: Int, override val valueSize: Int) extends Memory {
  override def acceptVisit(visitor: Visitor): Memory = visitor.visitSharedMemory(this)
}