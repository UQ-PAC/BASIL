package ir

import boogie._

trait Expr {
  var ssa_id: Int = 0
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
  def getType: IRType
  def gammas: Set[Expr] = Set()
  def locals: Set[Variable] = Set()
}

trait Literal extends Expr

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

class Extract(var end: Int, var start: Int, var body: Expr) extends Expr {
  override def toBoogie: BExpr = BVExtract(end, start, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def locals: Set[Variable] = body.locals
  override def getType: BitVecType = BitVecType(end - start)
  override def toString: String = s"$body[$end:$start]"
}

class Repeat(var repeats: Int, var body: Expr) extends Expr {
  override def toBoogie: BExpr = BVRepeat(repeats, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def locals: Set[Variable] = body.locals
  override def getType: BitVecType = BitVecType(bodySize * repeats)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }
  override def toString: String = s"Repeat($repeats, $body)"
}

class ZeroExtend(var extension: Int, var body: Expr) extends Expr {
  override def toBoogie: BExpr = BVZeroExtend(extension, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def locals: Set[Variable] = body.locals
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }
  override def toString: String = s"ZeroExtend($extension, $body)"

}

class SignExtend(var extension: Int, var body: Expr) extends Expr {
  override def toBoogie: BExpr = BVSignExtend(extension, body.toBoogie)
  override def gammas: Set[Expr] = body.gammas
  override def locals: Set[Variable] = body.locals
  override def getType: BitVecType = BitVecType(bodySize + extension)
  private def bodySize: Int = body.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign extend: " + this)
  }
  override def toString: String = s"SignExtend($extension, $body)"
}

class UnaryExpr(var op: UnOp, var arg: Expr) extends Expr {
  override def toBoogie: BExpr = UnaryBExpr(op, arg.toBoogie)
  override def gammas: Set[Expr] = arg.gammas
  override def locals: Set[Variable] = arg.locals
  override def getType: IRType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolType) => BoolType
    case (_: BVUnOp, bv: BitVecType) => bv
    case (_: IntUnOp, IntType) => IntType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def toString: String = op match {
    case uOp: BoolUnOp => s"($uOp$arg)"
    case uOp: BVUnOp => s"bv$uOp$inSize($arg)"
    case uOp: IntUnOp => s"($uOp$arg)"
  }

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

class BinaryExpr(var op: BinOp, var arg1: Expr, var arg2: Expr) extends Expr {
  override def toBoogie: BExpr = BinaryBExpr(op, arg1.toBoogie, arg2.toBoogie)
  override def gammas: Set[Expr] = arg1.gammas ++ arg2.gammas
  override def locals: Set[Variable] = arg1.locals ++ arg2.locals
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
            println(arg1)
            println(arg2)
            println(this)
            throw new Exception("bitvector size mismatch")
          }
        case BVCOMP =>
          if (bv1.size == bv2.size) {
            BitVecType(1)
          } else {
            //BitVecType(1)
            println(arg1)
            println(arg2)
            println(this)
            throw new Exception("bitvector size mismatch")
          }
        case BVULT | BVULE | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
          if (bv1.size == bv2.size) {
            BoolType
          } else {
            println(arg1)
            println(arg2)
            throw new Exception("bitvector size mismatch")
          }
        case BVEQ | BVNEQ =>
          BoolType
      }
    case (intOp: IntBinOp, IntType, IntType) =>
      intOp match {
        case IntADD | IntSUB | IntMUL | IntDIV | IntMOD => IntType
        case IntEQ | IntNEQ | IntLT | IntLE | IntGT | IntGE => BoolType
      }
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  private def inSize = arg1.getType match {
    case bv: BitVecType => bv.size
    case _ => throw new Exception("type mismatch")
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

class MemoryStore(var mem: Memory, var index: Expr, var value: Expr, var endian: Endian, var size: Int) extends Expr {
  override def toBoogie: BMemoryStore = BMemoryStore(mem.toBoogie, index.toBoogie, value.toBoogie, endian, size)
  override def toGamma: GammaStore =
    GammaStore(mem.toGamma, index.toBoogie, value.toGamma, size, size / mem.valueSize)

  override def gammas: Set[Expr] = Set()
  override def locals: Set[Variable] = index.locals ++ value.locals

  override def getType: IRType = BitVecType(size)
  override def toString: String = s"MemoryStore($mem, $index, $value, $endian, $size)"
}

class MemoryLoad(var mem: Memory, var index: Expr, var endian: Endian, var size: Int) extends Expr {
  override def toBoogie: BMemoryLoad = BMemoryLoad(mem.toBoogie, index.toBoogie, endian, size)
  override def toGamma: BExpr = if (mem.name == "stack") {
    GammaLoad(mem.toGamma, index.toBoogie, size, size / mem.valueSize)
  } else {
    BinaryBExpr(
      BoolOR,
      GammaLoad(mem.toGamma, index.toBoogie, size, size / mem.valueSize),
      L(mem.toBoogie, index.toBoogie)
    )
  }
  override def locals: Set[Variable] = index.locals
  override def gammas: Set[Expr] = Set(this)
  override def getType: IRType = BitVecType(size)
  override def toString: String = s"MemoryLoad($mem, $index, $endian, $size)"
}

case class Memory(name: String, addressSize: Int, valueSize: Int) extends Expr {
  override def toBoogie: BMapVar = BMapVar(name, MapBType(BitVecBType(addressSize), BitVecBType(valueSize)), Scope.Global)
  override def toGamma: BMapVar = BMapVar(s"Gamma_$name", MapBType(BitVecBType(addressSize), BoolBType), Scope.Global)
  override val getType: IRType = MapType(BitVecType(addressSize), BitVecType(valueSize))
  override def toString: String = s"Memory($name, $addressSize, $valueSize)"
}

case class Variable(name: String, irType: IRType) extends Expr {
  override def toGamma: BVar = BVariable(s"Gamma_$name", BoolBType, Scope.Local)
  override def toBoogie: BVar = BVariable(s"$name", irType.toBoogie, Scope.Local)
  override def getType: IRType = irType
  override def locals: Set[Variable] = Set(this)
  override def gammas: Set[Expr] = Set(this)

  def size: Int = irType match {
    case b: BitVecType => b.size
    case _ => throw new Exception("tried to get size of non-bitvector")
  }
  override def toString: String = s"Variable($name, $irType)"
}