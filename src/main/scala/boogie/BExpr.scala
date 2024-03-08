package boogie
import ir.*
import specification.*
import collection.mutable

import java.io.Writer

trait BExpr {
  def getType: BType
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
  def specGlobals: Set[SpecGlobalOrAccess] = Set()
  def oldSpecGlobals: Set[SpecGlobalOrAccess] = Set()
  def resolveSpec: BExpr = this
  def resolveOld: BExpr = this
  def removeOld: BExpr = this
  def resolveSpecL: BExpr = this
  def resolveInsideOld: BExpr = this
  def resolveSpecParam: BExpr = this
  def resolveSpecParamOld: BExpr = this
  def resolveSpecInv: BExpr = this
  def resolveSpecInvOld: BExpr = this
  def loads: Set[BExpr] = Set()
  def serialiseBoogie(w: Writer): Unit = w.append(toString)
}

trait BLiteral extends BExpr {}

sealed trait BoolBLiteral extends BLiteral

case object TrueBLiteral extends BoolBLiteral {
  override val getType: BType = BoolBType
  override def toString: String = "true"
}

case object StarBLiteral extends BoolBLiteral {
  override val getType: BType = BoolBType
  override def toString: String = "*"
}


case object FalseBLiteral extends BoolBLiteral {
  override val getType: BType = BoolBType
  override def toString: String = "false"
}

case class BitVecBLiteral(value: BigInt, size: Int) extends BLiteral {
  override val getType: BType = BitVecBType(size)
  override def toString: String = s"${value}bv$size"
}

case class IntBLiteral(value: BigInt) extends BLiteral {
  override val getType: BType = IntBType
  override def toString: String = value.toString
  override def resolveSpecL: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def resolveSpec: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def resolveOld: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def removeOld: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
}

case class BVExtract(end: Int, start: Int, body: BExpr) extends BExpr {
  override val getType: BitVecBType = BitVecBType(end - start)
  override def toString: String = s"$body[$end:$start]"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def resolveSpec: BVExtract = copy(body = body.resolveSpec)
  override def resolveSpecInv: BVExtract = copy(body = body.resolveSpecInv)
  override def resolveSpecInvOld: BVExtract = copy(body = body.resolveSpecInvOld)
  override def resolveSpecParam: BVExtract = copy(body = body.resolveSpecParam)
  override def resolveSpecParamOld: BVExtract = copy(body = body.resolveSpecParamOld)
  override def resolveSpecL: BVExtract = copy(body = body.resolveSpecL)
  override def resolveOld: BVExtract = copy(body = body.resolveOld)
  override def resolveInsideOld: BVExtract = copy(body = body.resolveInsideOld)
  override def removeOld: BVExtract = copy(body = body.removeOld)
  override def loads: Set[BExpr] = body.loads

  override def serialiseBoogie(w: Writer): Unit = {
    body.serialiseBoogie(w)
    w.append(s"[$end:$start]")
  }

}

case class BVRepeat(repeats: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize * repeats)


  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of extract: " + this)
  }
  private def fnName: String = s"repeat${repeats}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def serialiseBoogie(w: Writer): Unit = {
    w.append(fnName)
    w.append("(")
    body.serialiseBoogie(w)
    w.append(")")
  }

  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"repeat $repeats", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def resolveSpec: BVRepeat = copy(body = body.resolveSpec)
  override def resolveSpecInv: BVRepeat = copy(body = body.resolveSpecInv)
  override def resolveSpecInvOld: BVRepeat = copy(body = body.resolveSpecInvOld)
  override def resolveSpecParam: BVRepeat = copy(body = body.resolveSpecParam)
  override def resolveSpecParamOld: BVRepeat = copy(body = body.resolveSpecParamOld)
  override def resolveSpecL: BVRepeat = copy(body = body.resolveSpecL)
  override def resolveOld: BVRepeat = copy(body = body.resolveOld)
  override def resolveInsideOld: BVRepeat = copy(body = body.resolveInsideOld)
  override def removeOld: BVRepeat = copy(body = body.removeOld)
  override def loads: Set[BExpr] = body.loads
}

case class BVZeroExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }

  private def fnName: String = s"zero_extend${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def serialiseBoogie(w: Writer): Unit = {
    w.append(fnName)
    w.append("(")
    body.serialiseBoogie(w)
    w.append(")")
  }

  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"zero_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def resolveSpec: BVZeroExtend = copy(body = body.resolveSpec)
  override def resolveSpecInv: BVZeroExtend = copy(body = body.resolveSpecInv)
  override def resolveSpecInvOld: BVZeroExtend = copy(body = body.resolveSpecInvOld)
  override def resolveSpecParam: BVZeroExtend = copy(body = body.resolveSpecParam)
  override def resolveSpecParamOld: BVZeroExtend = copy(body = body.resolveSpecParamOld)
  override def resolveSpecL: BVZeroExtend = copy(body = body.resolveSpecL)
  override def resolveOld: BExpr = copy(body = body.resolveOld)
  override def resolveInsideOld: BExpr = copy(body = body.resolveInsideOld)
  override def removeOld: BExpr = copy(body = body.removeOld)
  override def loads: Set[BExpr] = body.loads
}

case class BVSignExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign extend: " + this)
  }

  private def fnName: String = s"sign_extend${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def serialiseBoogie(w: Writer): Unit = {
    w.append(fnName)
    w.append("(")
    body.serialiseBoogie(w)
    w.append(")")
  }


  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"sign_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def resolveSpecL: BVSignExtend = copy(body = body.resolveSpecL)
  override def resolveSpec: BVSignExtend = copy(body = body.resolveSpec)
  override def resolveSpecInv: BVSignExtend = copy(body = body.resolveSpecInv)
  override def resolveSpecInvOld: BVSignExtend = copy(body = body.resolveSpecInvOld)
  override def resolveSpecParam: BVSignExtend = copy(body = body.resolveSpecParam)
  override def resolveSpecParamOld: BVSignExtend = copy(body = body.resolveSpecParamOld)
  override def resolveOld: BExpr = copy(body = body.resolveOld)
  override def resolveInsideOld: BExpr = copy(body = body.resolveInsideOld)
  override def removeOld: BExpr = copy(body = body.removeOld)
  override def loads: Set[BExpr] = body.loads
}

abstract class BVar(val name: String, val bType: BType, val scope: Scope) extends BExpr with Ordered[BVar] {
  def compare(that: BVar): Int = this.name.compare(that.name)

  override def getType: BType = bType
  override def toString: String = name
  def withType: String = if (name.isEmpty) {
    s"$bType"
  } else {
    s"$name: $bType"
  }
  override def locals: Set[BVar] = scope match {
    case Scope.Local => Set(this)
    case _           => Set()
  }
  override def globals: Set[BVar] = scope match {
    case Scope.Global => Set(this)
    case _            => Set()
  }
}

case class BVariable(override val name: String, override val bType: BType, override val scope: Scope)
    extends BVar(name, bType, scope) {}

enum Scope {
  case Local
  case Global
  case Parameter
  case Const
}

object BParam {
  def apply(bType: BType): BVariable = BVariable("", bType, Scope.Parameter)
  def apply(name: String, bType: BType): BVariable = BVariable(name, bType, Scope.Parameter)
}

case class BMapVar(override val name: String, override val bType: MapBType, override val scope: Scope)
    extends BVar(name, bType, scope) {
  override val getType: MapBType = bType
}

case class BFunctionCall(name: String, args: List[BExpr], bType: BType) extends BExpr {
  override val getType: BType = bType
  override def toString: String = s"$name(${args.mkString(", ")})"
  override def functionOps: Set[FunctionOp] = args.flatMap(a => a.functionOps).toSet
  override def locals: Set[BVar] = args.flatMap(a => a.locals).toSet
  override def globals: Set[BVar] = args.flatMap(a => a.globals).toSet
  override def specGlobals: Set[SpecGlobalOrAccess] = args.flatMap(a => a.specGlobals).toSet
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = args.flatMap(a => a.oldSpecGlobals).toSet
  override def resolveSpec: BFunctionCall = copy(args = args.map(a => a.resolveSpec))
  override def resolveSpecInv: BFunctionCall = copy(args = args.map(a => a.resolveSpecInv))
  override def resolveSpecInvOld: BFunctionCall = copy(args = args.map(a => a.resolveSpecInvOld))
  override def resolveSpecParam: BFunctionCall = copy(args = args.map(a => a.resolveSpecParam))
  override def resolveSpecParamOld: BFunctionCall = copy(args = args.map(a => a.resolveSpecParamOld))
  override def resolveSpecL: BFunctionCall = copy(args = args.map(a => a.resolveSpecL))
  override def resolveOld: BExpr = copy(args = args.map(a => a.resolveOld))
  override def removeOld: BExpr = copy(args = args.map(a => a.removeOld))
  override def loads: Set[BExpr] = args.flatMap(a => a.loads).toSet
}

case class UnaryBExpr(op: UnOp, arg: BExpr) extends BExpr {
  override def getType: BType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolBType)     => BoolBType
    case (_: BVUnOp, bv: BitVecBType) => bv
    case (_: IntUnOp, IntBType)       => IntBType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecBType => bv.size
    case _               => throw new Exception(s"Expected Bv but got ${arg.getType}")
  }

  override def toString: String = op match {
    case uOp: BoolUnOp => s"($uOp$arg)"
    case uOp: BVUnOp   => s"bv$uOp$inSize($arg)"
    case uOp: IntUnOp  => s"($uOp$arg)"
  }

  override def functionOps: Set[FunctionOp] = {
    val thisFn = op match {
      case b: BVUnOp =>
        Set(BVFunctionOp(s"bv$b$inSize", s"bv$b", List(BParam(arg.getType)), BParam(getType)))
      case _ => Set()
    }
    arg.functionOps ++ thisFn
  }

  override def locals: Set[BVar] = arg.locals
  override def globals: Set[BVar] = arg.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = arg.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = arg.oldSpecGlobals
  override def resolveSpec: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpec)
    case _          => copy(arg = arg.resolveSpec)
  }
  override def resolveSpecInv: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecInv)
    case _ => copy(arg = arg.resolveSpecInv)
  }
  override def resolveSpecInvOld: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecInvOld)
    case _ => copy(arg = arg.resolveSpecInvOld)
  }
  override def resolveSpecParam: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecParam)
    case _ => copy(arg = arg.resolveSpecParam)
  }
  override def resolveSpecParamOld: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecParamOld)
    case _ => copy(arg = arg.resolveSpecParamOld)
  }
  override def resolveSpecL: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecL)
    case _          => copy(arg = arg.resolveSpecL)
  }
  override def resolveOld: BExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveOld)
    case _          => copy(arg = arg.resolveOld)
  }
  override def resolveInsideOld: BExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveInsideOld)
    case _          => copy(arg = arg.resolveInsideOld)
  }
  override def removeOld: BExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.removeOld)
    case _          => copy(arg = arg.removeOld)
  }
  override def loads: Set[BExpr] = arg.loads
}

case class BinaryBExpr(op: BinOp, arg1: BExpr, arg2: BExpr) extends BExpr {
  override def getType: BType = (op, arg1.getType, arg2.getType) match {
    case (_: BoolBinOp, BoolBType, BoolBType) => BoolBType
    case (binOp: BVBinOp, bv1: BitVecBType, bv2: BitVecBType) =>
      binOp match {
        case BVCONCAT =>
          BitVecBType(bv1.size + bv2.size)
        case BVAND | BVOR | BVADD | BVMUL | BVUDIV | BVUREM | BVSHL | BVLSHR | BVNAND | BVNOR | BVXOR | BVXNOR | BVSUB |
            BVSREM | BVSDIV | BVSMOD | BVASHR =>
          if (bv1.size == bv2.size) {
            bv1
          } else {
            throw new Exception(s"bitvector size mismatch: $arg1, $arg2")
          }
        case BVCOMP =>
          if (bv1.size == bv2.size) {
            BitVecBType(1)
          } else {
            throw new Exception(s"bitvector size mismatch: $arg1, $arg2")
          }
        case BVULT | BVULE | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
          if (bv1.size == bv2.size) {
            BoolBType
          } else {
            throw new Exception(s"bitvector size mismatch: $arg1, $arg2")
          }
        case BVEQ | BVNEQ =>
          BoolBType
      }
    case (intOp: IntBinOp, IntBType, IntBType) =>
      intOp match {
        case IntADD | IntSUB | IntMUL | IntDIV | IntMOD     => IntBType
        case IntEQ | IntNEQ | IntLT | IntLE | IntGT | IntGE => BoolBType
      }
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  private def inSize = arg1.getType match {
    case bv: BitVecBType => bv.size
    case _               => throw new Exception("type mismatch")
  }

  override def serialiseBoogie(w: Writer): Unit = {
    val traversalQueue = mutable.Stack[BExpr | BinOp | String]()
    traversalQueue.append(this)

    while (traversalQueue.nonEmpty) {
      val next = traversalQueue.pop()

      def infix(b: BinaryBExpr): Unit = traversalQueue.pushAll(Seq("(", b.arg1, s" ${b.op} ", b.arg2, ")").reverse)
      def prefix(b: BinaryBExpr): Unit = traversalQueue.pushAll(Seq(s"bv${b.op}${b.inSize}(", b.arg1, ",", b.arg2, ")").reverse)

      next match
        case b: BinaryBExpr =>
          b.op match {
            case bOp: BoolBinOp => infix(b)
            case bOp: BVBinOp => bOp match {
                case BVEQ | BVNEQ | BVCONCAT => infix(b)
                case _ => prefix(b)
              }
            case bOp: IntBinOp => infix(b)
          }
        case b: BExpr => b.serialiseBoogie(w)
        case b: BinOp => w.append(b.toString)
        case s: String => w.append(s)
    }
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



  override def functionOps: Set[FunctionOp] = {
    val thisFn = op match {
      case b: BVBinOp =>
        b match {
          case BVEQ | BVNEQ | BVCONCAT => Set()
          case _ =>
            Set(
              BVFunctionOp(s"bv$b$inSize", s"bv$b", List(BParam(arg1.getType), BParam(arg2.getType)), BParam(getType))
            )
        }
      case _ => Set()
    }
    arg1.functionOps ++ arg2.functionOps ++ thisFn
  }

  override def locals: Set[BVar] = arg1.locals ++ arg2.locals
  override def globals: Set[BVar] = arg1.globals ++ arg2.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = arg1.specGlobals ++ arg2.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = arg1.oldSpecGlobals ++ arg2.oldSpecGlobals

  override def resolveSpec: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpec, arg2 = arg2.resolveSpec)
    case _           => copy(arg1 = arg1.resolveSpec, arg2 = arg2.resolveSpec)
  }

  override def resolveSpecInv: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpecInv, arg2 = arg2.resolveSpecInv)
    case _ => copy(arg1 = arg1.resolveSpecInv, arg2 = arg2.resolveSpecInv)
  }

  override def resolveSpecInvOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpecInvOld, arg2 = arg2.resolveSpecInvOld)
    case _ => copy(arg1 = arg1.resolveSpecInvOld, arg2 = arg2.resolveSpecInvOld)
  }

  override def resolveSpecParamOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpec, arg2 = arg2.resolveSpecParamOld)
    case _ => copy(arg1 = arg1.resolveSpecParamOld, arg2 = arg2.resolveSpecParamOld)
  }

  override def resolveSpecParam: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpecParam, arg2 = arg2.resolveSpecParam)
    case _ => copy(arg1 = arg1.resolveSpecParam, arg2 = arg2.resolveSpecParam)
  }

  override def resolveSpecL: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpecL, arg2 = arg2.resolveSpecL)
    case _           => copy(arg1 = arg1.resolveSpecL, arg2 = arg2.resolveSpecL)
  }

  override def resolveOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveOld, arg2 = arg2.resolveOld)
    case _           => copy(arg1 = arg1.resolveOld, arg2 = arg2.resolveOld)
  }

  override def resolveInsideOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveInsideOld, arg2 = arg2.resolveInsideOld)
    case _           => copy(arg1 = arg1.resolveInsideOld, arg2 = arg2.resolveInsideOld)
  }

  override def removeOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.removeOld, arg2 = arg2.removeOld)
    case _           => copy(arg1 = arg1.removeOld, arg2 = arg2.removeOld)
  }
  override def loads: Set[BExpr] = arg1.loads ++ arg2.loads
}

case class IfThenElse(guard: BExpr, thenExpr: BExpr, elseExpr: BExpr) extends BExpr {
  override def getType: BType = {
    if (thenExpr.getType == elseExpr.getType) {
      thenExpr.getType
    } else {
      throw new Exception("type mismatch")
    }
  }

  override def toString: String = s"(if $guard then $thenExpr else $elseExpr)"
  override def functionOps: Set[FunctionOp] = guard.functionOps ++ thenExpr.functionOps ++ elseExpr.functionOps
  override def locals: Set[BVar] = guard.locals ++ thenExpr.locals ++ elseExpr.locals
  override def globals: Set[BVar] = guard.globals ++ thenExpr.globals ++ elseExpr.globals
  override def specGlobals: Set[SpecGlobalOrAccess] = guard.specGlobals ++ thenExpr.specGlobals ++ elseExpr.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] =
    guard.oldSpecGlobals ++ thenExpr.oldSpecGlobals ++ elseExpr.oldSpecGlobals
  override def resolveSpec: IfThenElse =
    copy(guard = guard.resolveSpec, thenExpr = thenExpr.resolveSpec, elseExpr = elseExpr.resolveSpec)
  override def resolveSpecInv: IfThenElse =
    copy(guard = guard.resolveSpecInv, thenExpr = thenExpr.resolveSpecInv, elseExpr = elseExpr.resolveSpecInv)
  override def resolveSpecInvOld: IfThenElse =
    copy(guard = guard.resolveSpecInvOld, thenExpr = thenExpr.resolveSpecInvOld, elseExpr = elseExpr.resolveSpecInvOld)
  override def resolveSpecParam: IfThenElse =
    copy(guard = guard.resolveSpecParam, thenExpr = thenExpr.resolveSpecParam, elseExpr = elseExpr.resolveSpecParam)
  override def resolveSpecParamOld: IfThenElse =
    copy(guard = guard.resolveSpecParamOld, thenExpr = thenExpr.resolveSpecParamOld, elseExpr = elseExpr.resolveSpecParamOld)
  override def resolveSpecL: IfThenElse =
    copy(guard = guard.resolveSpecL, thenExpr = thenExpr.resolveSpecL, elseExpr = elseExpr.resolveSpecL)
  override def resolveOld: IfThenElse =
    copy(guard = guard.resolveOld, thenExpr = thenExpr.resolveOld, elseExpr = elseExpr.resolveOld)
  override def resolveInsideOld: IfThenElse =
    copy(guard = guard.resolveInsideOld, thenExpr = thenExpr.resolveInsideOld, elseExpr = elseExpr.resolveInsideOld)
  override def removeOld: IfThenElse =
    copy(guard = guard.removeOld, thenExpr = thenExpr.removeOld, elseExpr = elseExpr.removeOld)
  override def loads: Set[BExpr] = guard.loads ++ thenExpr.loads ++ elseExpr.loads
}

trait QuantifierExpr(sort: Quantifier, bound: List[BVar], body: BExpr) extends BExpr {
  override def toString: String = {
    val boundString = bound.map(_.withType).mkString(", ")
    s"($sort $boundString :: ($body))"
  }
  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals -- bound.toSet
  override def globals: Set[BVar] = body.globals -- bound.toSet
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def loads: Set[BExpr] = body.loads
}

enum Quantifier {
  case forall
  case exists
  case lambda
}

case class ForAll(bound: List[BVar], body: BExpr) extends QuantifierExpr(Quantifier.forall, bound, body)

case class Exists(bound: List[BVar], body: BExpr) extends QuantifierExpr(Quantifier.exists, bound, body)

case class Lambda(bound: List[BVar], body: BExpr) extends QuantifierExpr(Quantifier.lambda, bound, body)

case class Old(body: BExpr) extends BExpr {
  override def toString: String = s"old($body)"
  override def getType: BType = body.getType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def resolveSpecParam: BExpr = body.resolveSpecParamOld
  override def resolveSpecInv: BExpr = body.resolveSpecInvOld
  override def resolveSpec: BExpr = copy(body = body.resolveSpec)
  override def resolveSpecL: BExpr = copy(body = body.resolveSpecL)
  override def resolveOld: BExpr = body.resolveInsideOld
  override def removeOld: BExpr = body.resolveSpec
  override def loads: Set[BExpr] = body.loads
}

case class MapAccess(mapVar: BMapVar, index: BExpr) extends BExpr {
  override def toString: String = s"$mapVar[$index]"
  override val getType: BType = mapVar.getType.result
  override def functionOps: Set[FunctionOp] = index.functionOps
  override def locals: Set[BVar] = index.locals
  override def globals: Set[BVar] = index.globals ++ mapVar.globals
  override def loads: Set[BExpr] = index.loads
}

case class MapUpdate(map: BExpr, index: BExpr, value: BExpr) extends BExpr {
  override def toString = s"$map[$index := $value]"
  override val getType: BType = map.getType
  override def functionOps: Set[FunctionOp] = map.functionOps ++ index.functionOps ++ value.functionOps
  override def locals: Set[BVar] = map.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ map.globals ++ value.globals
  override def loads: Set[BExpr] = index.loads ++ value.loads ++ map.loads
}

sealed trait FunctionOp

case class BVFunctionOp(name: String, bvbuiltin: String, in: List[BVar], out: BVar) extends FunctionOp {
  def attribute: BAttribute = BAttribute("bvbuiltin", Some(s"\"$bvbuiltin\""))
}

case class MemoryLoadOp(addressSize: Int, valueSize: Int, endian: Endian, bits: Int) extends FunctionOp {
  val accesses: Int = bits / valueSize

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_load${bits}_le"
    case Endian.BigEndian    => s"memory_load${bits}_be"
  }
}
case class MemoryStoreOp(addressSize: Int, valueSize: Int, endian: Endian, bits: Int) extends FunctionOp {
  val accesses: Int = bits / valueSize

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_store${bits}_le"
    case Endian.BigEndian    => s"memory_store${bits}_be"
  }
}
case class GammaLoadOp(addressSize: Int, bits: Int, accesses: Int) extends FunctionOp {
  val fnName: String = s"gamma_load$bits"
}
case class GammaStoreOp(addressSize: Int, bits: Int, accesses: Int) extends FunctionOp {
  val fnName: String = s"gamma_store$bits"
}
case class LOp(memoryType: BType, indexType: BType) extends FunctionOp

/**
 * Utility to extract a particular byte from a bitvector.
 */
case class ByteExtract(valueSize: Int, offsetSize: Int) extends FunctionOp {
  val fnName: String = s"byte_extract${valueSize}_${offsetSize}"
}

case class BByteExtract(value: BExpr, offset: BExpr) extends BExpr {
  override def toString: String = s"$fnName($value, $offset)"

  val valueSize: Int = value.getType match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"ByteExtract does not have Bitvector type: $this")
  }

  val offsetSize: Int = offset.getType match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"ByteExtract does not have Bitvector type: $this")
  }

  val fnName: String = s"byte_extract${valueSize}_${offsetSize}"

  override val getType: BType = BitVecBType(8)
  override def functionOps: Set[FunctionOp] =
    value.functionOps ++ offset.functionOps + ByteExtract(valueSize, offsetSize)
  override def locals: Set[BVar] = value.locals ++ offset.locals
  override def globals: Set[BVar] = value.globals ++ offset.globals
  override def loads: Set[BExpr] = value.loads ++ offset.loads
}

/**
 * Utility to test if a particular value i is within the bounds of a base variable
 * and some length. Factors in the problem of wrap around, given the base + length
 * exceeds the bitvector size.
 *
 * Assumes all inputs are of the same bitvector width.
 */
case class InBounds(bits: Int, endian: Endian) extends FunctionOp {
  val fnName: String = endian match {
    case Endian.LittleEndian => s"in_bounds${bits}_le"
    case Endian.BigEndian=> s"in_bounds${bits}_be"
  }
}

case class BInBounds(base: BExpr, len: BExpr, endian: Endian, i: BExpr) extends BExpr {
  override def toString: String = s"$fnName($base, $len, $i)"

  val baseSize: Int = base.getType match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"InBounds does not have Bitvector type: $this")
  }

  val fnName: String = s"in_bounds${baseSize}"

  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] =
    base.functionOps ++ len.functionOps ++ i.functionOps + InBounds(baseSize, endian)
  override def locals: Set[BVar]  = base.locals ++ len.locals ++ i.locals
  override def globals: Set[BVar] = base.globals ++ len.globals ++ i.globals 
  override def loads: Set[BExpr]  = base.loads ++ len.loads ++ i.loads 
}

case class BMemoryLoad(memory: BMapVar, index: BExpr, endian: Endian, bits: Int) extends BExpr {
  override def toString: String = s"$fnName($memory, $index)"

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_load${bits}_le"
    case Endian.BigEndian    => s"memory_load${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"MemoryLoad does not have Bitvector type: $this")
  }

  override val getType: BType = BitVecBType(bits)
  override def functionOps: Set[FunctionOp] =
    memory.functionOps ++ index.functionOps + MemoryLoadOp(addressSize, valueSize, endian, bits)
  override def locals: Set[BVar] = memory.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals
  override def loads: Set[BExpr] = Set(this) ++ index.loads
}

case class BMemoryStore(memory: BMapVar, index: BExpr, value: BExpr, endian: Endian, bits: Int) extends BExpr {
  override def toString: String = s"$fnName($memory, $index, $value)"

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_store${bits}_le"
    case Endian.BigEndian    => s"memory_store${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  override val getType: BType = memory.getType
  override def functionOps: Set[FunctionOp] =
    memory.functionOps ++ index.functionOps ++ value.functionOps + MemoryStoreOp(addressSize, valueSize, endian, bits)
  override def locals: Set[BVar] = memory.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals ++ value.globals
  override def loads: Set[BExpr] = index.loads ++ value.loads
}

case class BDirectExpr(text: String, t: BType) extends BExpr {
  override def toString: String = text
  override def getType: BType = t
}

case class GammaLoad(gammaMap: BMapVar, index: BExpr, bits: Int, accesses: Int) extends BExpr {
  override def toString: String = s"$fnName($gammaMap, $index)"
  val fnName: String = s"gamma_load$bits"

  val addressSize: Int = gammaMap.getType.param match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"GammaLoad does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] =
    gammaMap.functionOps ++ index.functionOps + GammaLoadOp(addressSize, bits, accesses)
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals
  override def loads: Set[BExpr] = Set(this) ++ index.loads
}

case class GammaStore(gammaMap: BMapVar, index: BExpr, value: BExpr, bits: Int, accesses: Int) extends BExpr {
  override def toString: String = s"$fnName($gammaMap, $index, $value)"
  val fnName: String = s"gamma_store$bits"

  val addressSize: Int = gammaMap.getType.param match {
    case b: BitVecBType => b.size
    case _              => throw new Exception(s"GammaStore does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override val getType: BType = gammaMap.getType
  override def functionOps: Set[FunctionOp] =
    gammaMap.functionOps ++ index.functionOps ++ value.functionOps + GammaStoreOp(addressSize, bits, accesses)
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals ++ value.globals
  override def loads: Set[BExpr] = index.loads ++ value.loads
}

case class L(memory: BMapVar, index: BExpr) extends BExpr {
  override def toString: String = s"L($memory, $index)"
  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = index.functionOps + LOp(memory.getType, index.getType)
  override def locals: Set[BVar] = index.locals
  override def globals: Set[BVar] = index.globals
  override def loads: Set[BExpr] = index.loads
}
