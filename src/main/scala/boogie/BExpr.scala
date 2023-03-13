package boogie
import ir._
import specification._

trait BExpr {
  def getType: BType
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
  def replaceReserved(reserved: Set[String]): BExpr
  def specGlobals: Set[SpecGlobal] = Set()
  def oldSpecGlobals: Set[SpecGlobal] = Set()
  def resolveSpec: BExpr = this
  def resolveOld: BExpr = this
  def removeOld: BExpr = this
  def resolveSpecL: BExpr = this
}

trait BLiteral extends BExpr {
  override def replaceReserved(reserved: Set[String]): BLiteral = this
}

sealed trait BoolBLiteral extends BLiteral

case object TrueBLiteral extends BoolBLiteral {
  override def getType: BType = BoolBType
  override def toString: String = "true"
}

case object FalseBLiteral extends BoolBLiteral {
  override def getType: BType = BoolBType
  override def toString: String = "false"
}

case class BitVecBLiteral(value: BigInt, size: Int) extends BLiteral {
  override def getType: BType = BitVecBType(size)
  override def toString: String = s"${value}bv$size"
}

case class IntBLiteral(value: BigInt) extends BLiteral {
  override def getType: BType = IntBType
  override def toString: String = value.toString
  override def resolveSpecL: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def resolveSpec: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def resolveOld: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
  override def removeOld: BitVecBLiteral = BitVecBLiteral(value, 32) // TODO
}

case class BVExtract(end: Int, start: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(end - start)
  override def toString: String = s"$body[$end:$start]"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobal] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = body.oldSpecGlobals
  override def replaceReserved(reserved: Set[String]): BVExtract = copy(body = body.replaceReserved(reserved))
  override def resolveSpec: BVExtract = copy(body = body.resolveSpec)
  override def resolveSpecL: BVExtract = copy(body = body.resolveSpecL)
  override def resolveOld: BVExtract = copy(body = body.resolveOld)
  override def removeOld: BVExtract = copy(body = body.removeOld)
}

case class BVRepeat(repeats: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize * repeats)

  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _          => throw new Exception("type mismatch, non bv expression: " + body + " in body of extract: " + this)
  }
  private def fnName: String = s"repeat${repeats}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"repeat $repeats", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobal] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = body.oldSpecGlobals
  override def resolveSpec: BVRepeat = copy(body = body.resolveSpec)
  override def resolveSpecL: BVRepeat = copy(body = body.resolveSpecL)
  override def resolveOld: BVRepeat = copy(body = body.resolveOld)
  override def removeOld: BVRepeat = copy(body = body.removeOld)
  override def replaceReserved(reserved: Set[String]): BVRepeat = copy(body = body.replaceReserved(reserved))
}

case class BVZeroExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }

  private def fnName: String = s"zero_extend${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"zero_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobal] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = body.oldSpecGlobals
  override def resolveSpec: BVZeroExtend = copy(body = body.resolveSpec)
  override def resolveSpecL: BVZeroExtend = copy(body = body.resolveSpecL)
  override def resolveOld: BExpr = copy(body = body.resolveOld)
  override def removeOld: BExpr = copy(body = body.removeOld)
  override def replaceReserved(reserved: Set[String]): BVZeroExtend = copy(body = body.replaceReserved(reserved))
}

case class BVSignExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVecBType = BitVecBType(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign extend: " + this)
  }

  private def fnName: String = s"sign_extend${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def functionOps: Set[FunctionOp] = {
    val thisFn = BVFunctionOp(fnName, s"sign_extend $extension", List(BParam(BitVecBType(bodySize))), BParam(getType))
    body.functionOps + thisFn
  }
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def specGlobals: Set[SpecGlobal] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = body.oldSpecGlobals
  override def resolveSpecL: BVSignExtend = copy(body = body.resolveSpecL)
  override def resolveSpec: BVSignExtend = copy(body = body.resolveSpec)
  override def resolveOld: BExpr = copy(body = body.resolveOld)
  override def removeOld: BExpr = copy(body = body.removeOld)
  override def replaceReserved(reserved: Set[String]): BVSignExtend = copy(body = body.replaceReserved(reserved))
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
  override def replaceReserved(reserved: Set[String]): BVar
}

case class BVariable(override val name: String, override val bType: BType, override val scope: Scope)
    extends BVar(name, bType, scope) {
  override def replaceReserved(reserved: Set[String]): BVariable = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    copy(name = nameUpdate)
  }
}

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
  override def getType: MapBType = bType
  override def replaceReserved(reserved: Set[String]): BMapVar = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    copy(name = nameUpdate)
  }
}

case class BFunctionCall(name: String, args: List[BExpr], bType: BType) extends BExpr {
  override def getType: BType = bType
  override def toString: String = s"$name(${args.mkString(", ")})"
  override def functionOps: Set[FunctionOp] = args.flatMap(a => a.functionOps).toSet
  override def locals: Set[BVar] = args.flatMap(a => a.locals).toSet
  override def globals: Set[BVar] = args.flatMap(a => a.globals).toSet
  override def specGlobals: Set[SpecGlobal] = args.flatMap(a => a.specGlobals).toSet
  override def oldSpecGlobals: Set[SpecGlobal] = args.flatMap(a => a.oldSpecGlobals).toSet
  override def resolveSpec: BFunctionCall = copy(args = args.map(a => a.resolveSpec))
  override def resolveSpecL: BFunctionCall = copy(args = args.map(a => a.resolveSpecL))
  override def resolveOld: BExpr = copy(args = args.map(a => a.resolveOld))
  override def removeOld: BExpr = copy(args = args.map(a => a.removeOld))
  override def replaceReserved(reserved: Set[String]): BFunctionCall = {
    val nameUpdate = if (reserved.contains(name)) {
      '#' + name
    } else {
      name
    }
    val argsUpdate = args.map(a => a.replaceReserved(reserved))
    copy(name = nameUpdate, args = argsUpdate)
  }
}

case class UnaryBExpr(op: UnOp, arg: BExpr) extends BExpr {
  override def getType: BType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolBType) => BoolBType
    case (_: BVUnOp, bv: BitVecBType) => bv
    case (_: IntUnOp, IntBType)   => IntBType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecBType => bv.size
    case _          => throw new Exception("type mismatch")
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
  override def specGlobals: Set[SpecGlobal] = arg.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = arg.oldSpecGlobals
  override def resolveSpec: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpec)
    case _          => copy(arg = arg.resolveSpec)
  }
  override def resolveSpecL: UnaryBExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveSpecL)
    case _          => copy(arg = arg.resolveSpecL)
  }
  override def resolveOld: BExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.resolveOld)
    case _          => copy(arg = arg.resolveOld)
  }
  override def removeOld: BExpr = op match {
    case i: IntUnOp => copy(op = i.toBV, arg = arg.removeOld)
    case _          => copy(arg = arg.removeOld)
  }
  override def replaceReserved(reserved: Set[String]): UnaryBExpr = {
    copy(arg = arg.replaceReserved(reserved))
  }
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
            //println(arg1)
            //println(arg2)
            //println(this)
            throw new Exception("bitvector size mismatch")
          }
        case BVCOMP =>
          if (bv1.size == bv2.size) {
            BitVecBType(1)
          } else {
            throw new Exception("bitvector size mismatch")
            BitVecBType(1)
          }
        case BVULT | BVULE | BVUGT | BVUGE | BVSLT | BVSLE | BVSGT | BVSGE =>
          if (bv1.size == bv2.size) {
            BoolBType
          } else {
            //println(arg1)
            //println(arg2)
            throw new Exception("bitvector size mismatch")
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
    case _          => throw new Exception("type mismatch")
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
  override def specGlobals: Set[SpecGlobal] = arg1.specGlobals ++ arg2.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = arg1.oldSpecGlobals ++ arg2.oldSpecGlobals

  override def resolveSpec: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpec, arg2 = arg2.resolveSpec)
    case _           => copy(arg1 = arg1.resolveSpec, arg2 = arg2.resolveSpec)
  }

  override def resolveSpecL: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveSpecL, arg2 = arg2.resolveSpecL)
    case _           => copy(arg1 = arg1.resolveSpecL, arg2 = arg2.resolveSpecL)
  }

  override def resolveOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.resolveOld, arg2 = arg2.resolveOld)
    case _           => copy(arg1 = arg1.resolveOld, arg2 = arg2.resolveOld)
  }

  override def removeOld: BinaryBExpr = op match {
    case i: IntBinOp => copy(op = i.toBV, arg1 = arg1.removeOld, arg2 = arg2.removeOld)
    case _           => copy(arg1 = arg1.removeOld, arg2 = arg2.removeOld)
  }

  override def replaceReserved(reserved: Set[String]): BinaryBExpr = {
    copy(arg1 = arg1.replaceReserved(reserved), arg2 = arg2.replaceReserved(reserved))
  }
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
  override def specGlobals: Set[SpecGlobal] = guard.specGlobals ++ thenExpr.specGlobals ++ elseExpr.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = guard.oldSpecGlobals ++ thenExpr.oldSpecGlobals ++ elseExpr.oldSpecGlobals
  override def resolveSpec: IfThenElse =
    copy(guard = guard.resolveSpec, thenExpr = thenExpr.resolveSpec, elseExpr = elseExpr.resolveSpec)
  override def resolveSpecL: IfThenElse =
    copy(guard = guard.resolveSpecL, thenExpr = thenExpr.resolveSpecL, elseExpr = elseExpr.resolveSpecL)
  override def resolveOld: IfThenElse =
    copy(guard = guard.resolveOld, thenExpr = thenExpr.resolveOld, elseExpr = elseExpr.resolveOld)
  override def removeOld: IfThenElse =
    copy(guard = guard.removeOld, thenExpr = thenExpr.removeOld, elseExpr = elseExpr.removeOld)
  override def replaceReserved(reserved: Set[String]): IfThenElse = {
    copy(
      guard = guard.replaceReserved(reserved),
      thenExpr = thenExpr.replaceReserved(reserved),
      elseExpr = elseExpr.replaceReserved(reserved)
    )
  }
}

trait QuantifierExpr(sort: Quantifier, bound: List[BVar], body: BExpr) extends BExpr {
  override def toString: String = {
    val boundString = bound.map(_.withType).mkString(", ")
    s"($sort $boundString :: ($body))"
  }
  override def getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals -- bound.toSet
  override def globals: Set[BVar] = body.globals -- bound.toSet
  override def specGlobals: Set[SpecGlobal] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobal] = body.oldSpecGlobals
}

enum Quantifier {
  case forall
  case exists
}

case class ForAll(bound: List[BVar], body: BExpr) extends QuantifierExpr(Quantifier.forall, bound, body) {
  override def replaceReserved(reserved: Set[String]): ForAll = {
    val boundUpdate = bound.map(b => b.replaceReserved(reserved))
    val bodyUpdate = body.replaceReserved(reserved)
    copy(bound = boundUpdate, body = bodyUpdate)
  }
}

case class Exists(bound: List[BVar], body: BExpr) extends QuantifierExpr(Quantifier.exists, bound, body) {
  override def replaceReserved(reserved: Set[String]): Exists = {
    val boundUpdate = bound.map(b => b.replaceReserved(reserved))
    val bodyUpdate = body.replaceReserved(reserved)
    copy(bound = boundUpdate, body = bodyUpdate)
  }
}

case class Old(body: BExpr) extends BExpr {
  override def toString: String = s"old($body)"
  override def getType: BType = body.getType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def oldSpecGlobals: Set[SpecGlobal] = body.specGlobals
  override def resolveSpec: BExpr = copy(body = body.resolveSpec)
  override def resolveSpecL: BExpr = copy(body = body.resolveSpecL)
  override def resolveOld: BExpr = body match {
    case s: SpecGlobal => s.toOldVar
    case s: SpecGamma  => s.global.toOldGamma
    case _             => this // TODO
  }
  override def removeOld: BExpr = body.resolveSpec
  override def replaceReserved(reserved: Set[String]): Old = copy(body = body.replaceReserved(reserved))
}

case class MapAccess(mapVar: BMapVar, index: BExpr) extends BExpr {
  override def toString: String = s"$mapVar[$index]"
  override def getType: BType = mapVar.getType.result
  override def functionOps: Set[FunctionOp] = index.functionOps
  override def locals: Set[BVar] = index.locals
  override def globals: Set[BVar] = index.globals ++ mapVar.globals
  override def replaceReserved(reserved: Set[String]): MapAccess =
    copy(mapVar = mapVar.replaceReserved(reserved), index = index.replaceReserved(reserved))
}

case class MapUpdate(map: BExpr, index: BExpr, value: BExpr) extends BExpr {
  override def toString = s"$map[$index := $value]"
  override def getType: BType = map.getType
  override def functionOps: Set[FunctionOp] = map.functionOps ++ index.functionOps ++ value.functionOps
  override def locals: Set[BVar] = map.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ map.globals ++ value.globals
  override def replaceReserved(reserved: Set[String]): MapUpdate =
    copy(
      map = map.replaceReserved(reserved),
      index = index.replaceReserved(reserved),
      value = value.replaceReserved(reserved)
    )
}

sealed trait FunctionOp

case class BVFunctionOp(name: String, bvbuiltin: String, in: List[BVar], out: BVar) extends FunctionOp

case class BMemoryLoad(memory: BMapVar, index: BExpr, endian: Endian, bits: Int) extends BExpr with FunctionOp {
  override def toString: String = s"$fnName($memory, $index)"

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_load${bits}_le"
    case Endian.BigEndian    => s"memory_load${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"MemoryLoad does not have Bitvector type: $this")
  }

  val accesses: Int = bits / valueSize

  override def getType: BType = BitVecBType(bits)
  override def functionOps: Set[FunctionOp] = memory.functionOps ++ index.functionOps + this
  override def locals: Set[BVar] = memory.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals
  override def replaceReserved(reserved: Set[String]): BMemoryLoad =
    copy(memory = memory.replaceReserved(reserved), index = index.replaceReserved(reserved))

}

case class BMemoryStore(memory: BMapVar, index: BExpr, value: BExpr, endian: Endian, bits: Int)
    extends BExpr
    with FunctionOp {
  override def toString: String = s"$fnName($memory, $index, $value)"

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_store${bits}_le"
    case Endian.BigEndian    => s"memory_store${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val accesses: Int = bits / valueSize

  override def getType: BType = memory.getType
  override def functionOps: Set[FunctionOp] = memory.functionOps ++ index.functionOps ++ value.functionOps + this
  override def locals: Set[BVar] = memory.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals ++ value.globals
  override def replaceReserved(reserved: Set[String]): BMemoryStore =
    copy(
      memory = memory.replaceReserved(reserved),
      index = index.replaceReserved(reserved),
      value = value.replaceReserved(reserved)
    )
}

case class GammaLoad(gammaMap: BMapVar, index: BExpr, bits: Int, accesses: Int) extends BExpr with FunctionOp {
  override def toString: String = s"$fnName($gammaMap, $index)"
  val fnName: String = s"gamma_load$bits"

  val addressSize: Int = gammaMap.getType.param match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"GammaLoad does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override def getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = gammaMap.functionOps ++ index.functionOps + this
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals
  override def replaceReserved(reserved: Set[String]): GammaLoad =
    copy(gammaMap = gammaMap.replaceReserved(reserved), index = index.replaceReserved(reserved))

}

case class GammaStore(gammaMap: BMapVar, index: BExpr, value: BExpr, bits: Int, accesses: Int)
    extends BExpr
    with FunctionOp {
  override def toString: String = s"$fnName($gammaMap, $index, $value)"
  val fnName: String = s"gamma_store$bits"

  val addressSize: Int = gammaMap.getType.param match {
    case b: BitVecBType => b.size
    case _         => throw new Exception(s"GammaStore does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override def getType: BType = gammaMap.getType
  override def functionOps: Set[FunctionOp] = gammaMap.functionOps ++ index.functionOps ++ value.functionOps + this
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals ++ value.globals
  override def replaceReserved(reserved: Set[String]): GammaStore =
    copy(
      gammaMap = gammaMap.replaceReserved(reserved),
      index = index.replaceReserved(reserved),
      value = value.replaceReserved(reserved)
    )
}

case class L(memory: BMapVar, index: BExpr) extends BExpr with FunctionOp {
  override def toString: String = s"L($memory, $index)"
  override def getType: BType = BoolBType
  override def replaceReserved(reserved: Set[String]): L = this
  override def functionOps: Set[FunctionOp] = index.functionOps + this
  override def locals: Set[BVar] = index.locals
  override def globals: Set[BVar] = index.globals
}
