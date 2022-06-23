package boogie

trait BExpr {
  def getType: BType
  def bvFunctions: Set[BFunction] = Set()
  def locals: Set[BVar] = Set()
}

trait BLiteral(bType: BType) extends BExpr {
  override def getType: BType = bType
}

abstract class BoolLit extends BLiteral(BoolType)

object TrueLiteral extends BoolLit {
  override def toString: String = "true"
}

object FalseLiteral extends BoolLit {
  override def toString: String = "false"
}

case class BitVecLiteral(value: BigInt, size: Int) extends BLiteral(BitVec(size)) {
  override def toString: String = s"${value}bv$size"
}

case class BVExtract(end: Int, start: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = BitVec(end - start)
  override def toString: String = s"$body[$end:$start]"
  override def bvFunctions: Set[BFunction] = body.bvFunctions
  override def locals: Set[BVar] = body.locals
}

case class BVRepeat(repeats: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = BitVec(bodySize * repeats)

  private def bodySize: Int = body.getType match {
    case bv: BitVec => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of extract: " + this)
  }
  private def fnName: String = s"repeat${repeats}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def bvFunctions: Set[BFunction] = {
    val thisFn = BFunction(fnName, s"repeat $repeats", List(BParam(BitVec(bodySize))), BParam(getType), None)
    body.bvFunctions + thisFn
  }
  override def locals: Set[BVar] = body.locals
}

case class BVZeroExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = BitVec(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVec => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }

  private def fnName: String = s"zero_extend${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def bvFunctions: Set[BFunction] = {
    val thisFn = BFunction(fnName, s"zero_extend $extension", List(BParam(BitVec(bodySize))), BParam(getType), None)
    body.bvFunctions + thisFn
  }
  override def locals: Set[BVar] = body.locals
}

case class BVSignExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = BitVec(bodySize + extension)

  private def bodySize: Int = body.getType match {
    case bv: BitVec => bv.size
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero extend: " + this)
  }

  private def fnName: String = s"sign_extend ${extension}_$bodySize"

  override def toString: String = s"$fnName($body)"

  override def bvFunctions: Set[BFunction] = {
    val thisFn = BFunction(fnName, s"sign_extend $extension", List(BParam(BitVec(bodySize))), BParam(getType), None)
    body.bvFunctions + thisFn
  }
  override def locals: Set[BVar] = body.locals
}

trait BVar(val name: String, val bType: BType, val scope: Scope) extends BExpr {
  override def getType: BType = bType
  override def toString: String = name
  def withType: String = if (name.isBlank) {
    s"$bType"
  } else {
    s"$name: $bType"
  }
  override def locals: Set[BVar] = scope match {
    case Scope.Local => Set(this)
    case Scope.Global => Set()
  }
}

case class BParam(override val name: String, override val bType: BType) extends BVar(name, bType, Scope.Local) {
  override def locals: Set[BVar] = Set()
}

object BParam {
  def apply(bType: BType): BParam = BParam("", bType)
}

enum Scope {
  case Local
  case Global
}

case class BVariable(override val name: String, override val bType: BType, override val scope: Scope) extends BVar(name, bType, scope)

case class MapVar(override val name: String, override val bType: MapType) extends BVar(name, bType, Scope.Global) {
  override def getType: MapType = bType
}

case class FunctionCall(name: String, args: List[BExpr], bType: BType) extends BExpr {
  override def getType: BType = bType
  override def toString: String = s"$name(${args.mkString(", ")})"
  override def bvFunctions: Set[BFunction] = args.flatMap(a => a.bvFunctions).toSet // TODO add this?
  override def locals: Set[BVar] = args.flatMap(a => a.locals).toSet
}

case class UnaryBExpr(op: BUnOp, arg: BExpr) extends BExpr {
  override def getType: BType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolType) => BoolType
    case (_: BVUnOp, bv: BitVec) => bv
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVec => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def toString: String = op match {
    case uOp: BoolUnOp => s"$uOp$arg"
    case uOp: BVUnOp => s"bv$uOp$inSize($arg)"
  }

  override def bvFunctions: Set[BFunction] = {
    val thisFn = op match {
      case b: BVBinOp =>
        Set(BFunction(s"bv$b$inSize", s"bv$b", List(BParam(arg.getType)), BParam(getType), None))
      case _ => Set()
    }
    arg.bvFunctions ++ thisFn
  }

  override def locals: Set[BVar] = arg.locals
}

trait BUnOp

sealed trait BoolUnOp(op: String) extends BUnOp {
  override def toString: String = op
}

case object BoolNOT extends BoolUnOp("!")

sealed trait BVUnOp(op: String) extends BUnOp {
  override def toString: String = op
}

case object BVNOT extends BVUnOp("not")
case object BVNEG extends BVUnOp("neg")

case class BinaryBExpr(op: BBinOp, arg1: BExpr, arg2: BExpr) extends BExpr {
  override def getType: BType = (op, arg1.getType, arg2.getType) match {
    case (_: BoolBinOp, BoolType, BoolType) => BoolType
    case (binOp: BVBinOp, bv1: BitVec, bv2: BitVec) => binOp match {
      case BVCONCAT =>
        BitVec(bv1.size + bv2.size)
      case BVAND | BVOR | BVADD | BVMUL | BVUDIV | BVUREM | BVSHL | BVLSHR | BVNAND | BVNOR | BVXOR | BVXNOR | BVSUB | BVSREM | BVSDIV | BVSMOD | BVASHR =>
        if (bv1.size == bv2.size) {
          bv1
        } else {
          throw new Exception("bitvector size mismatch")
        }
      case BVCOMP =>
        if (bv1.size == bv2.size) {
          BitVec(1)
        } else {
          BitVec(1)
          //throw new Exception("bitvector size mismatch") TODO
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
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  private def inSize = arg1.getType match {
    case bv: BitVec => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def toString: String = op match {
    case bOp: BoolBinOp => s"$arg1 $bOp $arg2"
    case bOp: BVBinOp =>
      bOp match {
        case BVEQ | BVNEQ | BVCONCAT =>
          s"$arg1 $bOp $arg2"
        case _ =>
          s"bv$bOp$inSize($arg1, $arg2)"
      }
  }

  override def bvFunctions: Set[BFunction] = {
    val thisFn = op match {
      case b: BVBinOp => b match {
        case BVEQ | BVNEQ | BVCONCAT => Set()
        case _ =>
          Set(BFunction(s"bv$b$inSize", s"bv$b", List(BParam(arg1.getType), BParam(arg2.getType)), BParam(getType), None))
      }
      case _ => Set()
    }
    arg1.bvFunctions ++ arg2.bvFunctions ++ thisFn
  }

  override def locals: Set[BVar] = arg1.locals ++ arg2.locals
}

trait BBinOp

sealed trait BoolBinOp(op: String) extends BBinOp {
  override def toString: String = op
}

case object BoolEQ extends BoolBinOp("==")
case object BoolNEQ extends BoolBinOp("!=")
case object BoolAND extends BoolBinOp("&&")
case object BoolOR extends BoolBinOp("||")
case object BoolIMPLIES extends BoolBinOp("==>")
case object BoolEQUIV extends BoolBinOp("<==>")

sealed trait BVBinOp(op: String) extends BBinOp {
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

case class IfThenElse(guard: BExpr, thenExpr: BExpr, elseExpr: BExpr) extends BExpr {
  override def getType: BType = {
    if (thenExpr.getType == elseExpr.getType) {
      thenExpr.getType
    } else {
      throw new Exception("type mismatch")
    }
  }

  override def toString: String = s"(if $guard then $thenExpr else $elseExpr)"
  override def bvFunctions: Set[BFunction] = guard.bvFunctions ++ thenExpr.bvFunctions ++ elseExpr.bvFunctions
  override def locals: Set[BVar] = guard.locals ++ thenExpr.locals ++ elseExpr.locals
}

trait QuantifierExpr(sort: String, bound: List[BVar], body: BExpr) extends BExpr {
  override def toString: String = {
    val boundString = bound.map(_.withType).mkString(", ")
    s"sort $boundString :: ($body)"
  }
  override def getType: BType = BoolType
  override def bvFunctions: Set[BFunction] = body.bvFunctions
  override def locals: Set[BVar] = body.locals -- bound.toSet
}

case class ForAll(bound: List[BVar], body: BExpr) extends QuantifierExpr("forall", bound, body)

case class Exists(bound: List[BVar], body: BExpr) extends QuantifierExpr("exists", bound, body)

case class Old(body: BExpr) extends BExpr {
  override def toString: String = s"old($body)"
  override def getType: BType = body.getType
  override def bvFunctions: Set[BFunction] = body.bvFunctions
  override def locals: Set[BVar] = body.locals
}

case class MapAccess(mapVar: MapVar, index: BExpr) extends BExpr {
  override def toString: String = s"$mapVar[$index]"
  override def getType: BType = mapVar.getType.result
  override def bvFunctions: Set[BFunction] = index.bvFunctions
  override def locals: Set[BVar] = index.locals
}