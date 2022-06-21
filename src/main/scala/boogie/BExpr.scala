package boogie

trait BExpr {
  def getType: BType
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
  override def getType: BitVec = BitVec(end - start + 1)
  override def toString: String = s"$body[$end:$start]"
}

case class BVRepeat(repeats: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = body.getType match {
    case bv: BitVec => BitVec(bv.size * repeats)
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of extract: " + this)
  }
  override def toString: String = {
    val size = body.getType match {
      case bv: BitVec => bv.size
      case _ => throw new Exception("type mismatch")
    }
    s"repeat${repeats}_$size($body)"
  }
}

case class BVZeroExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = body.getType match {
    case bv: BitVec => BitVec(bv.size + extension)
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of zero_extend: " + this)
  }
  override def toString: String = {
    val size = body.getType match {
      case bv: BitVec => bv.size
      case _ => throw new Exception("type mismatch")
    }
    s"zero_extend${extension}_$size($body)"
  }
}

case class BVSignExtend(extension: Int, body: BExpr) extends BExpr {
  override def getType: BitVec = body.getType match {
    case bv: BitVec => BitVec(bv.size + extension)
    case _ => throw new Exception("type mismatch, non bv expression: " + body + " in body of sign_extend: " + this)
  }
  override def toString: String = {
    val size = body.getType match {
      case bv: BitVec => bv.size
      case _ => throw new Exception("type mismatch")
    }
    s"sign_extend${extension}_$size($body)"
  }
}

trait BVar(name: String, bType: BType) extends BExpr {
  override def getType: BType = bType
  override def toString: String = name
  def withType: String = if (name.isBlank) {
    s"$bType"
  } else {
    s"$name: $bType"
  }
}

case class BVariable(name: String, bType: BType) extends BVar(name, bType)

case class MapVar(name: String, bType: MapType) extends BVar(name, bType) {
  override def getType: MapType = bType
}

case class FunctionCall(name: String, args: List[BExpr], bType: BType) extends BExpr {
  override def getType: BType = bType
}

case class UnaryBExpr(op: BUnOp, arg: BExpr) extends BExpr {
  override def getType: BType = (op, arg.getType) match {
    case (_: BoolUnOp, BoolType) => BoolType
    case (_: BVUnOp, bv: BitVec) => bv
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  override def toString: String = op match {
    case uOp: BoolUnOp => s"$uOp$arg"
    case uOp: BVUnOp =>
      val size = arg.getType match {
        case bv: BitVec => bv.size
        case _ => throw new Exception("type mismatch")
      }
      s"bv$size$uOp($arg)"
  }
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
      case BVAND | BVOR | BVADD | BVMUL | BVUDIV | BVUREM | BVSHL | BVLSHL | BVNAND | BVNOR | BVXOR | BVXNOR | BVSUB | BVSREM | BVSDIV | BVSMOD | BVASHR =>
        if (bv1.size == bv2.size) {
          bv1
        } else {
          throw new Exception("bitvector size mismatch")
        }
      case BVCOMP =>
        if (bv1.size == bv2.size) {
          BitVec(1)
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
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  override def toString: String = op match {
    case bOp: BoolBinOp => s"$arg1 $bOp $arg2"
    case bOp: BVBinOp =>
      bOp match {
        case BVEQ | BVNEQ | BVCONCAT =>
          s"$arg1 $bOp $arg2"
        case _ =>
          val size = arg1.getType match {
            case bv: BitVec => bv.size
            case _ => throw new Exception("type mismatch")
          }
          s"bv$size$bOp($arg1, $arg2)"
      }
  }
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
case object BVLSHL extends BVBinOp("lshl")
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
}

trait QuantifierExpr(sort: String, bound: List[BVar], body: BExpr) extends BExpr {
  override def toString: String = {
    val boundString = bound.map(_.withType).mkString(", ")
    s"sort $boundString :: ($body)"
  }
  override def getType: BType = BoolType
}

case class ForAll(bound: List[BVar], body: BExpr) extends QuantifierExpr("forall", bound, body)

case class Exists(bound: List[BVar], body: BExpr) extends QuantifierExpr("exists", bound, body)

case class Old(body: BExpr) extends BExpr {
  override def toString: String = s"old($body)"
  override def getType: BType = body.getType
}

case class MapAccess(mapVar: MapVar, index: BExpr) extends BExpr {
  override def toString: String = s"$mapVar[$index}"
  override def getType: BType = mapVar.getType.result
}