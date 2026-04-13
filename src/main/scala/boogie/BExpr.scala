package boogie
import ir.*
import ir.dsl.given
import specification.*
import util.assertion.*

import java.io.Writer

import collection.mutable

sealed trait BExpr {
  def getType: BType
  def functionOps: Set[FunctionOp] = Set()
  def locals: Set[BVar] = Set()
  def globals: Set[BVar] = Set()
  def params: Set[BVar] = Set()
  def specGlobals: Set[SpecGlobalOrAccess] = Set()
  def oldSpecGlobals: Set[SpecGlobalOrAccess] = Set()
  def specGammas: Set[SpecGlobalOrAccess] = Set()
  def oldSpecGammas: Set[SpecGlobalOrAccess] = Set()
  def loads: Set[BExpr] = Set()
  def serialiseBoogie(w: Writer): Unit = w.append(toString)
  def acceptVisit(visitor: BVisitor): BExpr = this

  var label: Option[String] = None

  def simplify: BExpr =
    this match {
      case BinaryBExpr(BoolAND, a, b) =>
        (a.simplify, b.simplify) match {
          case (TrueBLiteral, b) => b
          case (a, TrueBLiteral) => a
          case (FalseBLiteral, _) => FalseBLiteral
          case (_, FalseBLiteral) => FalseBLiteral
          case (a, b) => BinaryBExpr(BoolAND, a, b)
        }
      case BinaryBExpr(BoolOR, a, b) =>
        (a.simplify, b.simplify) match {
          case (TrueBLiteral, _) => TrueBLiteral
          case (_, TrueBLiteral) => TrueBLiteral
          case (FalseBLiteral, b) => b
          case (a, FalseBLiteral) => a
          case (a, b) => BinaryBExpr(BoolOR, a, b)
        }
      case BinaryBExpr(BoolIMPLIES, a, b) =>
        (a.simplify, b.simplify) match {
          case (TrueBLiteral, b) => b
          case (FalseBLiteral, _) => TrueBLiteral
          case (_, TrueBLiteral) => TrueBLiteral
          case (a, b) => BinaryBExpr(BoolIMPLIES, a, b)
        }
      case _ => this
    }
}

trait BLiteral extends BExpr

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
}

case class BVExtract(end: Int, start: Int, body: BExpr) extends BExpr {
  override val getType: BitVecBType = BitVecBType(end - start)
  override def toString: String = s"$body[$end:$start]"
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def params: Set[BVar] = body.params
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.oldSpecGammas
  override def loads: Set[BExpr] = body.loads

  override def serialiseBoogie(w: Writer): Unit = {
    body.serialiseBoogie(w)
    w.append(s"[$end:$start]")
  }

  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBVExtract(this)

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
  override def params: Set[BVar] = body.params
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.oldSpecGammas
  override def loads: Set[BExpr] = body.loads
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBVRepeat(this)
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
  override def params: Set[BVar] = body.params
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.oldSpecGammas
  override def loads: Set[BExpr] = body.loads

  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBVZeroExtend(this)
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
  override def params: Set[BVar] = body.params
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.oldSpecGammas
  override def loads: Set[BExpr] = body.loads
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBVSignExtend(this)
}

abstract class BVar(val name: String, val bType: BType, val scope: Scope) extends BExpr with Ordered[BVar] {
  def compare(that: BVar): Int = this.name.compare(that.name)

  override def getType: BType = bType
  override def toString: String = scope match {
    case Scope.Local => Sigil.Boogie.localVar + name
    case Scope.Parameter => Sigil.Boogie.localVar + name
    case Scope.Const => Sigil.Boogie.globalVar + name
    case Scope.Global => Sigil.Boogie.globalVar + name
  }
  def withType: String = if (name.isEmpty) {
    s"$bType"
  } else {
    s"$this: $bType"
  }
  override def locals: Set[BVar] = scope match {
    case Scope.Local => Set(this)
    case _ => Set()
  }
  override def globals: Set[BVar] = scope match {
    case Scope.Global => Set(this)
    case _ => Set()
  }
  override def params: Set[BVar] = scope match {
    case Scope.Parameter => Set(this)
    case _ => Set()
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

case class BFunctionCall(name: String, args: List[BExpr], outType: BType, uninterpreted: Boolean = false)
    extends BExpr {
  override val getType: BType = outType
  override def toString: String = s"$name(${args.mkString(", ")})"
  override def functionOps: Set[FunctionOp] = {
    val ops = args.flatMap(a => a.functionOps).toSet
    if (uninterpreted) {
      ops ++ Set(BUninterpreted(name, args.map(_.getType), outType))
    } else {
      ops
    }
  }
  override def locals: Set[BVar] = args.flatMap(a => a.locals).toSet
  override def globals: Set[BVar] = args.flatMap(a => a.globals).toSet
  override def params: Set[BVar] = args.flatMap(a => a.params).toSet
  override def specGlobals: Set[SpecGlobalOrAccess] = args.flatMap(a => a.specGlobals).toSet
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = args.flatMap(a => a.oldSpecGlobals).toSet
  override def specGammas: Set[SpecGlobalOrAccess] = args.flatMap(a => a.specGammas).toSet
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = args.flatMap(a => a.oldSpecGammas).toSet
  override def loads: Set[BExpr] = args.flatMap(a => a.loads).toSet
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBFunctionCall(this)
}

case class UnaryBExpr(op: UnOp, arg: BExpr) extends BExpr {
  override def getType: BType = (op, arg.getType) match {
    case (BoolToBV1, BoolBType) => BitVecBType(1)
    case (_: BoolUnOp, BoolBType) => BoolBType
    case (_: BVUnOp, bv: BitVecBType) => bv
    case (_: IntUnOp, IntBType) => IntBType
    case _ => throw new Exception("type mismatch, operator " + op + " type doesn't match arg: " + arg)
  }

  private def inSize = arg.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception(s"Expected Bv but got ${arg.getType}")
  }

  override def toString: String = op match {
    case BoolToBV1 => s"$op($arg)"
    case uOp: BoolUnOp => s"($uOp$arg)"
    case uOp: BVUnOp => s"bv$uOp$inSize($arg)"
    case uOp: IntUnOp => s"($uOp$arg)"
  }

  override def functionOps: Set[FunctionOp] = {
    val thisFn = op match {
      case b @ BoolToBV1 => Set(BoolToBV1Op(arg))
      case b: BVUnOp =>
        Set(BVFunctionOp(s"bv$b$inSize", s"bv$b", List(BParam(arg.getType)), BParam(getType)))
      case _ => Set()
    }
    arg.functionOps ++ thisFn
  }

  override def locals: Set[BVar] = arg.locals
  override def globals: Set[BVar] = arg.globals
  override def params: Set[BVar] = arg.params
  override def specGlobals: Set[SpecGlobalOrAccess] = arg.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = arg.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = arg.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = arg.oldSpecGammas
  override def loads: Set[BExpr] = arg.loads

  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitUnaryBExpr(this)
}

case class AssocBExpr(op: BoolBinOp | EQ.type | NEQ.type | IntADD.type, arg: List[BExpr]) extends BExpr {
  require(arg.size >= 2, "AssocBExpr requires at least two operands")
  override def getType = BinaryBExpr(op, arg.head, arg.tail.head).getType
  override def serialiseBoogie(w: Writer): Unit = {
    w.append("(")
    arg
      .dropRight(1)
      .foreach(a => {
        a.serialiseBoogie(w)
        w.append(",")
      })
    arg.last.serialiseBoogie(w)
    w.append(")")
  }

  override def functionOps: Set[FunctionOp] = arg.flatMap(_.functionOps).toSet
  override def toString = s"(${arg.mkString(" " + op.toString + " ")})"
  override def locals: Set[BVar] = arg.flatMap(_.locals).toSet
  override def globals: Set[BVar] = arg.flatMap(_.globals).toSet
  override def params: Set[BVar] = arg.flatMap(_.params).toSet
  override def specGlobals: Set[SpecGlobalOrAccess] = arg.flatMap(_.specGlobals).toSet
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = arg.flatMap(_.oldSpecGlobals).toSet
  override def specGammas: Set[SpecGlobalOrAccess] = arg.flatMap(_.specGammas).toSet
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = arg.flatMap(_.oldSpecGammas).toSet
  override def loads: Set[BExpr] = arg.flatMap(_.loads).toSet
}

case class BinaryBExpr(op: BinOp, arg1: BExpr, arg2: BExpr) extends BExpr {
  override def getType: BType = (op, arg1.getType, arg2.getType) match {
    case (_: BoolBinOp, BoolBType, BoolBType) => BoolBType
    case (EQ | NEQ, _, _) => BoolBType
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
      }
    case (intOp: IntBinOp, IntBType, IntBType) =>
      intOp match {
        case IntADD | IntSUB | IntMUL | IntDIV | IntMOD => IntBType
        case IntLT | IntLE | IntGT | IntGE => BoolBType
      }
    case _ =>
      throw new Exception("type mismatch, operator " + op + " type doesn't match args: (" + arg1 + ", " + arg2 + ")")
  }

  private def inSize = arg1.getType match {
    case bv: BitVecBType => bv.size
    case _ => throw new Exception("type mismatch")
  }

  override def serialiseBoogie(w: Writer): Unit = {
    val traversalQueue = mutable.Stack[BExpr | BinOp | String]()
    traversalQueue.append(this)

    while (traversalQueue.nonEmpty) {
      val next = traversalQueue.pop()

      def infix(b: BinaryBExpr): Unit = traversalQueue.pushAll(Seq("(", b.arg1, s" ${b.op} ", b.arg2, s"): ${b.getType}").reverse)
      def prefix(b: BinaryBExpr): Unit =
        traversalQueue.pushAll(Seq(s"bv${b.op}${b.inSize}(", b.arg1, ",", b.arg2, ")").reverse)

      next match
        case b: BinaryBExpr =>
          b.op match {
            case EQ => infix(b)
            case NEQ => infix(b)
            case bOp: BoolBinOp => infix(b)
            case bOp: BVBinOp =>
              bOp match {
                case BVCONCAT => infix(b)
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
    case EQ | NEQ => s"($arg1 $op $arg2)"
    case bOp: BVBinOp =>
      bOp match {
        case BVCONCAT =>
          s"(($arg1 $bOp $arg2): $getType)"
        case _ =>
          s"bv$bOp$inSize($arg1, $arg2)"
      }
    case bOp: IntBinOp => s"($arg1 $bOp $arg2)"
  }

  override def functionOps: Set[FunctionOp] = {
    val thisFn = op match {
      case EQ | NEQ => Set()
      case b: BVBinOp =>
        b match {
          case BVCONCAT => Set()
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
  override def params: Set[BVar] = arg1.params ++ arg2.params
  override def specGlobals: Set[SpecGlobalOrAccess] = arg1.specGlobals ++ arg2.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = arg1.oldSpecGlobals ++ arg2.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = arg1.specGammas ++ arg2.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = arg1.oldSpecGammas ++ arg2.oldSpecGammas
  override def loads: Set[BExpr] = arg1.loads ++ arg2.loads

  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitBinaryBExpr(this)
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
  override def params: Set[BVar] = guard.params ++ thenExpr.params ++ elseExpr.params
  override def specGlobals: Set[SpecGlobalOrAccess] = guard.specGlobals ++ thenExpr.specGlobals ++ elseExpr.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] =
    guard.oldSpecGlobals ++ thenExpr.oldSpecGlobals ++ elseExpr.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = guard.specGammas ++ thenExpr.specGammas ++ elseExpr.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] =
    guard.oldSpecGammas ++ thenExpr.oldSpecGammas ++ elseExpr.oldSpecGammas
  override def loads: Set[BExpr] = guard.loads ++ thenExpr.loads ++ elseExpr.loads

  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitIfThenElse(this)
}

trait BQuantifierExpr(sort: Quantifier, bound: List[BVar], body: BExpr, triggers: List[List[BExpr]] = List())
    extends BExpr {
  override def toString: String = {
    val trstr = if triggers.nonEmpty then {
      triggers.filter(t => t.nonEmpty).map(t => "{" + t.mkString(",") + "}").mkString(" ")
    } else ""
    val boundString = bound.map(_.withType).mkString(", ")
    s"($sort $boundString::  $trstr($body))"
  }
  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals -- bound.toSet
  override def globals: Set[BVar] = body.globals -- bound.toSet
  override def params: Set[BVar] = body.params -- bound.toSet
  override def specGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.oldSpecGlobals
  override def specGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.oldSpecGammas
  override def loads: Set[BExpr] = body.loads
}

enum Quantifier {
  case forall
  case exists
  case lambda
}

case class ForAll(bound: List[BVar], body: BExpr, triggers: List[List[BExpr]] = List())
    extends BQuantifierExpr(Quantifier.forall, bound, body, triggers)

case class Exists(bound: List[BVar], body: BExpr, triggers: List[List[BExpr]] = List())
    extends BQuantifierExpr(Quantifier.exists, bound, body, triggers)

case class Lambda(bound: List[BVar], body: BExpr) extends BQuantifierExpr(Quantifier.lambda, bound, body)

case class Old(body: BExpr) extends BExpr {
  override def toString: String = s"old($body)"
  override def getType: BType = body.getType
  override def functionOps: Set[FunctionOp] = body.functionOps
  override def locals: Set[BVar] = body.locals
  override def globals: Set[BVar] = body.globals
  override def params: Set[BVar] = body.params
  override def oldSpecGlobals: Set[SpecGlobalOrAccess] = body.specGlobals
  override def oldSpecGammas: Set[SpecGlobalOrAccess] = body.specGammas
  override def loads: Set[BExpr] = body.loads
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitOld(this)
}

case class MapAccess(mapVar: BMapVar, index: BExpr) extends BExpr {
  override def toString: String = s"$mapVar[$index]"
  override val getType: BType = mapVar.getType.result
  override def functionOps: Set[FunctionOp] = index.functionOps
  override def locals: Set[BVar] = index.locals
  override def globals: Set[BVar] = index.globals ++ mapVar.globals
  override def params: Set[BVar] = index.params ++ mapVar.params
  override def loads: Set[BExpr] = index.loads
}

case class MapUpdate(map: BExpr, index: BExpr, value: BExpr) extends BExpr {
  override def toString = s"$map[$index := $value]"
  override val getType: BType = map.getType
  override def functionOps: Set[FunctionOp] = map.functionOps ++ index.functionOps ++ value.functionOps
  override def locals: Set[BVar] = map.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ map.globals ++ value.globals
  override def params: Set[BVar] = index.params ++ map.params ++ value.params
  override def loads: Set[BExpr] = index.loads ++ value.loads ++ map.loads
}

sealed trait FunctionOp

case class BVFunctionOp(name: String, bvbuiltin: String, in: List[BVar], out: BVar) extends FunctionOp {
  def attribute: BAttribute = BAttribute("bvbuiltin", Some(s"\"$bvbuiltin\""))
}

case class MemoryLoadOp(addressSize: Int, valueSize: Int, endian: Endian, bits: Int) extends FunctionOp {
  val accesses: Int = bits / valueSize
  debugAssert(accesses > 0)

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_load${bits}_le"
    case Endian.BigEndian => s"memory_load${bits}_be"
  }
}
case class MemoryStoreOp(addressSize: Int, valueSize: Int, endian: Endian, bits: Int) extends FunctionOp {
  val accesses: Int = bits / valueSize

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_store${bits}_le"
    case Endian.BigEndian => s"memory_store${bits}_be"
  }
}
case class GammaLoadOp(addressSize: Int, bits: Int, accesses: Int) extends FunctionOp {
  val fnName: String = s"gamma_load$bits"
}
case class GammaStoreOp(addressSize: Int, bits: Int, accesses: Int) extends FunctionOp {
  val fnName: String = s"gamma_store$bits"
}
case class LOp(indexType: BType) extends FunctionOp

/** Utility to extract a particular byte from a bitvector.
  */
case class ByteExtract(valueSize: Int, offsetSize: Int) extends FunctionOp {
  val fnName: String = s"byte_extract${valueSize}_${offsetSize}"
}

case class BByteExtract(value: BExpr, offset: BExpr) extends BExpr {
  override def toString: String = s"$fnName($value, $offset)"

  val valueSize: Int = value.getType match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"ByteExtract does not have Bitvector type: $this")
  }

  val offsetSize: Int = offset.getType match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"ByteExtract does not have Bitvector type: $this")
  }

  val fnName: String = s"byte_extract${valueSize}_${offsetSize}"

  override val getType: BType = BitVecBType(8)
  override def functionOps: Set[FunctionOp] =
    value.functionOps ++ offset.functionOps + ByteExtract(valueSize, offsetSize)
  override def locals: Set[BVar] = value.locals ++ offset.locals
  override def globals: Set[BVar] = value.globals ++ offset.globals
  override def params: Set[BVar] = value.params ++ offset.params
  override def loads: Set[BExpr] = value.loads ++ offset.loads
}

/** Utility to test if a particular value i is within the bounds of a base variable and some length. Factors in the
  * problem of wrap around, given the base + length exceeds the bitvector size.
  *
  * Assumes all inputs are of the same bitvector width.
  */
case class InBounds(bits: Int, endian: Endian) extends FunctionOp {
  val fnName: String = endian match {
    case Endian.LittleEndian => s"in_bounds${bits}_le"
    case Endian.BigEndian => s"in_bounds${bits}_be"
  }
}

case class BUninterpreted(name: String, in: List[BType], out: BType) extends FunctionOp

case class BInBounds(base: BExpr, len: BExpr, endian: Endian, i: BExpr) extends BExpr {
  override def toString: String = s"$fnName($base, $len, $i)"

  val baseSize: Int = base.getType match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"InBounds does not have Bitvector type: $this")
  }

  val fnName: String = s"in_bounds${baseSize}_${if endian == Endian.LittleEndian then "le" else "be"}"

  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] =
    base.functionOps ++ len.functionOps ++ i.functionOps + InBounds(baseSize, endian)
  override def locals: Set[BVar] = base.locals ++ len.locals ++ i.locals
  override def globals: Set[BVar] = base.globals ++ len.globals ++ i.globals
  override def params: Set[BVar] = base.params ++ len.params ++ i.params
  override def loads: Set[BExpr] = base.loads ++ len.loads ++ i.loads
}

case class BoolToBV1Op(arg: BExpr) extends FunctionOp {
  def attribute: BAttribute = BAttribute("inline", None)
  val fnName: String = "bool2bv1"
}

case class BMemoryLoad(memory: BMapVar, index: BExpr, endian: Endian, bits: Int) extends BExpr {
  override def toString: String = s"$fnName($memory, $index)"
  debugAssert(bits >= 8)

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_load${bits}_le"
    case Endian.BigEndian => s"memory_load${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"MemoryLoad does not have Bitvector type: $this")
  }

  override val getType: BType = BitVecBType(bits)
  override def functionOps: Set[FunctionOp] =
    memory.functionOps ++ index.functionOps + MemoryLoadOp(addressSize, valueSize, endian, bits)
  override def locals: Set[BVar] = memory.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals
  override def params: Set[BVar] = index.params ++ memory.params
  override def loads: Set[BExpr] = Set(this) ++ index.loads
}

case class BMemoryStore(memory: BMapVar, index: BExpr, value: BExpr, endian: Endian, bits: Int) extends BExpr {
  override def toString: String = s"$fnName($memory, $index, $value)"

  val fnName: String = endian match {
    case Endian.LittleEndian => s"memory_store${bits}_le"
    case Endian.BigEndian => s"memory_store${bits}_be"
  }

  val addressSize: Int = memory.getType.param match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  val valueSize: Int = memory.getType.result match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"MemoryStore does not have Bitvector type: $this")
  }

  override val getType: BType = memory.getType
  override def functionOps: Set[FunctionOp] =
    memory.functionOps ++ index.functionOps ++ value.functionOps + MemoryStoreOp(addressSize, valueSize, endian, bits)
  override def locals: Set[BVar] = memory.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ memory.globals ++ value.globals
  override def params: Set[BVar] = index.params ++ memory.params ++ value.params
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
    case _ => throw new Exception(s"GammaLoad does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] =
    gammaMap.functionOps ++ index.functionOps + GammaLoadOp(addressSize, bits, accesses)
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals
  override def params: Set[BVar] = index.params ++ gammaMap.params
  override def loads: Set[BExpr] = Set(this) ++ index.loads
}

case class GammaStore(gammaMap: BMapVar, index: BExpr, value: BExpr, bits: Int, accesses: Int) extends BExpr {
  require(accesses > 0)
  override def toString: String = s"$fnName($gammaMap, $index, $value)"
  val fnName: String = s"gamma_store$bits"

  val addressSize: Int = gammaMap.getType.param match {
    case b: BitVecBType => b.size
    case _ => throw new Exception(s"GammaStore does not have Bitvector type: $this")
  }

  val valueSize: Int = bits / accesses

  override val getType: BType = gammaMap.getType
  override def functionOps: Set[FunctionOp] =
    gammaMap.functionOps ++ index.functionOps ++ value.functionOps + GammaStoreOp(addressSize, bits, accesses)
  override def locals: Set[BVar] = gammaMap.locals ++ index.locals ++ value.locals
  override def globals: Set[BVar] = index.globals ++ gammaMap.globals ++ value.globals
  override def params: Set[BVar] = index.params ++ gammaMap.params ++ value.params
  override def loads: Set[BExpr] = index.loads ++ value.loads
}

case class L(memories: List[BMapVar], index: BExpr) extends BExpr {
  override def toString: String = if (memories.isEmpty) {
    s"L($index)"
  } else {
    s"L(${memories.mkString(", ")}, $index)"
  }
  override val getType: BType = BoolBType
  override def functionOps: Set[FunctionOp] = index.functionOps + LOp(index.getType)
  override def locals: Set[BVar] = index.locals ++ memories.flatMap(_.locals)
  override def globals: Set[BVar] = index.globals ++ memories.flatMap(_.globals)
  override def params: Set[BVar] = index.params ++ memories.flatMap(_.params)
  override def loads: Set[BExpr] = index.loads
}

/** spec * */

trait SpecVar extends BExpr {
  val address: BigInt
  override def getType: BType = {
    throw new Exception("getType called on SpecVar")
  }
}

trait SpecGlobalOrAccess extends SpecVar {
  val toAddrVar: BExpr
  val toOldVar: BVar
  val toOldGamma: BVar
  val size: Int
}

case class SpecGlobal(
  override val name: String,
  override val size: Int,
  arraySize: Option[Int],
  override val address: BigInt
) extends SymbolTableEntry,
      SpecGlobalOrAccess,
      util.ProductOrdered[SpecGlobal] derives ir.dsl.ToScala {
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)

  def sanitisedName = util.StringEscape.escape(name)

  override val toAddrVar: BVar = BVariable(s"${sanitisedName}_addr", BitVecBType(64), Scope.Const)
  override val toOldVar: BVar = BVariable(s"${sanitisedName}_old", BitVecBType(size), Scope.Local)
  override val toOldGamma: BVar = BVariable(s"Gamma_${sanitisedName}_old", BoolBType, Scope.Local)
  val toAxiom: BAxiom = BAxiom(BinaryBExpr(EQ, toAddrVar, BitVecBLiteral(address, 64)), List.empty)
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitSpecGlobal(this)
}

case class SpecGamma(global: SpecGlobal) extends SpecVar {
  override val address: BigInt = global.address
  val size: Int = global.size
  override def specGammas: Set[SpecGlobalOrAccess] = Set(this.global)
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitSpecGamma(this)
}

case class ArrayAccess(global: SpecGlobal, index: Int) extends SpecGlobalOrAccess {
  val offset: Int = index * (global.size / 8)
  override val address: BigInt = global.address + offset
  override val size: Int = global.size
  override val toOldVar: BVar = BVariable(s"${global.name}$$${index}_old", BitVecBType(global.size), Scope.Local)
  override val toAddrVar: BExpr = BinaryBExpr(BVADD, global.toAddrVar, BitVecBLiteral(offset, 64))
  override val toOldGamma: BVar = BVariable(s"Gamma_${global.name}$$${index}_old", BoolBType, Scope.Local)
  override def specGlobals: Set[SpecGlobalOrAccess] = Set(this)
  override def acceptVisit(visitor: BVisitor): BExpr = visitor.visitArrayAccess(this)
}

case class BFieldAccessExpr(record: BVar, field: BVar) extends BExpr {
  override def toString(): String = s"$record->$field"
  override def getType: BType = field.getType
}
