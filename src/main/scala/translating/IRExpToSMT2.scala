package translating
import ir.*
import boogie.*
import specification.*
import util.{BoogieGeneratorConfig, BoogieMemoryAccessMode, ProcRelyVersion}
import ir.cilvisitor.*

trait BasilIRExp[Repr[_]] {
  def vexpr(e: Expr): Repr[Expr] 
  def vextract(ed: Int, start: Int, a: Repr[Expr]): Repr[Expr]
  def vbinary_expr(e: BinOp, l: Repr[Expr], r: Repr[Expr]): Repr[Expr]
  def vunary_expr(e: UnOp, arg: Repr[Expr]): Repr[Expr]
  def vliteral(arg: Literal): Repr[Expr]
  def vuninterp_function(name: String, args: Seq[Repr[Expr]]): Repr[Expr]

  def vrvar(e: Variable): Repr[Expr]
  def vload(arg: MemoryLoad): Repr[Expr]
}

trait BasilIRExpWithVis[Repr[_]] extends BasilIRExp[Repr] {
  /**
   * Performs some simple reductions to fit basil IR into SMT2.
   */

  def vexpr(e: Expr): Repr[Expr] = {
    e match {
      case n: Literal                               => vliteral(n)
      case m @ MemoryLoad(mem, index, endian, size) => vload(m)
      case Extract(ed, start, arg)                  => vextract(ed, start, vexpr(arg))
      case Repeat(repeats, arg) => {
        vexpr((0 until (repeats - 1)).foldLeft(arg)((acc, n) => BinaryExpr(BVCONCAT, acc, arg)))
      }
      case ZeroExtend(bits, arg) => vexpr(BinaryExpr(BVCONCAT, BitVecLiteral(0, bits), arg))
      case SignExtend(bits, arg) =>
        vexpr(BinaryExpr(BVCONCAT, Repeat(bits, Extract(size(arg).get, size(arg).get - 1, arg)), arg))
      case BinaryExpr(op, arg, arg2) =>
        op match {
          case BVNEQ   => vunary_expr(BoolNOT, vbinary_expr(BVEQ, vexpr(arg), vexpr(arg2)))
          case IntNEQ  => vunary_expr(BoolNOT, vbinary_expr(IntEQ, vexpr(arg), vexpr(arg2)))
          case BoolNEQ => vunary_expr(BoolNOT, vbinary_expr(BoolEQ, vexpr(arg), vexpr(arg2)))
          case _       => vbinary_expr(op, vexpr(arg), vexpr(arg2))
        }
      case UnaryExpr(op, arg)                       => vunary_expr(op, vexpr(arg))
      case v: Variable                              => vrvar(v)
      case f @ UninterpretedFunction(n, params, rt) => vuninterp_function(n, params.map(vexpr))
    }
  }

}

enum Sexp[+T] {
  case Symb(v: String)
  case Slist(v: List[Sexp[T]])

}

object Sexp {

  def print[T](s: Sexp[T]): String = s match {
    case Sexp.Symb(a)  => a
    case Sexp.Slist(v) => "(" + v.map(print).mkString(" ") + ")"
  }
}

def sym[T](l: String): Sexp[T] = Sexp.Symb[T](l)
def list[T](l: Sexp[T]*): Sexp[T] = Sexp.Slist(l.toList)


object BasilIRToSMT2 extends BasilIRExpWithVis[Sexp] {

  def exprUnsat(e: Expr, name : Option[String] = None): String = {

    val assert = if (name.isDefined ) {
        list(sym("assert"), list(sym("!"), BasilIRToSMT2.vexpr(e), sym(":named"), sym(name.get)))
    } else {
        list(sym("assert"), BasilIRToSMT2.vexpr(e))
    }

    val terms = list(sym("push"))::BasilIRToSMT2.extractDecls(e)
      ++ List(
        assert,
        list(sym("echo"), sym("\"" + name.getOrElse("") + "  ::  " + e + "\"")),
        list(sym("check-sat")),
        list(sym("get-model")),
        list(sym("pop"))
      )

    (terms.map(Sexp.print)).mkString("\n")
  }

  def unaryOpnameToFun(b: UnOp) = {
    b match {
      case BoolNOT => "not"
      case BVNOT   => "bvnot"
      case BVNEG   => "bvneg"
      case IntNEG  => "-"
      case BoolToBV1 => "bool2bv1"
    }
  }

  def opnameToFun(b: BinOp) = {
    b match {
      case IntEQ        => "="
      case BoolEQ       => "="
      case BVEQ         => "="
      case BVNEQ        => ???
      case IntNEQ       => ???
      case BoolNEQ      => ???
      case BoolOR       => "or"
      case BVCONCAT     => "concat"
      case b: BVBinOp   => "bv" + b.opName
      case b: BoolBinOp => b.opName
      case b: IntBinOp  => b.opName
    }
  }

  def fixVname(n: String): String = {
    n.map(c =>
      c match {
        case '#' => 'x'
        case c   => c
      }
    ).mkString("")
  }

  def int2smt(i: Int) = sym(i.toString)
  def bv2smt(i: BitVecLiteral) = list(sym("_"), sym(s"bv${i.value}"), sym(i.size.toString))

  override def vextract(ed: Int, start: Int, a: Sexp[Expr]): Sexp[Expr] =
    list(list(sym("_"), sym("extract"), int2smt(ed - 1), int2smt(start)), a)
  override def vbinary_expr(e: BinOp, l: Sexp[Expr], r: Sexp[Expr]): Sexp[Expr] = list(sym(opnameToFun(e)), l, r)
  override def vunary_expr(e: UnOp, arg: Sexp[Expr]): Sexp[Expr] = list(sym(unaryOpnameToFun(e)), arg)
  override def vliteral(arg: Literal): Sexp[Expr] = arg match {
    case bv @ BitVecLiteral(value, size) => bv2smt(bv)
    case IntLiteral(i)                   => sym(i.toString)
    case TrueLiteral                     => sym("true")
    case FalseLiteral                    => sym("false")
  }

  def endianToBool(endian: Endian): Sexp[Expr] = {
    if endian == Endian.LittleEndian then vexpr(FalseLiteral) else vexpr(TrueLiteral)
  }
  override def vuninterp_function(name: String, args: Seq[Sexp[Expr]]): Sexp[Expr] = {
    if (args.size == 1) {
      list(sym(name), args.head)
    } else {
      list(sym(name), Sexp.Slist(args.toList))
    }
  }

  override def vrvar(e: Variable): Sexp[Expr] = sym(fixVname(e.name))
  override def vload(l: MemoryLoad): Sexp[Expr] =
    list(sym("memoryload"), sym(l.mem.name), vexpr(l.index), endianToBool(l.endian), int2smt(l.size))

  def basilTypeToSMTType(v: IRType): Sexp[Expr] = {
    v match {
      case BoolType        => sym("Bool")
      case IntType         => sym("Int")
      case BitVecType(sz)  => list(sym("_"), sym("BitVec"), int2smt(sz))
      case MapType(pt, rt) => list(sym("Array"), basilTypeToSMTType(pt), basilTypeToSMTType(rt))
    }
  }


  def booltoBVDef : Sexp[Expr] = {
    list(
      sym("define-fun"),
      sym("bool2bv1"),
      list(list(sym("arg"), basilTypeToSMTType(BoolType))),
      basilTypeToSMTType(BitVecType(1)),
      list(sym("ite"), sym("arg"), bv2smt(BitVecLiteral(1, 1)), bv2smt(BitVecLiteral(0, 1)))
    )
  }

  def interpretFun(x: UninterpretedFunction): Option[Sexp[Expr]] = {
    x.name match {
      case "bool2bv1" => {
        Some(booltoBVDef)
      }
      case "bvsaddo" => None
      case _ => {
        Some(list(
          sym("declare-fun"),
          sym(x.name),
          Sexp.Slist(x.params.toList.map(a => basilTypeToSMTType(a.getType))),
          basilTypeToSMTType(x.returnType)
        ))
      }
    }
  }

  def extractDecls(e: Expr): List[Sexp[Expr]] = {

    class ToDecl extends CILVisitor {
      var decled = Set[Sexp[Expr]]()

      override def vexpr(e: Expr) = e match {
        case f: UninterpretedFunction => {
          val decl = interpretFun(f)
          decled = decled ++ decl.toSet
          DoChildren() // get variables out of args
        }
        case UnaryExpr(BoolToBV1, _) => {
          decled = decled + booltoBVDef
          DoChildren()
        }
        case v: Variable => {
          val decl = list(sym("declare-const"), sym(fixVname(v.name)), basilTypeToSMTType(v.getType))
          decled = decled + decl
          SkipChildren()
        }
        case l: MemoryLoad => {
          val decl = list(
            sym("declare-fun"),
            sym("memoryload"),
            list(
              basilTypeToSMTType(l.mem.getType),
              basilTypeToSMTType(BitVecType(l.mem.addressSize)),
              basilTypeToSMTType(BoolType),
              basilTypeToSMTType(IntType)
            ),
            basilTypeToSMTType(BitVecType(l.size))
          )
          val mem = list(sym("declare-const"), sym(l.mem.name), basilTypeToSMTType(l.mem.getType))
          decled = decled + mem
          decled = decled + decl
          DoChildren()
        }
        case _ => DoChildren()
      }

      def getDecls(e: Expr): Set[Sexp[Expr]] = {
        decled = Set()
        visit_expr(this, e)
        decled
      }
    }

    ToDecl().getDecls(e).toList
  }

}
