package translating

import ir.*
import ir.cilvisitor.*
import util.{OnCrash, RingTrace}

import java.io.{BufferedWriter, File, FileWriter, Writer}

trait BasilIR[Repr[+_]] extends BasilIRExp[Repr] {
  // def vstmt(s: Statement) : Repr[Statement]

  def vstmt(s: Statement): Repr[Statement] = {
    s match {
      case a: LocalAssign => vassign(vlvar(a.lhs), vexpr(a.rhs))
      case m: MemoryAssign => vmemassign(vlvar(m.lhs), vexpr(m.rhs))
      case a: SimulAssign =>
        vsimulassign(a.assignments.toList.map { case (lhs, rhs) =>
          vlvar(lhs) -> vexpr(rhs)
        })

      case m: MemoryLoad => vload(vlvar(m.lhs), m.mem.name, vexpr(m.index), m.endian, m.size)
      case m: MemoryStore => vstore(m.mem, vexpr(m.index), vexpr(m.value), m.endian, m.size)
      case c: DirectCall =>
        vcall(
          c.outParams.toList.map((l, r) => (l, r)),
          c.target.name,
          c.actualParams.toList.map((l, r) => (l, vexpr(r)))
        )
      case i: IndirectCall => vindirect(vrvar(i.target))
      case a: Assert => vassert(a)
      case a: Assume => vassume(a)
      case n: NOP => vnop()
    }
  }

  def vjump(j: Jump): Repr[Jump] = {
    j match {
      case g: GoTo => vgoto(g.targets.toList.map(_.label))
      case g: Unreachable => vunreachable()
      case r: Return => vreturn(r.outParams.toList.map((l, r) => (vlvar(l), vexpr(r))))
    }
  }

  def vexpr(e: Expr): Repr[Expr] = {
    e match {
      case n: Literal => vliteral(n)
      case Extract(ed, start, arg) => vextract(ed, start, vexpr(arg))
      case Repeat(repeats, arg) => vrepeat(repeats, vexpr(arg))
      case ZeroExtend(bits, arg) => vzeroextend(bits, vexpr(arg))
      case SignExtend(bits, arg) => vsignextend(bits, vexpr(arg))
      case BinaryExpr(op, arg, arg2) => vbinary_expr(op, vexpr(arg), vexpr(arg2))
      case b @ AssocExpr(op, args) => vassoc_expr(op, args.map(vexpr))
      case UnaryExpr(op, arg) => vunary_expr(op, vexpr(arg))
      case v: Variable => vrvar(v)
      case f @ FApplyExpr(n, params, rt, _) => vfapply_expr(n, params.map(vexpr))
      case q: QuantifierExpr => vquantifier(q)
      case q: LambdaExpr => vlambda(q)
      case r: OldExpr => vold(r.body)
      case r: SharedMemory => vmemory(r)
      case r: StackMemory => vmemory(r)
    }
  }

  def vblock(b: Block): Repr[Block] = vblock(b.label, b.address, b.statements.toList.map(vstmt), vjump(b.jump))
  def vproc(p: Procedure): Repr[Procedure] = vproc(
    p.name,
    p.formalInParam.toList.map(vlvar),
    p.formalOutParam.toList.map(vlvar),
    p.entryBlock.map(vblock),
    (p.blocks.toSet -- p.entryBlock.toSet -- p.returnBlock.toSet).toList.sortBy(x => -x.rpoOrder).map(vblock),
    p.returnBlock.map(vblock)
  )

  def vblock(
    label: String,
    address: Option[BigInt],
    statements: List[Repr[Statement]],
    terminator: Repr[Jump]
  ): Repr[Block]

  def vprog(p: Program): Repr[Program] = vprog(p.mainProcedure.name, p.procedures.toList.map(vproc))
  def vprog(mainProc: String, procedures: List[Repr[Procedure]]): Repr[Program]

  def vproc(
    name: String,
    inParams: List[Repr[Variable]],
    outParams: List[Repr[Variable]],
    entryBlock: Option[Repr[Block]],
    middleBlocks: List[Repr[Block]],
    returnBlock: Option[Repr[Block]]
  ): Repr[Procedure]

  def vmemory(m: Memory): Repr[Memory]
  def vassign(lhs: Repr[Variable], rhs: Repr[Expr]): Repr[LocalAssign]
  def vmemassign(lhs: Repr[Variable], rhs: Repr[Expr]): Repr[LocalAssign]
  def vsimulassign(assignments: List[(Repr[Variable], Repr[Expr])]): Repr[SimulAssign]
  def vload(lhs: Repr[Variable], mem: String, index: Repr[Expr], endian: Endian, size: Int): Repr[MemoryLoad]
  def vstore(mem: Memory, index: Repr[Expr], value: Repr[Expr], endian: Endian, size: Int): Repr[MemoryStore]
  def vcall(
    outParams: List[(Variable, Variable)],
    procname: String,
    inparams: List[(Variable, Repr[Expr])]
  ): Repr[DirectCall]
  def vindirect(target: Repr[Variable]): Repr[IndirectCall]
  def vassert(body: Assert): Repr[Assert]
  def vassume(body: Assume): Repr[Assume]
  def vnop(): Repr[NOP]

  def vgoto(t: List[String]): Repr[GoTo]
  def vunreachable(): Repr[Unreachable]
  def vreturn(outs: List[(Repr[Variable], Repr[Expr])]): Repr[Return]

  def vlvar(e: Variable): Repr[Variable]

}

trait BasilIRExp[Repr[+_]] {
  def vexpr(e: Expr): Repr[Expr]
  def vassoc_expr(o: BoolBinOp, es: List[Repr[Expr]]): Repr[Expr]
  def vextract(ed: Int, start: Int, a: Repr[Expr]): Repr[Expr]
  def vquantifier(q: QuantifierExpr): Repr[Expr]
  def vlambda(q: LambdaExpr): Repr[Expr]
  def vold(body: Expr): Repr[Expr]
  def vzeroextend(bits: Int, b: Repr[Expr]): Repr[Expr]
  def vsignextend(bits: Int, b: Repr[Expr]): Repr[Expr]
  def vbinary_expr(e: BinOp, l: Repr[Expr], r: Repr[Expr]): Repr[Expr]
  def vunary_expr(e: UnOp, arg: Repr[Expr]): Repr[Expr]
  def vliteral(l: Literal): Repr[Literal] = {
    l match {
      case TrueLiteral => vboollit(true)
      case FalseLiteral => vboollit(false)
      case v: IntLiteral => vintlit(v.value)
      case b: BitVecLiteral => vbvlit(b)
    }
  }

  def vboollit(b: Boolean): Repr[BoolLit]
  def vbvlit(b: BitVecLiteral): Repr[BitVecLiteral]
  def vintlit(b: BigInt): Repr[IntLiteral]
  def vrepeat(reps: Int, value: Repr[Expr]): Repr[Expr]

  def vfapply_expr(name: String, args: Seq[Repr[Expr]]): Repr[Expr]

  def vrvar(e: Variable): Repr[Variable]
}

trait BasilIRExpWithVis[Repr[+_]] extends BasilIRExp[Repr] {

  /** Performs some simple reductions to fit basil IR into SMT2.
    */

  def vexpr(e: Expr): Repr[Expr] = {
    e match {
      case n: Literal => vliteral(n)
      case Extract(ed, start, arg) => vextract(ed, start, vexpr(arg))
      case Repeat(repeats, arg) => {
        vexpr((0 until (repeats - 1)).foldLeft(arg)((acc, n) => BinaryExpr(BVCONCAT, acc, arg)))
      }
      case ZeroExtend(bits, arg) => vexpr(BinaryExpr(BVCONCAT, BitVecLiteral(0, bits), arg))
      case SignExtend(bits, arg) =>
        vexpr(BinaryExpr(BVCONCAT, Repeat(bits, Extract(size(arg).get, size(arg).get - 1, arg)), arg))
      case b @ BinaryExpr(op, arg, arg2) =>
        op match {
          case NEQ => vunary_expr(BoolNOT, vbinary_expr(EQ, vexpr(arg), vexpr(arg2)))
          case _ => vbinary_expr(op, vexpr(arg), vexpr(arg2))
        }
      case UnaryExpr(op, arg) => vunary_expr(op, vexpr(arg))
      case v: Variable => vrvar(v)
      case b @ AssocExpr(op, args) => vassoc_expr(op, args.map(vexpr))
      case r: SharedMemory => ???
      case r: StackMemory => ???
      case f @ FApplyExpr(n, params, rt, _) => vfapply_expr(n, params.map(vexpr))
      case q: QuantifierExpr => vquantifier(q)
      case q: LambdaExpr => vlambda(q)
      case r: OldExpr => vold(r.body)
    }
  }

}

enum Sexp[+T] {
  case Symb(v: String)
  case Slist(v: List[Sexp[T]])
}

object Sexp {

  def print[T](s: Sexp[T]): String = s match {
    case Sexp.Symb(a) => a
    case Sexp.Slist(v) => "(" + v.map(print).mkString(" ") + ")"
  }
}

def sym[T](l: String): Sexp[T] = Sexp.Symb[T](l)
def list[T](l: Sexp[T]*): Sexp[T] = Sexp.Slist(l.toList)

val dumpTrace = RingTrace[String](3, "BasilIRToSMT2")
object BasilIRToSMT2 extends BasilIRExpWithVis[Sexp] {

  OnCrash.register(dumpTrace)

  def vload(lhs: Sexp[Variable], mem: String, index: Sexp[Expr], endian: Endian, size: Int): Sexp[MemoryLoad] = ???
  def vstore(mem: Memory, index: Sexp[Expr], value: Sexp[Expr], endian: Endian, size: Int): Sexp[MemoryStore] = ???

  def vold(e: Expr) = ???
  def vquantifier(e: QuantifierExpr) = ???
  def vlambda(e: LambdaExpr) = ???
  def vprog(mainProc: String, procedures: List[Sexp[Procedure]]): Sexp[Program] = ???
  def vrepeat(reps: Int, value: Sexp[Expr]): Sexp[Expr] = ???
  def vzeroextend(bits: Int, b: Sexp[Expr]): Sexp[Expr] = ???
  def vsignextend(bits: Int, b: Sexp[Expr]): Sexp[Expr] = ???
  def vboollit(b: Boolean): Sexp[BoolLit] = ???
  def vbvlit(b: BitVecLiteral): Sexp[BitVecLiteral] = ???
  def vintlit(b: BigInt): Sexp[IntLiteral] = ???

  class SMTBuilder() {
    var before = true
    var exprs = Vector[Sexp[Expr]]()
    var exprsBefore = Vector[Sexp[Expr]]()
    var decls = Set[Sexp[Expr]]()
    var typedecls = Set[Sexp[Expr]]()

    def addAssume(e: Expr) = {
      before = false
      val (t, d) = BasilIRToSMT2.extractDecls(e)
      decls = decls ++ d
      typedecls = typedecls ++ t
      exprs = exprs ++ List(list(sym("assume"), BasilIRToSMT2.vexpr(e)))
    }

    def addCommand(rawSexp: String*) = {
      if (before) {
        exprsBefore = exprsBefore.appended(list(rawSexp.map(sym[Expr](_)): _*))
      } else {
        exprs = exprs.appended(list(rawSexp.map(sym[Expr](_)): _*))
      }
    }

    def addAssert(e: Expr, name: Option[String] = None) = {
      before = false
      val (t, d) = BasilIRToSMT2.extractDecls(e)
      decls = decls ++ d
      typedecls = typedecls ++ t
      val expr: Sexp[Expr] = BasilIRToSMT2.vexpr(e)
      val inner: Sexp[Expr] = name.map(n => list(sym("!"), expr, sym(":named"), sym(n))).getOrElse(expr)

      exprs = exprs ++ List(list(sym("assert"), inner))
    }

    def writeCheckSat(b: Writer, getUnsatCore: Boolean = false) = {
      val query = getCheckSat(getUnsatCore)
      for (q <- query) {
        b.append(Sexp.print(q))
        b.append("\n")
      }
    }

    def writeCheckSatToFile(fname: File, getUnsatCore: Boolean = false) = {
      val f = BufferedWriter(FileWriter(fname))
      try {
        writeCheckSat(f, getUnsatCore)
      } finally {
        if (f != null) {
          f.close()
        }
      }
    }

    def getCheckSat(getUnsatCore: Boolean = false) = {
      val setUnsat =
        if getUnsatCore then Seq(list(sym("set-option"), sym(":produce-unsat-cores"), sym("true"))) else Seq()
      val getUnsat = if getUnsatCore then Seq(list(sym("get-unsat-core"))) else Seq()

      setUnsat.iterator ++ exprsBefore.iterator ++ typedecls ++ decls ++ exprs ++ Seq(
        list(sym("check-sat"))
      ) ++ getUnsat
    }
  }

  /** Immediately invoke z3 and block until it returns a result.
    *
    * Return Some(true) when proven, Some(false) when counterexample found, and None when unknown.
    */
  def proveExpr(e: Expr, softTimeoutMillis: Option[Int] = None): Option[Boolean] = {
    val query = exprUnsat(e, None, false)
    val res = util.z3.checkSATSMT2(query, softTimeoutMillis)
    res match {
      case util.z3.SatResult.UNSAT => Some(true)
      case util.z3.SatResult.SAT(_) => Some(false)
      case util.z3.SatResult.Unknown(_, _) => None
    }
  }

  def exprUnsat(e: Expr, name: Option[String] = None, getModel: Boolean = true, genQuery: Boolean = true): String = {
    val assert = if (name.isDefined) {
      list(sym("assert"), list(sym("!"), BasilIRToSMT2.vexpr(e), sym(":named"), sym(name.get)))
    } else {
      list(sym("assert"), BasilIRToSMT2.vexpr(e))
    }

    val (typedecls, decls) = BasilIRToSMT2.extractDecls(e)
    val terms =
      if genQuery then
        list(sym("push")) :: (typedecls.toVector ++ decls).toList
          ++ List(assert, list(sym("check-sat")))
          ++ (if (getModel) then
                List(list(sym("echo"), sym("\"" + name.getOrElse("") + "  ::  " + e + "\"")), list(sym("get-model")))
              else List())
          ++ List(list(sym("pop")))
      else (typedecls.toVector ++ decls).toList :+ assert

    (terms.map(Sexp.print)).mkString("\n")
  }

  def unaryOpnameToFun(b: UnOp) = {
    b match {
      case BoolNOT => "not"
      case BVNOT => "bvnot"
      case BVNEG => "bvneg"
      case IntNEG => "-"
      case BoolToBV1 => "bool2bv1"
    }
  }

  def opnameToFun(b: BinOp) = {
    b match {
      case EQ => "="
      case BoolAND => "and"
      case BoolIMPLIES => "=>"
      case NEQ => ???
      case BoolOR => "or"
      case BVCONCAT => "concat"
      case b: BVBinOp => "bv" + b.opName
      case b: IntBinOp => b.opName
    }
  }

  def fixVname(n: String): String = {
    n.map(c =>
      c match {
        case '#' => 'x'
        case c => c
      }
    ).mkString("")
  }

  def int2smt(i: Int) = sym(i.toString)
  def bv2smt(i: BitVecLiteral) = list(sym("_"), sym(s"bv${i.value}"), sym(i.size.toString))

  override def vextract(ed: Int, start: Int, a: Sexp[Expr]): Sexp[Expr] =
    list(list(sym("_"), sym("extract"), int2smt(ed - 1), int2smt(start)), a)
  override def vbinary_expr(e: BinOp, l: Sexp[Expr], r: Sexp[Expr]): Sexp[Expr] = {
    dumpTrace.add(e.toString + "(" + l + "," + r + ")")
    list(sym(opnameToFun(e)), l, r)
  }
  override def vassoc_expr(e: BoolBinOp, l: List[Sexp[Expr]]): Sexp[Expr] =
    dumpTrace.add(e.toString + "(" + l.mkString(",") + ")")
    Sexp.Slist(sym(opnameToFun(e)) :: l)
  override def vunary_expr(e: UnOp, arg: Sexp[Expr]): Sexp[Expr] = list(sym(unaryOpnameToFun(e)), arg)

  override def vliteral(arg: Literal): Sexp[Literal] = arg match {
    case bv @ BitVecLiteral(value, size) => bv2smt(bv)
    case IntLiteral(i) => sym(i.toString)
    case TrueLiteral => sym("true")
    case FalseLiteral => sym("false")
  }

  def endianToBool(endian: Endian): Sexp[Expr] = {
    if endian == Endian.LittleEndian then vexpr(FalseLiteral) else vexpr(TrueLiteral)
  }
  override def vfapply_expr(name: String, args: Seq[Sexp[Expr]]): Sexp[Expr] = {
    if (args.size == 1) {
      list(sym(name), args.head)
    } else {
      list(sym(name), Sexp.Slist(args.toList))
    }
  }

  override def vrvar(e: Variable): Sexp[Variable] = sym(fixVname(e.name))

  def basilTypeToSMTType(v: IRType): Sexp[Expr] = {
    v match {
      case BoolType => sym("Bool")
      case IntType => sym("Int")
      case BitVecType(sz) => list(sym("_"), sym("BitVec"), int2smt(sz))
      case MapType(pt, rt) => list(sym("Array"), basilTypeToSMTType(pt), basilTypeToSMTType(rt))
      case CustomSort(n) => sym(n)
    }
  }

  def booltoBVDef: Sexp[Expr] = {
    list(
      sym("define-fun"),
      sym("bool2bv1"),
      list(list(sym("arg"), basilTypeToSMTType(BoolType))),
      basilTypeToSMTType(BitVecType(1)),
      list(sym("ite"), sym("arg"), bv2smt(BitVecLiteral(1, 1)), bv2smt(BitVecLiteral(0, 1)))
    )
  }

  def interpretFun(x: FApplyExpr): Option[Sexp[Expr]] = {
    x.name match {
      case "bool2bv1" => {
        Some(booltoBVDef)
      }
      case "bvsaddo" => None
      case _ => {
        Some(
          list(
            sym("declare-fun"),
            sym(x.name),
            Sexp.Slist(x.params.toList.map(a => basilTypeToSMTType(a.getType))),
            basilTypeToSMTType(x.returnType)
          )
        )
      }
    }
  }

  def extractDecls(e: Expr): (Set[Sexp[Expr]], Set[Sexp[Expr]]) = {

    class ToDecl extends CILVisitor {
      var decled = Set[Sexp[Expr]]()
      var typeDecled = Set[Sexp[Expr]]()

      override def vexpr(e: Expr) =
        e.getType match {
          case CustomSort(b) => {
            typeDecled = typeDecled + list(sym("declare-sort"), sym(b))
          }
          case _ => ()
        }
        e match {
          case f: FApplyExpr => {
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
          case _ => DoChildren()
        }

      def getDecls(e: Expr) = {
        decled = Set()
        visit_expr(this, e)
        (typeDecled, decled)
      }
    }

    ToDecl().getDecls(e)
  }

}
