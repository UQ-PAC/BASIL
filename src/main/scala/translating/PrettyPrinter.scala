package translating
import ir.*
import ir.cilvisitor.*
import util.StringEscape.quote
import util.functional.optionToMap

import scala.collection.immutable.ListMap
import scala.collection.mutable

private val localSigils = false

object PrettyPrinter {

  type PrettyPrintable = Program | Procedure | Statement | Jump | Command | Block | Expr | IRContext

  extension (b: BigInt) {
    def pprint: String = "0x%x".format(b)
  }

  extension (p: PrettyPrintable)
    def pprint: String = p match {
      case e: Expr => pp_expr(e)
      case e: Command => pp_cmd(e)
      case e: Block => pp_block(e)
      case e: Procedure => pp_proc(e)
      case e: Program => pp_prog(e)
      case e: IRContext => pp_irctx(e)
    }

  def pp_irctx(e: IRContext) = BasilIRPrettyPrinter().vcontext(e).toString
  def pp_expr(e: Expr) = BasilIRPrettyPrinter()(e)
  def pp_stmt(s: Statement) = BasilIRPrettyPrinter()(s)
  def pp_cmd(c: Command) = c match {
    case j: Jump => pp_jump(j)
    case j: Statement => pp_stmt(j)
  }
  def pp_block(s: Block) = BasilIRPrettyPrinter()(s)
  def pp_jump(s: Jump) = BasilIRPrettyPrinter()(s)
  def pp_prog(s: Program) = BasilIRPrettyPrinter()(s)
  def pp_proc(s: Procedure) = BasilIRPrettyPrinter()(s)
  def pp_proc_sig(s: Procedure) = BasilIRPrettyPrinter().pp_proc_sig(s)

  def pp_prog_with_analysis_results[T](
    before: Map[Block, T],
    after: Map[Block, T],
    p: Program,
    resultPrinter: T => String = ((x: T) => x.toString)
  ) = {
    BasilIRPrettyPrinter(
      with_analysis_results_begin = block => before.get(block).map(resultPrinter),
      block => after.get(block).map(resultPrinter)
    )(p)
  }

  def pp_proc_with_analysis_results[T](
    before: Map[Block, T],
    after: Map[Block, T],
    p: Procedure,
    resultPrinter: T => String = ((x: T) => x.toString)
  ) = {
    BasilIRPrettyPrinter(
      with_analysis_results_begin = block => before.get(block).map(resultPrinter),
      block => after.get(block).map(resultPrinter)
    )(p)
  }

  def pp_block_with_analysis_results[T](
    before: Map[Block, T],
    after: Map[Block, T],
    p: Block,
    resultPrinter: T => String = ((x: T) => x.toString)
  ) = {
    BasilIRPrettyPrinter(
      with_analysis_results_begin = block => before.get(block).map(resultPrinter),
      block => after.get(block).map(resultPrinter)
    )(p)
  }

  def pp_dot_prog(program: Program) = {
    dotBlockGraph(
      program,
      (program.collect {

        case b: Block if b.parent.entryBlock.contains(b) =>
          b -> (pp_proc_sig(b.parent) + "\n" + pp_block(b))
        case b: Block =>
          b -> pp_block(b)
      }).toMap
    )
  }

}

def indent(s: String, indent: String = "  "): String = {
  s.flatMap(c =>
    c match {
      case '\n' => s"\n$indent"
      case c => "" + c
    }
  )
}

trait PPProg[+T]

case class BST[T <: Expr | Command](v: String) extends PPProg[T] {
  override def toString = v
}

case class Prog(mainProc: String, globalDecls: List[String], procedures: List[Proc]) extends PPProg[Program] {
  override def toString =
    globalDecls.map(_ + ";").mkString("\n") + "\n\n" + procedures.map(_.toString + "\n\n").mkString("")
}

case class Proc(header: String, localDecls: List[String], blocks: String) extends PPProg[Procedure] {
  override def toString = {
    if (blocks.size > 0) {
      header + "\n" + blocks + "\n"
    } else {
      header + ";\n"
    }
  }
}

case class PBlock(
  label: String,
  address: Option[String],
  commands: List[String],
  entryComment: Option[String] = None,
  exitComment: Option[String] = None
) extends PPProg[Block] {
  override def toString = {
    val addr = address.map(" " + _).getOrElse("")
    val comment = entryComment.map(c => c + "\n").getOrElse("")
    val excomment = exitComment.map(c => "\n" + c).getOrElse("")
    s"block ${label}${addr} [\n${comment}"
      ++ ("  " + indent(commands.map(_ + ";").mkString("\n"), "  "))
      ++ s"${excomment}\n]"
  }
}

// case class BST[+T](val v: String) {
//   def ++(s: String) = BST(v ++ s)
//   override def toString = v
// }

class BasilIRPrettyPrinter(
  with_analysis_results_begin: Block => Option[String] = _ => None,
  with_analysis_results_end: Block => Option[String] = _ => None
) extends BasilIR[PPProg] {
  val blockIndent = "  "
  val statementIndent = "    "
  val seenVars = mutable.HashSet[Variable]()

  class GetUninterp extends CILVisitor {
    var funs = mutable.LinkedHashSet[(String, List[IRType], IRType)]()
    override def vexpr(e: Expr) = {
      e match {
        case u: FApplyExpr => funs.add(u.signature)
        case _ => ()
      }
      DoChildren()
    }
  }

  def getUninterp(p: Program) = {
    val v = GetUninterp()
    visit_prog(v, p)
    v.funs.toList
  }

  def apply(x: Block): String = {
    vblock(x).toString
  }
  def apply(x: Procedure): String = {
    vproc(x).toString
  }
  def apply(x: Statement): String = {
    vstmt(x).toString
  }
  def apply(x: Jump): String = {
    vjump(x).toString
  }
  def apply(x: Expr): String = {
    vexpr(x).toString
  }
  def apply(x: Program): String = {
    vprog(x).toString
  }

  class Vars(val global: Boolean = true) extends CILVisitor {
    val vars = mutable.Set[Variable]()

    override def vlvar(v: Variable) = {
      v match {
        case v: Global if global => vars.add(v)
        case v: LocalVar if (!global) => vars.add(v)
        case _ => ()
      }

      SkipChildren()
    }

    override def vrvar(v: Variable) = {
      v match {
        case v: Global if global => vars.add(v)
        case v: LocalVar if (!global) => vars.add(v)
        case _ => ()
      }

      SkipChildren()
    }
  }

  def globals(prog: Program): Set[Variable] = {
    val v = Vars(true)
    visit_prog(v, prog)
    v.vars.toSet
  }

  def locals(prog: Procedure): Set[Variable] = {
    val v = Vars(false)
    visit_proc(v, prog)
    v.vars.toSet
  }

  def memoryRegions(prog: Program): Set[Memory] = {
    prog.collect {
      case m: MemoryLoad => m.mem
      case m: MemoryStore => m.mem
    }.toSet
  }

  def vcontext(i: IRContext) = {
    val prog = vprog(i.program)
    import ir.parsing.Attrib
    import ir.parsing.MemoryAttribData

    val r = parsing.SymbolTableInfo.from(i)
    val jsonedCtx = indent(r.toAttrib.pprint, "  ")

    val memory = i.program.initialMemory
      .map { case (k, v) =>
        MemoryAttribData.of(v)
      }
      .toVector
      .sortBy(_.address)
      .map(_.toAttrib)
    val jsonedMem = Attrib.List(memory).pprint

    val decl =
      s"prog entry ${Sigil.BASIR.proc}${i.program.mainProcedure.name} {\n  .symbols = ${jsonedCtx};\n  .initial_memory = ${jsonedMem}\n} ;"

    prog.toString ++ "\n\n" ++ decl ++ "\n"
  }

  override def vprog(p: Program): PPProg[Program] = {

    val threadspec = s"\nprog entry ${Sigil.BASIR.proc}${p.mainProcedure.name}"

    val uninterp = getUninterp(p)
    val ufdecls = uninterp.map { case (n, pt, rt) =>
      s"declare-fun ${Sigil.BASIR.globalVar}$n : (${pt.map(vtype).mkString(", ")}) -> ${vtype(rt)}"
    }

    Prog(
      p.mainProcedure.name,
      memoryRegions(p).toList.sortBy(_.name).map(memdecl) ++
        globals(p).toList.sorted.map(vardecl)
        ++ ufdecls
        ++ List(threadspec)
      // ++ List(initialMemory(p.initialMemory.values))
      ,
      ((p.mainProcedure) :: p.procedures.filterNot(_ == p.mainProcedure).toList)
        .map(vproc)
        .collect {
          case p: Proc => p
          case _ => ???
        }
    )
  }

  def vprog(mainProc: String, procedures: List[PPProg[Procedure]]): PPProg[Program] = {
    // shouldn't be used
    ???
  }

  override def vblock(b: Block): PPProg[Block] = {
    val label = Sigil.BASIR.block + b.label
    val address = b.address
    val statements = b.statements.toList.map(vstmt)
    val terminator = vjump(b.jump)
    val entryComent = with_analysis_results_begin(b).map(c => {
      val broken = c.split('\n').mkString("\n" + blockIndent + "// ")
      s"${blockIndent}// $broken"
    })

    val addr = address.map(a => s"${Sigil.BASIR.attrib}address = ${vaddress(a)}")
    val olabel = b.meta.originalLabel.map(s => Sigil.BASIR.attrib + "originalLabel = \"" + s + "\"")
    val allattrs = addr.toSeq ++ olabel
    val attr = if allattrs.nonEmpty then Some("{" + allattrs.mkString("; ") + "}") else None

    val exitComment = with_analysis_results_end(b).map(c => s"${blockIndent}// ${c}")
    PBlock(label, attr, statements.map(_.toString) ++ Seq(terminator.toString), entryComent, exitComment)
  }

  override def vblock(
    label: String,
    address: Option[BigInt],
    statements: List[PPProg[Statement]],
    terminator: PPProg[Jump]
  ): PPProg[Block] = {
    ???
  }

  def vardecl(v: Variable): String = {
    s"var ${vlvar(v)}"
  }

  def memdecl(m: Memory): String = {
    val shared = m match {
      case s: SharedMemory => "shared "
      case o: StackMemory => ""
    }

    s"memory ${shared}${Sigil.BASIR.globalVar}${m.name} : ${vtype(m.getType)}"

  }

  trait Record
  case class Val(val v: String) extends Record {
    override def toString = v
  }
  case class Lst(val v: List[Record]) extends Record {
    override def toString = {
      if v.size == 0 then {
        "[]"
      } else if v.size == 1 then {
        s"[${v.head}]"
      } else {
        val x = mutable.ArrayBuffer[String]()
        x.addAll(v.map(_.toString))
        var res = ""
        while (x.size > 0) {
          var totake = 1
          var size = x.head.size

          while (size < (78 - totake) && totake < x.size) {
            size += x(totake).size
            totake += 1
          }

          val lot = x.take(totake)

          val bsep = if (res == "") then "" else ";\n"
          res = res + bsep + lot.mkString(";")
          x.remove(0, totake)
        }
        "[\n  " + indent(res) + "\n]"
      }
    }
  }
  case class Rec(val v: ListMap[String, Record]) extends Record {
    override def toString = {
      if v.isEmpty then "{}"
      else if v.size == 1 then {
        s"{" + v.map((k, v) => k + " = " + v).mkString("\n") + "}"
      } else {
        s"{\n  " + indent(v.map((k, v) => k + " = " + v).mkString(";\n")) + "\n}"
      }
    }
  }

  def initialMemory(mems: Iterable[MemorySection]) = {

    val initmem = Lst(
      mems
        .map(s =>
          Rec(
            ListMap(
              "address" -> Val(vaddress(s.address).toString),
              "name" -> Val(s.name),
              "size" -> Val(vaddress(s.address).toString),
              "readonly" -> Val(vboollit(s.readOnly).toString),
              "bytes" -> Lst(s.bytes.map(b => Val(vintlit(b.value).toString)).toList)
            )
              ++ s.region.map(r => "region" -> Val(r.name))
          )
        )
        .toList
    )

    s"let initial_memory = $initmem"

  }

  def vaddress(a: BigInt) = vintlit(a)

  def vparam(l: Variable): String = l match {
    case _: Global => Sigil.BASIR.globalVar + s"${l.name}:${vtype { l.getType }}"
    case _: LocalVar =>
      val sigil = if localSigils then Sigil.BASIR.localVar else ""
      s"$sigil${l.name}:${vtype { l.getType }}"
  }

  def pp_proc_sig(p: Procedure) = {
    val name = Sigil.BASIR.proc + p.name
    val inParams = p.formalInParam.toList.map(vparam)
    val outParams = p.formalOutParam.toList.map(vparam)

    val addr = p.address.map(l => vaddress(l).toString).map(Sigil.BASIR.attrib + "address = " + _).toList
    val pname = Seq(s"${Sigil.BASIR.attrib}name = \"${p.procName}\"")
    val retblock = p.returnBlock.map(b => s"${Sigil.BASIR.attrib}returnBlock = \"${b.label}\"")

    val allattrs = pname ++ addr ++ retblock

    val attrs = if allattrs.isEmpty then "" else "  { " + allattrs.map("" + _).mkString("; ") + " }"

    val fnl = if (p.formalInParam.size > 3) then "\n    " else " "

    val br = if (inParams.length + outParams.length > 6) then "\n " else ""

    s"proc $name$br (${inParams.mkString(", ")})$fnl-> (${outParams.mkString(", ")})\n$attrs"
  }

  override def vproc(p: Procedure): PPProg[Procedure] = {
    seenVars.clear()
    val decls = locals(p).map(vardecl)

    val inParams = p.formalInParam.toList.map(vparam)
    val outParams = p.formalOutParam.toList.map(vparam)
    val middleBlocks = p.blocksBookended.map(vblock)

    val localDecls = decls.toList.sorted

    val requires = p.requiresExpr.map(e => {
      s"  require ${vexpr(e)};"
    })
    val ensures = p.ensuresExpr.map(e => {
      s"  ensures ${vexpr(e)};"
    })

    val spec = (if (requires.nonEmpty) then "\n" + requires.mkString("\n") else "")
      + (if ensures.nonEmpty then ("\n" + ensures.mkString("\n")) else "")

    val mblocks =
      if (middleBlocks.size == 0) then None
      else {
        Some(s"[\n  " + indent(middleBlocks.mkString(";\n"), "  ") + "\n]")
      }

    val blocks = (mblocks.toList).map(_ + ";").mkString("\n")

    val header = pp_proc_sig(p) + spec

    Proc(header, localDecls, blocks)
  }

  def vproc(
    name: String,
    inParams: List[PPProg[Variable]],
    outParams: List[PPProg[Variable]],
    entryBlock: Option[PPProg[Block]],
    middleBlocks: List[PPProg[Block]],
    returnBlock: Option[PPProg[Block]]
  ): PPProg[Procedure] = ???

  private def getStatementAttribs(x: Statement): Iterable[(String, String)] = {
    (optionToMap("label")(x.label)
      ++ optionToMap("comment")(x.comment)).toList.sorted
  }

  override def vstmt(x: Statement) = {
    val orig = super.vstmt(x)

    val bst = orig.asInstanceOf[BST[?]]

    val attribs = getStatementAttribs(x)
    val attribsString = if attribs.isEmpty then {
      ""
    } else {
      attribs
        .map { (k, v) =>
          s"${Sigil.BASIR.attrib}$k = ${quote(v)}"
        }
        .mkString(" { ", "; ", " }")
    }

    BST(bst.v + attribsString)
  }

  override def vassign(lhs: PPProg[Variable], rhs: PPProg[Expr]): PPProg[LocalAssign] = BST(s"${lhs} := ${rhs}")
  override def vmemassign(lhs: PPProg[Variable], rhs: PPProg[Expr]): PPProg[LocalAssign] = BST(s"${lhs} mem:= ${rhs}")
  override def vsimulassign(assignments: List[(PPProg[Variable], PPProg[Expr])]): PPProg[SimulAssign] =
    val pref = "( "
    val suffix = " )"
    val str = assignments
      .map { case (lhs, rhs) =>
        lhs.toString + " := " + rhs
      }
      .mkString(",\n")
    BST(pref + indent(str, "  ") + suffix)

  override def vstore(
    mem: Memory,
    index: PPProg[Expr],
    value: PPProg[Expr],
    endian: Endian,
    size: Int
  ): BST[MemoryStore] = {
    val le = if endian == Endian.LittleEndian then "le" else "be"

    BST(s"store $le ${Sigil.BASIR.globalVar}${mem.name} ${index} ${value} ${size}")
  }

  def vload(lhs: PPProg[Variable], mem: String, index: PPProg[Expr], endian: Endian, size: Int): PPProg[MemoryLoad] = {
    val le = if endian == Endian.LittleEndian then "le" else "be"
    BST(s"$lhs := load $le ${Sigil.BASIR.globalVar}${mem} ${index} ${size}")
  }

  override def vcall(
    outParams: List[(Variable, Variable)],
    procname: String,
    inparams: List[(Variable, PPProg[Expr])]
  ): PPProg[DirectCall] = {

    val op = {
      if (outParams.forall(_._2.isInstanceOf[LocalVar])) {
        "var (" + outParams.map((l, r) => vparam(r)).mkString(", ") + ")"
      } else {
        "(" + outParams.map((l, r) => vlvar(r)).mkString(", ") + ")"
      }
    }

    if (outParams.size > 5) {
      BST(s"$op\n    := call ${Sigil.BASIR.proc}$procname (${inparams.map((l, r) => r).mkString(", ")})")
    } else if (outParams.size > 0) {
      BST(s"$op := call ${Sigil.BASIR.proc}$procname (${inparams.map((l, r) => r).mkString(", ")})")
    } else {
      BST(s"call ${Sigil.BASIR.proc}$procname (${inparams.map((l, r) => r).mkString(", ")})")
    }
  }

  override def vindirect(target: PPProg[Variable]): PPProg[IndirectCall] = BST(s"indirect call ${target} ")
  override def vassert(body: Assert): PPProg[Assert] = {
    BST(s"assert ${vexpr(body.body)}")
  }

  override def vassume(body: Assume): PPProg[Assume] = {
    val stmt = if body.checkSecurity then "guard" else "assume"
    BST(s"$stmt ${vexpr(body.body)}")
  }
  override def vnop(): PPProg[NOP] = BST("nop")

  override def vgoto(t: List[String]): PPProg[GoTo] = BST[GoTo](s"goto(${t.map(Sigil.BASIR.block + _).mkString(", ")})")
  override def vunreachable(): PPProg[Unreachable] = BST[Unreachable]("unreachable")
  override def vreturn(outs: List[(PPProg[Variable], PPProg[Expr])]) = BST(
    s"return (${outs.map((l, r) => r).mkString(", ")})"
  )

  def vtype(t: IRType): String = t match {
    case BitVecType(sz) => s"bv$sz"
    case IntType => "int"
    case BoolType => "bool"
    case CustomSort(n) => n
    case m: MapType => s"(${vtype(m.param)} -> ${vtype(m.result)})"
  }

  override def vrvar(e: Variable): PPProg[Variable] = e match {
    case l: LocalVar =>
      val sigil = if localSigils then Sigil.BASIR.localVar else ""
      BST(s"${sigil}${e.name}:${vtype(e.getType)}")
    case l: Global => BST(s"${Sigil.BASIR.globalVar}${e.name}:${vtype(e.getType)}")
  }
  override def vlvar(e: Variable): PPProg[Variable] = {
    e match {
      case l: LocalVar =>
        val sigil = if localSigils then Sigil.BASIR.localVar else ""
        BST(s"var $sigil${e.name}: ${vtype(e.getType)}")
      case l: Global => BST(s"${Sigil.BASIR.globalVar}${e.name}: ${vtype(e.getType)}")
    }
  }

  override def vmemory(m: Memory) = BST(Sigil.BASIR.globalVar + m.name)
  override def vold(e: Expr) = BST(s"old(${vexpr(e)})")
  override def vlambda(e: LambdaExpr) = ???
  override def vquantifier(e: QuantifierExpr) = ???
  override def vextract(ed: Int, start: Int, a: PPProg[Expr]): PPProg[Expr] = BST(s"extract($ed, $start, ${a})")
  override def vzeroextend(bits: Int, b: PPProg[Expr]): PPProg[Expr] = BST(s"zero_extend($bits, $b)")
  override def vsignextend(bits: Int, b: PPProg[Expr]): PPProg[Expr] = BST(s"sign_extend($bits, $b)")
  override def vrepeat(reps: Int, b: PPProg[Expr]) = BST(s"repeat($reps, $b)")
  override def vbinary_expr(e: BinOp, l: PPProg[Expr], r: PPProg[Expr]): PPProg[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($l, $r)")
  }
  override def vbool_expr(e: BoolBinOp, l: List[PPProg[Expr]]): PPProg[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn(${l.mkString(",")})")
  }

  override def vunary_expr(e: UnOp, arg: PPProg[Expr]): PPProg[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($arg)")
  }

  override def vboollit(b: Boolean) = BST(b.toString)
  override def vintlit(i: BigInt) = BST("0x%x".format(i))
  override def vbvlit(i: BitVecLiteral) = BST("0x%x".format(i.value) + s":bv${i.size}")
  override def vfapply_expr(name: String, args: Seq[PPProg[Expr]]): PPProg[Expr] = BST(
    s"${Sigil.BASIR.globalVar}$name(${args.mkString(", ")})"
  )
}
