package translating
import ir.*
import ir.cilvisitor.*
import scala.collection.immutable.ListMap
import scala.collection.mutable

object PrettyPrinter {
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
    globalDecls.map(_ + ";").mkString("\n") + "\n\n" + procedures.map(_.toString + ";\n").mkString("")
}

case class Proc(signature: String, localDecls: List[String], blocks: String) extends PPProg[Procedure] {
  override def toString = {
    if (blocks.size > 0) {
      signature + "\n{" + "\n" + blocks + "\n}"
    } else {
      signature + " {}"
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
    val indent = "  "
    val addr = address.map(" " + _).getOrElse("")
    val comment = entryComment.map(c => c + "\n").getOrElse("")
    val excomment = exitComment.map(c => "\n" + c).getOrElse("")
    s"block ${label}${addr} [\n${comment}"
      ++ commands.map("  " + _).mkString(";\n")
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

  override def vprog(p: Program): PPProg[Program] = {
    Prog(
      p.mainProcedure.name,
      memoryRegions(p).toList.sortBy(_.name).map(memdecl) ++
        globals(p).toList.sorted.map(vardecl)
        ++ List("\nlet entry_procedure = " + p.mainProcedure.name)
      // ++ List(initialMemory(p.initialMemory.values))
      ,
      p.procedures.toList.map(vproc).collect {
        case p: Proc => p
        case _ => ???
      }
    )
  }

  def vprog(mainProc: String, procedures: List[PPProg[Procedure]]): PPProg[Program] = {
    // shouldn't be used
    assert(false)
  }

  override def vblock(b: Block): PPProg[Block] = {
    val label = b.label
    val address = b.address
    val statements = b.statements.toList.map(vstmt)
    val terminator = vjump(b.jump)
    val entryComent = with_analysis_results_begin(b).map(c => s"${blockIndent}// ${c}")
    val exitComment = with_analysis_results_end(b).map(c => s"${blockIndent}// ${c}")
    val addr = address.map(a => s"{address = ${vaddress(a)}}")
    PBlock(label, addr, statements.map(_.toString) ++ Seq(terminator.toString), entryComent, exitComment)
  }

  override def vblock(
    label: String,
    address: Option[BigInt],
    statements: List[PPProg[Statement]],
    terminator: PPProg[Jump]
  ): PPProg[Block] = {
    assert(false)
  }

  def vardecl(v: Variable): String = {
    s"var ${v.name} : ${vtype(v.getType)}"
  }

  def memdecl(m: Memory): String = {
    s"memory ${m.name} : ${vtype(m.getType)}"

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

  def vparam(l: Variable): String = {
    s"${l.name}:${vtype { l.getType }}"
  }

  def pp_proc_sig(p: Procedure) = {
    val name = p.name
    val inParams = p.formalInParam.toList.map(vparam)
    val outParams = p.formalOutParam.toList.map(vparam)
    s"proc $name(${inParams.mkString(", ")}) -> (${outParams.mkString(", ")})"
  }

  override def vproc(p: Procedure): PPProg[Procedure] = {
    seenVars.clear()
    val decls = locals(p).map(vardecl)

    val name = p.name
    val inParams = p.formalInParam.toList.map(vparam)
    val outParams = p.formalOutParam.toList.map(vparam)
    val entryBlock = p.entryBlock
    val middleBlocks =
      (p.entryBlock.toList ++ (p.blocks.toSet -- p.entryBlock.toSet -- p.returnBlock.toSet).toList.sortBy(x =>
        -x.rpoOrder
      )
        ++ p.returnBlock).map(vblock)
    val returnBlock = p.returnBlock.map(vblock)

    val localDecls = decls.toList.sorted

    val iblocks = p.entryBlock.map(b => (s"  entry_block = " + '"' + b.label + '"')).toList

    val addr = p.address.map(l => vaddress(l).toString).map("  address = " + _).toList

    val mblocks =
      if (middleBlocks.size == 0) then None
      else {
        Some(s"  blocks = [\n    " + indent(middleBlocks.mkString(";\n"), "    ") + "\n  ]")
      }

    val pname = Seq(s"  name = \"${p.procName}\"")

    val blocks = (pname ++ addr ++ iblocks ++ mblocks.toList).map(_ + ";").mkString("\n")

    val header = pp_proc_sig(p)

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

  override def vassign(lhs: PPProg[Variable], rhs: PPProg[Expr]): PPProg[LocalAssign] = BST(s"${lhs} := ${rhs}")

  override def vstore(
    mem: String,
    index: PPProg[Expr],
    value: PPProg[Expr],
    endian: Endian,
    size: Int
  ): BST[MemoryStore] = {
    val le = if endian == Endian.LittleEndian then "le" else "be"
    BST(s"store $le ${mem} ${index} ${value} ${size}")
  }

  def vload(lhs: PPProg[Variable], mem: String, index: PPProg[Expr], endian: Endian, size: Int): PPProg[MemoryLoad] = {
    val le = if endian == Endian.LittleEndian then "le" else "be"
    BST(s"$lhs := load $le ${mem} ${index} ${size}")
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
      BST(s"$op\n    := call $procname (${inparams.map((l, r) => r).mkString(", ")})")
    } else if (outParams.size > 0) {
      BST(s"$op := call $procname (${inparams.map((l, r) => r).mkString(", ")})")
    } else {
      BST(s"call $procname (${inparams.map((l, r) => r).mkString(", ")})")
    }
  }

  override def vindirect(target: PPProg[Variable]): PPProg[IndirectCall] = BST(s"indirect call ${target} ")
  override def vassert(body: Assert): PPProg[Assert] = {
    val comment = body.comment.map(c => s" /* $c */").getOrElse("")
    BST(s"assert ${vexpr(body.body)}$comment")
  }
  override def vassume(body: Assume): PPProg[Assume] = {
    val comment = body.comment.map(c => s" /* $c */").getOrElse("")
    BST(s"assume ${vexpr(body.body)}$comment")
  }
  override def vnop(): PPProg[NOP] = BST("nop")

  override def vgoto(t: List[String]): PPProg[GoTo] = BST[GoTo](s"goto(${t.mkString(", ")})")
  override def vunreachable(): PPProg[Unreachable] = BST[Unreachable]("unreachable")
  override def vreturn(outs: List[(PPProg[Variable], PPProg[Expr])]) = BST(
    s"return (${outs.map((l, r) => r).mkString(", ")})"
  )

  def vtype(t: IRType): String = t match {
    case BitVecType(sz) => s"bv$sz"
    case IntType => "nat"
    case BoolType => "bool"
    case m: MapType => s"map ${vtype(m.result)}[${vtype(m.param)}]"
  }

  override def vrvar(e: Variable): PPProg[Variable] = BST(s"${e.name}:${vtype(e.getType)}")
  override def vlvar(e: Variable): PPProg[Variable] = {
    e match {
      case l: LocalVar => BST("var " + e.name + s": ${vtype(e.getType)}")
      case l => BST(e.name + s": ${vtype(e.getType)}")
    }
  }

  override def vextract(ed: Int, start: Int, a: PPProg[Expr]): PPProg[Expr] = BST(s"extract($ed, $start, ${a})")
  override def vzeroextend(bits: Int, b: PPProg[Expr]): PPProg[Expr] = BST(s"zero_extend($bits, $b)")
  override def vsignextend(bits: Int, b: PPProg[Expr]): PPProg[Expr] = BST(s"sign_extend($bits, $b)")
  override def vrepeat(reps: Int, b: PPProg[Expr]) = BST(s"repeat($reps, $b)")
  override def vbinary_expr(e: BinOp, l: PPProg[Expr], r: PPProg[Expr]): PPProg[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($l, $r)")
  }
  override def vunary_expr(e: UnOp, arg: PPProg[Expr]): PPProg[Expr] = {
    val opn = e.getClass.getSimpleName.toLowerCase.stripSuffix("$")
    BST(s"$opn($arg)")
  }

  override def vboollit(b: Boolean) = BST(b.toString)
  override def vintlit(i: BigInt) = BST("0x%x".format(i))
  override def vbvlit(i: BitVecLiteral) = BST("0x%x".format(i.value) + s":bv${i.size}")
  override def vuninterp_function(name: String, args: Seq[PPProg[Expr]]): PPProg[Expr] = BST(
    s"$name(${args.mkString(", ")})"
  )
}
