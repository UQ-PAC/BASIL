package translating

import BilParser.BilAdtParser._
import astnodes._
import util.AssumptionViolationException

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object AdtStatementLoader {

  def visitProject(ctx: ProjectContext): Program = visitProgram(ctx.program)

  def visitProgram(ctx: ProgramContext): Program = {
    val functions = ctx.subs.sub.asScala.map(visitSub).toList
    Program(functions)
  }

  @tailrec
  def visitExp(ctx: ExpContext): Expr = ctx match {
    case e: ExpParenContext  => visitExp(e.exp)
    case e: LoadContext      => visitLoad(e)
    case e: BinOpContext     => visitBinOp(e)
    case e: UOpContext       => visitUOp(e)
    case e: ExpImmVarContext => visitImmVar(e.immVar)
    case e: ExpIntContext    => visitExpInt(e)
    case e: ExpCastContext   => visitCast(e.cast)
    case e: ExtractContext   => visitExtract(e)
    case e: ConcatContext    => visitConcat(e)
  }

  def visitLoad(ctx: LoadContext): MemAccess = {
    MemAccess.init(visitMemVar(ctx.memVar), visitExp(ctx.idx), visitEndian(ctx.endian), parseInt(ctx.num))
  }

  def visitStore(ctx: StoreContext): Store = {
    Store.init(
      visitMemVar(ctx.memVar),
      visitExp(ctx.idx),
      visitExp(ctx.value),
      visitEndian(ctx.endian),
      parseInt(ctx.num)
    )
  }

  def visitBinOp(ctx: BinOpContext): BinOp = {
    BinOp(BinOperator(ctx.op.getText), visitExp(ctx.lhs), visitExp(ctx.rhs))
  }

  def visitUOp(ctx: UOpContext): UnOp = {
    UnOp(UnOperator(ctx.op.getText), visitExp(ctx.exp))
  }

  def visitImmVar(ctx: ImmVarContext): LocalVar = {
    LocalVar(parseAllowed(visitQuoteString(ctx.name)), parseInt(ctx.size))
  }

  def visitMemVar(ctx: MemVarContext): Memory = {
    Memory(parseAllowed(visitQuoteString(ctx.name)), parseInt(ctx.addr_size), parseInt(ctx.value_size))
  }

  def visitExpInt(ctx: ExpIntContext): Literal = {
    Literal(parseBigInt(ctx.value), parseInt(ctx.size))
  }

  def visitCast(ctx: CastContext): Expr = ctx.CAST.getText match {
    case "UNSIGNED" => UnsignedExtend(parseInt(ctx.size), visitExp(ctx.exp))
    case "SIGNED"   => SignedExtend(parseInt(ctx.size), visitExp(ctx.exp))
    case "LOW"      => LowCast(parseInt(ctx.size), visitExp(ctx.exp))
    case "HIGH"     => HighCast(parseInt(ctx.size), visitExp(ctx.exp))
  }

  def visitExtract(ctx: ExtractContext): Extract = {
    Extract(parseInt(ctx.hb), parseInt(ctx.lb), visitExp(ctx.exp))
  }

  def visitConcat(ctx: ConcatContext): Concat = {
    Concat(visitExp(ctx.lhs), visitExp(ctx.rhs))
  }

  def visitJmp(ctx: JmpContext): Statement = ctx match {
    case i: IndirectCallContext => visitIndirectCall(i)
    case d: DirectCallContext   => visitDirectCall(d)
    case g: GotoJmpContext      => visitGotoJmp(g)
  }

  def visitIndirectCall(ctx: IndirectCallContext): IndirectCall = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    IndirectCall(visitImmVar(ctx.callee.immVar), visitExp(ctx.cond), returnTarget, line, insn)
  }

  def visitDirectCall(ctx: DirectCallContext): DirectCall = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    DirectCall(parseAllowed(visitQuoteString(ctx.callee.tid.name).stripPrefix("@")), visitExp(ctx.cond), returnTarget, line, insn)
  }

  def visitGotoJmp(ctx: GotoJmpContext): GoTo = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    GoTo(parseLabel(ctx.target.tid.name), visitExp(ctx.cond), line, insn)
  }

  def visitArg(ctx: ArgContext): (Option[Parameter], Option[Parameter]) = {
    val lhs = visitImmVar(ctx.lhs)
    val rhs = ctx.rhs match {
      case i: ImmOptContext => visitImmVar(i.immVar)
      case c: CastOptContext =>
        c.cast.exp match {
          case e: ExpImmVarContext => visitImmVar(e.immVar)
          case _                   => return (None, None)
        }
    }
    ctx.intent.getText match {
      case "In()"   => (Some(Parameter(lhs.name, lhs.size, rhs)), None)
      case "Out()"  => (None, Some(Parameter(lhs.name, lhs.size, rhs)))
      case "Both()" => (Some(Parameter(lhs.name, lhs.size, rhs)), Some(Parameter(lhs.name + "_out", lhs.size, rhs)))
      case _        => (None, None)
    }
  }

  def visitSub(ctx: SubContext): Subroutine = {
    val inOut = ctx.args.arg.asScala.map { arg => visitArg(arg) }.unzip
    val in = inOut._1.flatten
    val out = inOut._2.flatten
    val alwaysIn = List(
      Parameter("FP", 64, LocalVar("R29", 64)),
      Parameter("LR", 64, LocalVar("R30", 64)),
      Parameter("SP", 64, LocalVar("R31", 64))
    )
    val alwaysOut = List(
      Parameter("FP_out", 64, LocalVar("R29", 64)),
      Parameter("LR_out", 64, LocalVar("R30", 64)),
      Parameter("SP_out", 64, LocalVar("R31", 64))
    )

    val address = parseFromAttrs(ctx.attrs, "address") match {
      case Some(x: String) => Integer.parseInt(x.stripPrefix("0x"), 16)
      case None            => -1
    }

    Subroutine(
      parseAllowed(visitQuoteString(ctx.name)),
      address,
      ctx.blks.blk.asScala.map(visitBlk).toList,
      in.toList ++ alwaysIn,
      out.toList ++ alwaysOut
    )
  }

  def visitBlk(ctx: BlkContext): Block = {
    val statements: List[Statement] = ctx.defs.assign.asScala.map(visitAssign).toList ++
      ctx.jmps.jmp.asScala.map(visitJmp).toList

    /*
    // group by instruction
    val instructions = statements.foldLeft(Vector[Instruction]()) { (insns, kv) =>
      val instruction = kv._1
      val statement = kv._2
      insns.lastOption match {
        case Some(i) =>
          if (i.asm == instruction) {
            insns.dropRight(1) :+ i.copy(statements = i.statements :+ statement)
          } else {
            insns :+ Instruction(instruction, List(statement))
          }
        case None => Vector(Instruction(instruction, List(statement)))
      }
    }
    */

    val label = parseLabel(ctx.tid.name)
    val address = parseFromAttrs(ctx.attrs, "address") match {
      case Some(x: String) => Some(Integer.parseInt(x.stripPrefix("0x"), 16))
      case None            => None
    }

    Block(label, address, statements)
  }

  def visitEndian(ctx: EndianContext): Endian = ctx.ENDIAN.getText match {
    case "LittleEndian" => Endian.LittleEndian
    case "BigEndian"    => Endian.BigEndian
  }

  def visitAssign(ctx: AssignContext): Statement = ctx match {
    case imm: ImmDefContext => visitImmDef(imm)
    case mem: MemDefContext => visitMemDef(mem)
  }

  def visitImmDef(ctx: ImmDefContext): LocalAssign = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    LocalAssign(visitImmVar(ctx.lhs), visitExp(ctx.rhs), line, insn)
  }

  def visitMemDef(ctx: MemDefContext): MemAssign = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    MemAssign.init(visitMemVar(ctx.lhs), visitStore(ctx.rhs), line, insn)
  }

  def visitQuoteString(ctx: QuoteStringContext): String = ctx.getText.stripPrefix("\"").stripSuffix("\"")

  def parseAllowed(s: String): String = s.map(c => if allowedChars.contains(c) then c else '$')

  def parseLabel(ctx: QuoteStringContext): String = "l" + parseAllowed(visitQuoteString(ctx).stripPrefix("@").stripPrefix("%"))

  def parseFromAttrs(ctx: AttrsContext, field: String): Option[String] = {
    ctx.attr.asScala.map(visitAttr).collectFirst {
      case (lhs: String, rhs: String) if lhs == field => rhs
    }
  }

  def parseInt(ctx: NumContext): Int = ctx match {
    case _: NumHexContext => Integer.parseInt(ctx.getText.stripPrefix("0x"), 16)
    case _: NumDecContext => ctx.getText.toInt
  }

  def parseBigInt(ctx: NumContext): BigInt = ctx match {
    case _: NumHexContext => BigInt(ctx.getText.stripPrefix("0x"), 16)
    case _: NumDecContext => BigInt(ctx.getText)
  }

  def visitAttr(ctx: AttrContext): (String, String) = (visitQuoteString(ctx.lhs), visitQuoteString(ctx.rhs))

  val allowedChars: Set[Char] = Set('_', '\'', '~', '#', '$', '^', '_', '.', '?', '`') ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')

}
