package translating

import Parsers.BAP_ADTParser._
import bap._
import ir.Endian

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object BAPLoader {

  def visitProject(ctx: ProjectContext): BAPProgram = {
    val memorySections = visitSections(ctx.sections)
    val subroutines = visitProgram(ctx.program)
    BAPProgram(subroutines, memorySections)
  }

  def visitSections(ctx: SectionsContext): List[BAPMemorySection] = {
    ctx.section.asScala.map(visitSection).toList
  }

  def visitSection(ctx: SectionContext): BAPMemorySection = {
    val bytes = ctx.membyte.asScala.map(visitByte).toSeq
    BAPMemorySection(visitQuoteString(ctx.name), parseInt(ctx.address), bytes.size, bytes)
  }

  def visitByte(ctx: MembyteContext): BAPLiteral = {
    BAPLiteral(Integer.parseInt(ctx.getText.stripPrefix("\\x"), 16), 8)
  }

  def visitProgram(ctx: ProgramContext): List[BAPSubroutine] = ctx.subs.sub.asScala.map(visitSub).toList

  @tailrec
  def visitExp(ctx: ExpContext): BAPExpr = ctx match {
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

  def visitLoad(ctx: LoadContext): BAPMemAccess = {
    BAPMemAccess(visitMemVar(ctx.memVar), visitExp(ctx.idx), visitEndian(ctx.endian), parseInt(ctx.num))
  }

  def visitStore(ctx: StoreContext): BAPStore = {
    BAPStore(
      visitMemVar(ctx.memVar),
      visitExp(ctx.idx),
      visitExp(ctx.value),
      visitEndian(ctx.endian),
      parseInt(ctx.num)
    )
  }

  def visitBinOp(ctx: BinOpContext): BAPBinOp = {
    BAPBinOp(BAPBinOperator(ctx.op.getText), visitExp(ctx.lhs), visitExp(ctx.rhs))
  }

  def visitUOp(ctx: UOpContext): BAPUnOp = {
    BAPUnOp(BAPUnOperator(ctx.op.getText), visitExp(ctx.exp))
  }

  def visitImmVar(ctx: ImmVarContext): BAPVar = {
    val name = parseAllowed(visitQuoteString(ctx.name))
    if (((name.startsWith("R") || name.startsWith("V"))
      && (name.length == 2 || name.length == 3)
      && name.substring(1).forall(_.isDigit)) ||
      (name == "NF" || name == "ZF" || name == "CF" || name == "VF")
    ) {
      BAPRegister(name, parseInt(ctx.size))
    } else {
      BAPLocalVar(name, parseInt(ctx.size))
    }
  }

  def visitMemVar(ctx: MemVarContext): BAPMemory = {
    BAPMemory(parseAllowed(visitQuoteString(ctx.name)), parseInt(ctx.addr_size), parseInt(ctx.value_size))
  }

  def visitExpInt(ctx: ExpIntContext): BAPLiteral = {
    BAPLiteral(parseBigInt(ctx.value), parseInt(ctx.size))
  }

  def visitCast(ctx: CastContext): BAPExpr = ctx.CAST.getText match {
    case "UNSIGNED" => BAPUnsignedExtend(parseInt(ctx.size), visitExp(ctx.exp))
    case "SIGNED"   => BAPSignedExtend(parseInt(ctx.size), visitExp(ctx.exp))
    case "LOW"      => BAPLowCast(parseInt(ctx.size), visitExp(ctx.exp))
    case "HIGH"     => BAPHighCast(parseInt(ctx.size), visitExp(ctx.exp))
  }

  def visitExtract(ctx: ExtractContext): BAPExtract = {
    BAPExtract(parseInt(ctx.hb), parseInt(ctx.lb), visitExp(ctx.exp))
  }

  def visitConcat(ctx: ConcatContext): BAPConcat = {
    BAPConcat(visitExp(ctx.lhs), visitExp(ctx.rhs))
  }

  def visitJmp(ctx: JmpContext): BAPJump = ctx match {
    case i: IndirectCallContext => visitIndirectCall(i)
    case d: DirectCallContext   => visitDirectCall(d)
    case g: GotoJmpContext      => visitGotoJmp(g)
  }

  def visitIndirectCall(ctx: IndirectCallContext): BAPIndirectCall = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    checkCondition(ctx.cond, ctx)
    BAPIndirectCall(visitImmVar(ctx.callee.immVar), returnTarget, line, insn)
  }

  def visitDirectCall(ctx: DirectCallContext): BAPDirectCall = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    checkCondition(ctx.cond, ctx)
    BAPDirectCall(
      parseAllowed(visitQuoteString(ctx.callee.tid.name).stripPrefix("@")),
      returnTarget,
      line,
      insn
    )
  }

  def checkCondition(condition: ExpContext, ctx: JmpContext): Unit = {
    val conditionParsed = visitExp(condition)
    if (conditionParsed != BAPLiteral(1, 1)) {
      // If this is thrown then we have will have to actually support BAP giving calls (as opposed to gotos).
      // This is not something that it seems like the ARM64 instruction set should produce.
      throw BAPCallConditionParsingException(s"Error parsing BAP at \"${ctx.getText}\": call contains non-true condition: \"${condition.getText}\", parsed as $conditionParsed")
    }
  }

  def visitGotoJmp(ctx: GotoJmpContext): BAPGoTo = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    BAPGoTo(parseLabel(ctx.target.tid.name), visitExp(ctx.cond), line, insn)
  }

  def visitArg(ctx: ArgContext): (Option[BAPParameter], Option[BAPParameter]) = {
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
      case "In()"  => (Some(BAPParameter(lhs.name, lhs.size, rhs)), None)
      case "Out()" => (None, Some(BAPParameter(lhs.name, lhs.size, rhs)))
      case "Both()" =>
        (Some(BAPParameter(lhs.name, lhs.size, rhs)), Some(BAPParameter(lhs.name + "_out", lhs.size, rhs)))
      case _ => (None, None)
    }
  }

  def visitSub(ctx: SubContext): BAPSubroutine = {
    val inOut = ctx.args.arg.asScala.map { arg => visitArg(arg) }.unzip
    val in = inOut._1.flatten
    val out = inOut._2.flatten
    /*
    val alwaysIn = List(
      BAPParameter("FP", 64, BAPLocalVar("R29", 64)),
      BAPParameter("LR", 64, BAPLocalVar("R30", 64)),
      BAPParameter("SP", 64, BAPLocalVar("R31", 64))
    )
    val alwaysOut = List(
      BAPParameter("FP_out", 64, BAPLocalVar("R29", 64)),
      BAPParameter("LR_out", 64, BAPLocalVar("R30", 64)),
      BAPParameter("SP_out", 64, BAPLocalVar("R31", 64))
    )
     */

    val address = parseFromAttrs(ctx.attrs, "address") match {
      case Some(x: String) => Integer.parseInt(x.stripPrefix("0x"), 16)
      case None            => -1
    }

    BAPSubroutine(
      parseAllowed(visitQuoteString(ctx.name)),
      address,
      ctx.blks.blk.asScala.map(visitBlk).toList,
      in.toList,
      out.toList
    )
  }

  def visitBlk(ctx: BlkContext): BAPBlock = {
    val statements: List[BAPStatement] = ctx.defs.assign.asScala.map(visitAssign).toList
    val jumps: List[BAPJump] = ctx.jmps.jmp.asScala.map(visitJmp).toList

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

    BAPBlock(label, address, statements, jumps)
  }

  def visitEndian(ctx: EndianContext): Endian = ctx.ENDIAN.getText match {
    case "LittleEndian" => Endian.LittleEndian
    case "BigEndian"    => Endian.BigEndian
  }

  def visitAssign(ctx: AssignContext): BAPStatement = ctx match {
    case imm: ImmDefContext => visitImmDef(imm)
    case mem: MemDefContext => visitMemDef(mem)
  }

  def visitImmDef(ctx: ImmDefContext): BAPLocalAssign = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    val addr = parseFromAttrs(ctx.attrs, "address").map(x => Integer.parseInt(x.stripPrefix("0x"), 16));
    BAPLocalAssign(visitImmVar(ctx.lhs), visitExp(ctx.rhs), line, insn, addr)
  }

  def visitMemDef(ctx: MemDefContext): BAPMemAssign = {
    val line = visitQuoteString(ctx.tid.name)
    val insn = parseFromAttrs(ctx.attrs, "insn").getOrElse("")
    val addr = parseFromAttrs(ctx.attrs, "address").map(x => Integer.parseInt(x.stripPrefix("0x"), 16));
    BAPMemAssign(visitMemVar(ctx.lhs), visitStore(ctx.rhs), line, insn, addr)
  }

  def visitQuoteString(ctx: QuoteStringContext): String = ctx.getText.stripPrefix("\"").stripSuffix("\"")

  def parseAllowed(s: String): String = s.map(c => if allowedChars.contains(c) then c else '$')

  def parseLabel(ctx: QuoteStringContext): String =
    "l" + parseAllowed(visitQuoteString(ctx).stripPrefix("@").stripPrefix("%"))

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

  val allowedChars: Set[Char] =
    Set('_', '\'', '~', '#', '$', '^', '_', '.', '?', '`') ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')

  class BAPCallConditionParsingException(message: String)
    extends Exception(message) {

    def this(message: String, cause: Throwable) = {
      this(message)
      initCause(cause)
    }
  }

}
