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
    LocalVar(visitQuoteString(ctx.name), parseInt(ctx.size))
  }

  def visitMemVar(ctx: MemVarContext): Memory = {
    Memory(visitQuoteString(ctx.name), parseInt(ctx.addr_size), parseInt(ctx.value_size))
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

  def visitJmp(ctx: JmpContext): (String, Statement) = ctx match {
    case i: IndirectCallContext => visitIndirectCall(i)
    case d: DirectCallContext   => visitDirectCall(d)
    case g: GotoJmpContext      => visitGotoJmp(g)
  }

  def visitIndirectCall(ctx: IndirectCallContext): (String, IndirectCall) = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val jump = IndirectCall(visitImmVar(ctx.callee.immVar), visitExp(ctx.cond), returnTarget)
    (parseFromAttrs(ctx.attrs, "insn").getOrElse(""), jump)
  }

  def visitDirectCall(ctx: DirectCallContext): (String, DirectCall) = {
    val returnTarget = Option(ctx.returnTarget) match {
      case Some(r: DirectContext) => Some(parseLabel(r.tid.name))
      case None                   => None
    }
    val jump = DirectCall(visitQuoteString(ctx.callee.tid.name).stripPrefix("@"), visitExp(ctx.cond), returnTarget)
    (parseFromAttrs(ctx.attrs, "insn").getOrElse(""), jump)
  }

  def visitGotoJmp(ctx: GotoJmpContext): (String, GoTo) = {
    val jump = GoTo(parseLabel(ctx.target.tid.name), visitExp(ctx.cond))
    (parseFromAttrs(ctx.attrs, "insn").getOrElse(""), jump)
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

  def visitSub(ctx: SubContext): FunctionNode = {
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

    FunctionNode(
      visitQuoteString(ctx.name),
      address,
      ctx.blks.blk.asScala.map(visitBlk).toList,
      in.toList ++ alwaysIn,
      out.toList ++ alwaysOut
    )
  }

  def visitBlk(ctx: BlkContext): Block = {
    val statements: Vector[(String, Statement)] = ctx.defs.assign.asScala.map(visitAssign).toVector ++
      ctx.jmps.jmp.asScala.map(visitJmp).toVector

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

    val label = parseLabel(ctx.tid.name)
    val address = parseFromAttrs(ctx.attrs, "address") match {
      case Some(x: String) => Some(Integer.parseInt(x.stripPrefix("0x"), 16))
      case None            => None
    }

    Block(label, address, instructions.toList)
  }

  def visitEndian(ctx: EndianContext): Endian = ctx.ENDIAN.getText match {
    case "LittleEndian" => Endian.LittleEndian
    case "BigEndian"    => Endian.BigEndian
  }

  def visitAssign(ctx: AssignContext): (String, Statement) = ctx match {
    case imm: ImmDefContext => visitImmDef(imm)
    case mem: MemDefContext => visitMemDef(mem)
  }

  def visitImmDef(ctx: ImmDefContext): (String, LocalAssign) = {
    val assign = LocalAssign(visitImmVar(ctx.lhs), visitExp(ctx.rhs))
    (parseFromAttrs(ctx.attrs, "insn").getOrElse(""), assign)
  }

  def visitMemDef(ctx: MemDefContext): (String, MemAssign) = {
    val assign = MemAssign.init(visitMemVar(ctx.lhs), visitStore(ctx.rhs))
    (parseFromAttrs(ctx.attrs, "insn").getOrElse(""), assign)
  }

  def visitQuoteString(ctx: QuoteStringContext): String = ctx.getText.stripPrefix("\"").stripSuffix("\"")

  def parseLabel(ctx: QuoteStringContext): String = "l" + visitQuoteString(ctx).stripPrefix("@").stripPrefix("%")

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

}
