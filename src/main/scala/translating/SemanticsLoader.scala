package translating
import Parsers.SemanticsParser.*
import com.google.protobuf.ByteString
import Parsers.*
import java.util.Base64
import scala.jdk.CollectionConverters._
import ir.*
import scala.collection.mutable._
import com.grammatech.gtirb.proto.Module.ByteOrder.LittleEndian

class SemanticsLoader(targetuuid: ByteString, context: SemanticsContext) extends SemanticsBaseVisitor[Any] {

  var cseMap: Map[String, IRType] = Map[String, IRType]()

  def unChrisifyUUID(uuid: String): ByteString = { // This probably needs a better name, but :/
    return ByteString.copyFrom(Base64.getDecoder().decode(uuid))
  }

  def createStatements(): ArrayBuffer[Statement] = {

    val basicBlks = context.basic_blk().asScala
    var statements: ArrayBuffer[Statement] = ArrayBuffer[Statement]()

    for (BasicBlk <- basicBlks) {
      val Blkuuid = unChrisifyUUID(BasicBlk.uuid().getText())
      if (Blkuuid.equals(targetuuid)) {
        statements = visitBasic_blk(BasicBlk)
      }
    }
    return statements
  }

  // def getCondition(): Any = {
  //   // this is just boilerplate for now, but may be useful for retriving conditions
  //   val basicBlks = context.basic_blk().asScala
  //   for (BasicBlk <- basicBlks) {
  //     val Blkuuid = unChrisifyUUID(BasicBlk.uuid().getText())
  //     if (Blkuuid.equals(targetuuid)) {
  //       visitBasic_blk(BasicBlk)
  //     }
  //   }

  // }

  override def visitBasic_blk(ctx: Basic_blkContext): ArrayBuffer[Statement] = {
    val instructions = ctx.instruction().asScala
    val statements = ArrayBuffer[Statement]()

    for (instuction <- instructions) {
      val instructionstmts = visitInstruction(instuction)
      statements ++= instructionstmts
    }

    return statements
  }

  override def visitInstruction(ctx: InstructionContext): ArrayBuffer[Statement] = {
    val statements = ArrayBuffer[Statement]()
    val stmts = ctx.stmt_string().asScala.map(_.stmt())

    for (stmt <- stmts) {
      if (stmt.assignment_stmt() != null) { // match on type would be helpful, but i can't figure out how to gat antlr to co-operate
        val statement = visitAssignment_stmt(stmt.assignment_stmt())
        statements.addOne(statement)

      } else if (stmt.call_stmt() != null) {
        val statement = visitCall_stmt(stmt.call_stmt())
        statements.addOne(statement)

      } else if (stmt.conditional_stmt() != null) {
        visitConditional_stmt(
          stmt.conditional_stmt()
        ) //  may be useful for retriving conditions, but does nothing for now

      }
    }
    return statements
  }

  def visitAssignment_stmt(ctx: Assignment_stmtContext): LocalAssign = {
    var stmt: LocalAssign = null
    ctx match
      case a: AssignContext =>
        stmt = visitAssign(a)

      case c: ConstDeclContext =>
        stmt = visitConstDecl(c)
    return stmt
  }

  override def visitCall_stmt(ctx: Call_stmtContext): MemoryAssign = {
    val mem = Memory("mem", 64, 8) // yanked from BAP
    val addr = visitExpr(ctx.expr(0))
    val value = visitExpr(ctx.expr(1))
    val size = visitExpr(ctx.expr(3)).asInstanceOf[IntLiteral].value.toInt
    val memstore = MemoryStore(mem, addr, value, Endian.LittleEndian, size)
    return MemoryAssign(mem, memstore)
  }

  override def visitConditional_stmt(ctx: Conditional_stmtContext): Any = {
    return // see above comment in conditional statements
  }

  override def visitAssign(ctx: AssignContext): LocalAssign = {
    val lhs = visitLexpr(ctx.lexpr())
    val rhs = visitExpr(ctx.expr())
    if (lhs == null || rhs == null) {
      return null
    } else {
      return LocalAssign(lhs, rhs)
    }
  }

  override def visitConstDecl(ctx: ConstDeclContext): LocalAssign = {
    val ty = visitType(ctx.`type`())
    val name = ctx.METHOD().getText()

    if (name.startsWith("Cse")) {
      cseMap += (name -> ty)
    }

    val expr = visitExpr(ctx.expr())
    if (expr != null) {
      return LocalAssign(LocalVar(name, ty), expr) //Check if localVar is right here, could be register
    } else {
      return null
    }
  }

  def visitType(ctx: TypeContext): IRType = {
    ctx match
      case e: TypeBitsContext =>
        val size = visitExpr(e.expr()).asInstanceOf[IntLiteral].value.toInt
        return BitVecType(size)
      case _: Any => ??? // can be extended as more types added to grammar
  }

  def visitExpr(ctx: ExprContext): Expr = {
    var value: Expr = null
    ctx match
      case e: ExprVarContext    => value = visitExprVar(e)
      case e: ExprTApplyContext => value = visitExprTApply(e)
      case e: ExprSlicesContext => value = visitExprSlices(e)
      case e: ExprFieldContext  => value = visitExprField(e)
      case e: ExprArrayContext  => value = visitExprArray(e)
      case e: ExprLitIntContext => value = visitExprLitInt(e)
      case e: ExprLitHexContext =>
        ??? // not in current semantics, but still unsure how this ports to IR
      case e: ExprLitBitsContext   => value = visitExprLitBits(e)
      case e: ExprLitMaskContext   => ??? // ditto above comment
      case e: ExprLitStringContext => ??? // ditto
    return value
  }

  override def visitExprVar(ctx: ExprVarContext): Expr = {
    return createExprVar(getExprVarText(ctx))
  }

  def getExprVarText(ctx: ExprVarContext): String = {
    if (ctx.SSYMBOL() != null) {
      return ctx.SSYMBOL().getText()
    } else {
      return ctx.METHOD().getText()
    }
  }

  override def visitExprTApply(ctx: ExprTApplyContext): Expr = {
    val str = ctx.METHOD.getText().substring(0, ctx.METHOD.getText().lastIndexOf("."))
    // removes everything up to and including the last dot

    str match
      case "Mem.read" =>
        val mem = Memory("mem", 64, 8) // yanked from BAP, hope this never changes
        val addr = visitExpr(ctx.expr(0))
        val size = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
        return MemoryLoad(mem, addr, Endian.LittleEndian, size)
      case "not_bool"    => return UnaryExpr(BoolNOT, visitExpr(ctx.expr(0)))
      case "cvt_bool_bv" =>
        // in ocaml, takes bool and turns to bitvector -> since this is usually tied to a BinaryExpr, and
        // BinaryExpr's don't have any "evaluate" method, i have just returned the binary expr that will
        //evaluate to a bool
        return visitExpr(ctx.expr(0))
      case "eq_enum"  => return BinaryExpr(BVXNOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "or_bool"  => return BinaryExpr(BoolOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "and_bool" => return BinaryExpr(BoolAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "replicate_bits" =>
        val value = visitExpr(ctx.expr(0)).asInstanceOf[IntLiteral].value
        // not 100% on this, since it hasn't come up yet
        val size = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
        return BitVecLiteral(value, size)
      case "not_bits"    => return UnaryExpr(BVNOT, visitExpr(ctx.expr(0)))
      case "or_bits"     => return BinaryExpr(BVOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "and_bits"    => return BinaryExpr(BVAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "eor_bits"    => return BinaryExpr(BVXOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "eq_bits"     => return BinaryExpr(BVEQ, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "add_bits"    => return BinaryExpr(BVADD, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "sub_bits"    => return BinaryExpr(BVSUB, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "mul_bits"    => return BinaryExpr(BVMUL, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "sdiv_bits"   => return BinaryExpr(BVSDIV, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "lsl_bits"    => return ??? //  can't find logical shift left binop?
      case "lsr_bits"    => return BinaryExpr(BVLSHR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "asr_bits"    => return BinaryExpr(BVASHR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "slt_bits"    => return BinaryExpr(BVSLT, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "sle_bits"    => return BinaryExpr(BVSLE, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "append_bits" =>
        // there is no append but there is concat, so it probably does the same thing
        return BinaryExpr(BVCONCAT, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "ZeroExtend" =>
        val intexpr = ctx.targs().asScala.last.expr() match {
          case e: ExprLitIntContext =>
            if (e.DEC() != null) {
              e.DEC().getText().toInt
            } else {
              e.BINARY().getText().toInt
            }
        }
        return ZeroExtend(intexpr, visitExpr(ctx.expr(0)))

      case "SignExtend" =>
        val intexpr = ctx.targs().asScala.last.expr() match {
          case e: ExprLitIntContext =>
            if (e.DEC() != null) {
              e.DEC().getText().toInt
            } else {
              e.BINARY().getText().toInt
            }
        }
        return SignExtend(intexpr, visitExpr(ctx.expr(0)))

  }

  override def visitExprSlices(ctx: ExprSlicesContext): Expr = {
    val (start, end) = visitSlice_expr(ctx.slice_expr()): @unchecked
    return Extract(start.asInstanceOf[Int], end.asInstanceOf[Int], visitExpr(ctx.expr()))
  }

  override def visitSlice_expr(ctx: Slice_exprContext): Tuple = {
    val start = visitExpr(ctx.expr(0)).asInstanceOf[IntLiteral].value.toInt
    val end = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
    return (start, end)
  }

  override def visitExprField(ctx: ExprFieldContext): Expr = {
    var value: String = null
    ctx.expr() match
      case e: ExprVarContext => value = getExprVarText(e)
      case _: Any            => visitExpr(ctx.expr())
    val field: ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText())
    return createExprVarArray(field)
  }

  override def visitExprArray(ctx: ExprArrayContext): Expr = {
    val array: ArrayBuffer[String] = ctx
      .expr()
      .asScala
      .map {
        case e: ExprVarContext => getExprVarText(e)
        case e: ExprLitIntContext =>
          if (e.DEC() != null) {
            e.DEC().getText()
          } else {
            e.BINARY().getText()
          }
        case e: ExprLitBitsContext => Integer.parseInt(e.BINARY().getText(), 2).asInstanceOf[String]
      }
      .to(ArrayBuffer)

    createExprVarArray(array)
  }

  override def visitExprLitInt(ctx: ExprLitIntContext): Expr = {
    //I'm too scared to change the grammar at this point in time, so i'm just going to roll with stuff like this for now
    if (ctx.DEC() != null) {
      IntLiteral(ctx.DEC().getText().toInt)
    } else {
      IntLiteral(ctx.BINARY().getText().toInt)
    }

  }

  override def visitExprLitBits(ctx: ExprLitBitsContext): Expr = {
    return BitVecLiteral(BigInt.apply(ctx.BINARY().getText(), 2), ctx.BINARY().getText().length())
  }

  def visitLexpr(ctx: LexprContext): Variable = {
    var value: Variable = null
    ctx match
      case l: LExprVarContext =>
        value = visitLExprVar(l)
      case l: LExprFieldContext =>
        value = visitLExprField(l)
      case l: LExprArrayContext =>
        value = visitLExprArray(l)
    return value
  }

  override def visitLExprVar(ctx: LExprVarContext): Variable = {
    return createExprVar(getLExprVarText(ctx)).asInstanceOf[Variable]
  }

  def getLExprVarText(ctx: LExprVarContext): String = {
    if (ctx.SSYMBOL() != null) {
      return ctx.SSYMBOL().getText()
    } else {
      return ctx.METHOD().getText()
    }
  }

  override def visitLExprField(ctx: LExprFieldContext): Variable = {
    var value: String = null
    ctx.lexpr() match
      case l: LExprVarContext => value = getLExprVarText(l)
      case _: Any             => visitLexpr(ctx.lexpr())
    val field: ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText())
    return createExprVarArray(field)
  }

  override def visitLExprArray(ctx: LExprArrayContext): Variable = {

    val lvalue: String = ctx.lexpr() match {
      case l: LExprVarContext => getLExprVarText(l)
      case _                  => "error"
    }
    val exprs = ctx.expr().asScala

    val array: ArrayBuffer[String] = ArrayBuffer(lvalue) ++= exprs.map {
      case e: ExprVarContext => getExprVarText(e)
      case e: ExprLitIntContext =>
        if (e.DEC() != null) {
          e.DEC().getText()
        } else {
          e.BINARY().getText()
        }
      case e: ExprLitBitsContext => Integer.parseInt(e.BINARY().getText(), 2).asInstanceOf[String]
      case _                     => ???
    }

    createExprVarArray(array)
  }

  def createExprVar(name: String): Expr = {
    name match
      case n if n.startsWith("Cse") => return LocalVar(name, cseMap.get(name).get)
      case "TRUE"                   => return TrueLiteral
      case "FALSE"                  => return FalseLiteral
      case "SP_EL0"                 => return Register("R31", BitVecType(64))
      case "_PC" =>
        Register(
          "_PC",
          BitVecType(1)
        ) // null elements literally don't exist in IR, so pray we never have to read from them
      // TODO: figure out what to do with this, since they do appear :/
      // Nick says to just remove them, which i will soon
      case "__BranchTaken" =>
        Register(
          "__BranchTaken",
          BitVecType(1)
        )
      case "BTypeNext" =>
        Register(
          "__BTypeNext",
          BitVecType(1)
        )
  }

  def createExprVarArray(v: ArrayBuffer[String]): Variable = {
    v(0) match //currently only works for fields & arrays of size 2, but arrays bigger than that don't seem to appear
      case "_R" =>
        val size = getSizeofRegister(v(0))
        val name = "R" + v(1)
        return Register(name, BitVecType(size))

      case "PSTATE" =>
        val Rname = v(1).asInstanceOf[String] + "F"
        return LocalVar(Rname, BitVecType(1))
  }

  def getSizeofRegister(name: String): Int = {
    name match
      case "_R" => return 64
      case "_Z" => return 128 // or _V?  vector registers haven't come up yet
  }

}
