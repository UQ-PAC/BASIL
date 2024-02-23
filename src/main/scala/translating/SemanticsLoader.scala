package translating
import Parsers.SemanticsParser.*
import com.google.protobuf.ByteString
import Parsers.*

import java.util.Base64
import scala.jdk.CollectionConverters.*
import ir.*

import scala.collection.{immutable, mutable}
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import com.grammatech.gtirb.proto.Module.ByteOrder.LittleEndian
import util.Logger

class SemanticsLoader(parserMap: immutable.Map[String, Array[Array[StmtContext]]]) {

  private val constMap = mutable.Map[String, IRType]()
  private val varMap = mutable.Map[String, IRType]()
  private var instructionCount = 0
  private var blockCount = 0

  val opcodeSize = 4

  def visitBlock(blockUUID: ByteString, blockCountIn: Int, blockAddress: Option[Int]): ArrayBuffer[Statement] = {
    blockCount = blockCountIn
    instructionCount = 0
    val instructions = parserMap(Base64.getEncoder.encodeToString(blockUUID.toByteArray))

    val statements: ArrayBuffer[Statement] = ArrayBuffer()

    for (instruction <- instructions) {
      constMap.clear
      varMap.clear

      for ((s, i) <- instruction.zipWithIndex) {
        
        val label = blockAddress.map {(a: Int) => 
          val instructionAddress = a + (opcodeSize * instructionCount)
          instructionAddress.toString + "$" + i
        }
        
        val statement = visitStmt(s, label)
        if (statement.isDefined) {
          statements.append(statement.get)
        }
      }
      instructionCount += 1
    }
    statements
  }

  private def visitStmt(ctx: StmtContext, label: Option[String] = None): Option[Statement] = {
    ctx match {
      case a: AssignContext => visitAssign(a, label)
      case c: ConstDeclContext => visitConstDecl(c, label)
      case v: VarDeclContext => visitVarDecl(v, label)
      case v: VarDeclsNoInitContext =>
        visitVarDeclsNoInit(v)
        None
      case a: AssertContext => visitAssert(a, label)
      case t: TCallContext => visitTCall(t, label)
      case i: IfContext => visitIf(i, label)
      case t: ThrowContext => Some(visitThrow(t, label))
    }
  }

  private def visitAssert(ctx: AssertContext, label: Option[String] = None): Option[Assert] = {
    val expr = visitExpr(ctx.expr)
    if (expr.isDefined) {
      Some(Assert(expr.get, None, label))
    } else {
      None
    }
  }

  private def visitThrow(ctx: ThrowContext, label: Option[String] = None): Assert = {
    val message = ctx.ID().asScala.map(_.getText).mkString(" ,")
    Assert(FalseLiteral, Some(message), label)
  }

  private def visitTCall(ctx: TCallContext, label: Option[String] = None): Option[Statement] = {
    val function = ctx.ID.getText

    val typeArgs = Option(ctx.tes) match {
      case Some(e) => e.expr().asScala
      case None => Nil
    }
    val args = Option(ctx.args) match {
      case Some(e) => e.expr().asScala
      case None => Nil
    }

    function match {
      case "Mem.set.0" =>
        checkArgs(function, 1, 4, typeArgs.size, args.size, ctx.getText)
        val mem = SharedMemory("mem", 64, 8) // yanked from BAP
        val size = parseInt(typeArgs.head) * 8
        val index = visitExpr(args.head)
        val value = visitExpr(args(3))
        val otherSize = parseInt(args(1)) * 8
        val accessType = parseInt(args(2)) // AccType enum in ASLi
        if (size != otherSize) {
          throw Exception(s"inconsistent size parameters in Mem.set.0: ${ctx.getText}")
        }
        // we expect this to be 0 'AccType_NORMAL' but if we encounter other values they may require further investigation
        if (accessType != 0) {
          Logger.info(s"Mem.set.0 with non-0 access type encountered: ${ctx.getText}")
        }

        // LittleEndian is an assumption
        if (index.isDefined && value.isDefined) {
          Some(MemoryAssign(mem, index.get, value.get, Endian.LittleEndian, size, label))
        } else {
          None
        }

      case _ =>
        Logger.debug(s"Unidentified function call $function: ${ctx.getText}")
        None
    }
  }

  private def checkArgs(name: String, typeArgsExpected: Int, argsExpected: Int, typeArgsCount: Int, argsCount: Int, token: String): Unit = {
    if (typeArgsExpected != typeArgsCount || argsExpected != argsCount) {
      throw Exception(s"Unexpected argument counts for $name - expected $typeArgsExpected type argument(s) and $argsExpected argument(s), got $typeArgsCount type arguments and $argsCount arguments: $token")
    }
  }

  private def parseInt(ctx: ExprContext) = ctx match {
    case e: ExprLitIntContext => e.value.getText.toInt
    case _ => throw Exception(s"expected ${ctx.getText} to be an integer literal")
  }

  private def visitIf(ctx: IfContext, label: Option[String] = None): Option[TempIf] = {
    val condition = visitExpr(ctx.cond)
    val thenStmts = ctx.stmt().asScala.flatMap(visitStmt(_, label))

    val elseStmts = Option(ctx.elseStmt) match {
      case Some(_) => ctx.elseStmt.stmt().asScala.flatMap(visitStmt(_, label))
      case None => mutable.Buffer()
    }

    if (condition.isDefined) {
      Some(TempIf(condition.get, thenStmts, elseStmts, label))
    } else {
      None
    }
  }

  private def visitVarDeclsNoInit(ctx: VarDeclsNoInitContext): Unit = {
    val ty = visitType(ctx.`type`())
    ctx.lvars.ID().asScala.foreach(lvar => varMap += (lvar.getText -> ty))
  }

  private def visitVarDecl(ctx: VarDeclContext, label: Option[String] = None): Option[Assign] = {
    val ty = visitType(ctx.`type`())
    val name = ctx.lvar.getText
    varMap += (name -> ty)

    val expr = visitExpr(ctx.expr())
    if (expr.isDefined) {
      Some(Assign(LocalVar(name, ty), expr.get, label))
    } else {
      None
    }
  }

  private def visitAssign(ctx: AssignContext, label: Option[String] = None): Option[Assign] = {
    val lhs = visitLexpr(ctx.lexpr)
    val rhs = visitExpr(ctx.expr)
    if (lhs.isDefined && rhs.isDefined) {
      Some(Assign(lhs.get, rhs.get, label))
    } else {
      None
    }
  }

  private def visitConstDecl(ctx: ConstDeclContext, label: Option[String] = None): Option[Assign] = {
    val ty = visitType(ctx.`type`())
    val name = ctx.lvar.getText
    constMap += (name -> ty)
    val expr = visitExpr(ctx.expr)
    if (expr.isDefined) {
      Some(Assign(LocalVar(name + "$" + blockCount + "$" + instructionCount, ty), expr.get, label))
    } else {
      None
    }
  }

  private def visitType(ctx: TypeContext): IRType = {
    ctx match
      case e: TypeBitsContext => BitVecType(parseInt(e.size))
      case r: TypeRegisterContext =>
        // this is a special register - not the same as a register in the IR
        // ignoring the register's fields for now
        BitVecType(r.size.getText.toInt)
      case c: TypeConstructorContext => c.str.getText.match {
        case "FPRounding" => BitVecType(2)
        case _ => throw Exception(s"unknown type ${ctx.getText}")
      }
      case _ => throw Exception(s"unknown type ${ctx.getText}")
  }

  private def visitExpr(ctx: ExprContext): Option[Expr] = {
    ctx match {
      case e: ExprVarContext => visitExprVar(e)
      case e: ExprTApplyContext => visitExprTApply(e)
      case e: ExprSlicesContext => visitExprSlices(e)
      case e: ExprFieldContext => Some(visitExprField(e))
      case e: ExprArrayContext => Some(visitExprArray(e))
      case e: ExprLitIntContext => None // we should not encounter this unless expected TODO replace with exception
      case e: ExprLitBitsContext => Some(visitExprLitBits(e))
    }
  }

  private  def visitExprVar(ctx: ExprVarContext): Option[Expr] = {
    val name = ctx.ID.getText
    name match {
      case n if constMap.contains(n) => Some(LocalVar(n + "$" + blockCount + "$" + instructionCount, constMap(n)))
      case v if varMap.contains(v) => Some(LocalVar(v, varMap(v)))
      case "SP_EL0" => Some(Register("R31", 64))
      case "_PC" => Some(Register("_PC", 64))
      case "TRUE" => Some(TrueLiteral)
      case "FALSE" => Some(FalseLiteral)
      case "FPCR" => Some(Register("FPCR", 32))
      // ignore the following
      case "__BranchTaken" => None
      case "BTypeNext" => None
      case "BTypeCompatible" => None
      case _ => throw Exception(s"could not identify variable '$name'")
    }
  }

  private def visitExprTApply(ctx: ExprTApplyContext): Option[Expr] = {

    val function = ctx.ID.getText

    val typeArgs: mutable.Buffer[ExprContext] = Option(ctx.tes) match {
      case Some(e) => e.expr().asScala
      case None => mutable.Buffer()
    }
    val args: mutable.Buffer[ExprContext] = Option(ctx.args) match {
      case Some(e) => e.expr().asScala
      case None => mutable.Buffer()
    }

    function match {
      case "Mem.read.0" =>
        checkArgs(function, 1, 3, typeArgs.size, args.size, ctx.getText)
        val mem = SharedMemory("mem", 64, 8)
        val index = visitExpr(args.head)
        val size = parseInt(typeArgs.head) * 8

        val otherSize = parseInt(args(1)) * 8
        val accessType = parseInt(args(2)) // AccType enum in ASLi
        if (size != otherSize) {
          throw Exception(s"inconsistent size parameters in Mem.read.0: ${ctx.getText}")
        }
        // we expect this to be 0 'AccType_NORMAL' but if we encounter other values they may require further investigation
        if (accessType != 0) {
          Logger.info(s"Mem.set.0 with non-0 access type encountered: ${ctx.getText}")
        }


        if (index.isDefined) {
          // LittleEndian is assumed
          Some(MemoryLoad(mem, index.get, Endian.LittleEndian, size))
        } else {
          None
        }

      case "cvt_bool_bv.0" =>
        checkArgs(function, 0, 1, typeArgs.size, args.size, ctx.getText)
        val expr = visitExpr(args.head)
        if (expr.isDefined) {
          val e = expr.get
          e match {
            case b: BinaryExpr if b.op == BVEQ => Some(BinaryExpr(BVCOMP, b.arg1, b.arg2))
            case _ => throw Exception(s"unhandled conversion from bool to bitvector: ${ctx.getText}")
          }
        } else {
          None
        }

      case "not_bool.0" => resolveUnaryOp(BoolNOT, function, 0, typeArgs, args, ctx.getText)
      case "eq_enum.0" => resolveBinaryOp(BoolEQ, function, 0, typeArgs, args, ctx.getText)
      case "or_bool.0" => resolveBinaryOp(BoolOR, function, 0, typeArgs, args, ctx.getText)
      case "and_bool.0" => resolveBinaryOp(BoolAND, function, 0, typeArgs, args, ctx.getText)

      case "not_bits.0" => resolveUnaryOp(BVNOT, function, 1, typeArgs, args, ctx.getText)
      case "or_bits.0" => resolveBinaryOp(BVOR, function, 1, typeArgs, args, ctx.getText)
      case "and_bits.0" => resolveBinaryOp(BVAND, function, 1, typeArgs, args, ctx.getText)
      case "eor_bits.0" => resolveBinaryOp(BVXOR, function, 1, typeArgs, args, ctx.getText)
      case "eq_bits.0" => resolveBinaryOp(BVEQ, function, 1, typeArgs, args, ctx.getText)
      case "add_bits.0" => resolveBinaryOp(BVADD, function, 1, typeArgs, args, ctx.getText)
      case "sub_bits.0" => resolveBinaryOp(BVSUB, function, 1, typeArgs, args, ctx.getText)
      case "mul_bits.0" => resolveBinaryOp(BVMUL, function, 1, typeArgs, args, ctx.getText)
      case "sdiv_bits.0" => resolveBinaryOp(BVSDIV, function, 1, typeArgs, args, ctx.getText)

      // have not yet encountered these two so need to be careful
      case "slt_bits.0" => resolveBinaryOp(BVSLT, function, 1, typeArgs, args, ctx.getText)
      case "sle_bits.0" => resolveBinaryOp(BVSLE, function, 1, typeArgs, args, ctx.getText)

      case "lsl_bits.0" => resolveBitShiftOp(BVSHL, function, typeArgs, args, ctx.getText)
      case "lsr_bits.0" => resolveBitShiftOp(BVLSHR, function, typeArgs, args, ctx.getText)
      case "asr_bits.0" => resolveBitShiftOp(BVASHR, function, typeArgs, args, ctx.getText)

      case "append_bits.0" =>
        resolveBinaryOp(BVCONCAT, function, 2, typeArgs, args, ctx.getText)

      case "ZeroExtend.0" =>
        checkArgs(function, 2, 2, typeArgs.size, args.size, ctx.getText)
        val oldSize = parseInt(typeArgs(0))
        val newSize = parseInt(typeArgs(1))
        val arg0 = visitExpr(args(0))
        val arg1 = parseInt(args(1))
        if (arg1 != newSize) {
          Exception(s"inconsistent size parameters in ZeroExtend.0: ${ctx.getText}")
        }
        if (arg0.isDefined) {
          Some(ZeroExtend(newSize - oldSize, arg0.get))
        } else {
          None
        }

      case "SignExtend.0" =>
        checkArgs(function, 2, 2, typeArgs.size, args.size, ctx.getText)
        val oldSize = parseInt(typeArgs(0))
        val newSize = parseInt(typeArgs(1))
        val arg0 = visitExpr(args(0))
        val arg1 = parseInt(args(1))
        if (arg1 != newSize) {
          Exception(s"inconsistent size parameters in SignExtend.0: ${ctx.getText}")
        }
        if (arg0.isDefined) {
          Some(SignExtend(newSize - oldSize, arg0.get))
        } else {
          None
        }

      case _ =>
        Logger.debug(s"unidentified call to $function: ${ctx.getText}")
        None
    }

  }

  private def resolveBinaryOp(operator: BinOp,
                              function: String,
                              typeArgsExpected: Int,
                              typeArgs: mutable.Buffer[ExprContext],
                              args: mutable.Buffer[ExprContext],
                              token: String
                             ): Option[BinaryExpr] = {
    checkArgs(function, typeArgsExpected, 2, typeArgs.size, args.size, token)
    // we don't currently check the size for BV ops which is the type arg
    val arg0 = visitExpr(args(0))
    val arg1 = visitExpr(args(1))
    if (arg0.isDefined && arg1.isDefined) {
      Some(BinaryExpr(operator, arg0.get, arg1.get))
    } else {
      None
    }
  }

  private def resolveUnaryOp(operator: UnOp,
                             function: String,
                             typeArgsExpected: Int,
                             typeArgs: mutable.Buffer[ExprContext],
                             args: mutable.Buffer[ExprContext],
                             token: String
                            ): Option[UnaryExpr] = {
    checkArgs(function, typeArgsExpected, 1, typeArgs.size, args.size, token)
    // we don't currently check the size for BV ops which is the type arg
    val arg = visitExpr(args.head)
    if (arg.isDefined) {
      Some(UnaryExpr(operator, arg.get))
    } else {
      None
    }
  }

  private def resolveBitShiftOp(operator: BinOp,
                                function: String,
                                typeArgs: mutable.Buffer[ExprContext],
                                args: mutable.Buffer[ExprContext],
                                token: String
                               ): Option[BinaryExpr] = {
    checkArgs(function, 2, 2, typeArgs.size, args.size, token)
    val size0 = parseInt(typeArgs(0))
    val size1 = parseInt(typeArgs(1))
    val arg0 = visitExpr(args(0))
    val arg1 = visitExpr(args(1))
    if (arg0.isDefined && arg1.isDefined) {
      if (size0 == size1) {
        Some(BinaryExpr(operator, arg0.get, arg1.get))
      } else {
        Some(BinaryExpr(operator, arg0.get, ZeroExtend(size0 - size1, arg1.get)))
      }
    } else {
      None
    }
  }

  private def visitExprSlices(ctx: ExprSlicesContext): Option[Extract] = {
    val slices = ctx.slices.slice().asScala
    if (slices.size != 1) {
      // need to determine the semantics for this case
      throw Exception(s"currently unable to handle Expr_Slices that contains more than one slice: ${ctx.getText}")
    }
    val (hi, lo) = visitSliceContext(slices.head)
    val expr = visitExpr(ctx.expr)
    if (expr.isDefined) {
      Some(Extract(hi, lo, expr.get))
    } else {
      None
    }
  }

  private def visitSliceContext(ctx: SliceContext): (Int, Int) = {
    ctx match {
      case s: Slice_HiLoContext =>
        val hi = parseInt(s.hi)
        val lo = parseInt(s.lo)
        (hi + 1, lo)
      case s: Slice_LoWdContext =>
        val lo = parseInt(s.lo)
        val wd = parseInt(s.wd)
        (lo + wd, lo)
    }
  }

  private  def visitExprField(ctx: ExprFieldContext): Register = {
    val name = ctx.expr match {
      case e: ExprVarContext => e.ID.getText
      case _ => throw Exception(s"expected ${ctx.getText} to have an Expr_Var as first parameter")
    }
    val field = ctx.field.getText

    resolveFieldExpr(name, field)
  }

  private  def visitExprArray(ctx: ExprArrayContext): Register = {
    val name = ctx.array match {
      case e: ExprVarContext => e.ID.getText
      case _ => throw Exception(s"expected ${ctx.getText} to have an Expr_Var as first parameter")
    }
    val index = parseInt(ctx.index)

    resolveArrayExpr(name, index)
  }

  private def visitExprLitBits(ctx: ExprLitBitsContext): BitVecLiteral = {
    var num = BigInt(ctx.value.getText, 2)
    val len = ctx.value.getText.length
    if (num < 0) {
      num = num + (BigInt(1) << len)
    }
    BitVecLiteral(num, len)
  }

  private def visitLexpr(ctx: LexprContext): Option[Variable] = {
    ctx match {
      case l: LExprVarContext => visitLExprVar(l)
      case l: LExprFieldContext => Some(visitLExprField(l))
      case l: LExprArrayContext => Some(visitLExprArray(l))
    }
  }

  private def visitLExprVar(ctx: LExprVarContext): Option[Variable] = {
    val name = ctx.ID.getText
    name match {
      case n if constMap.contains(n) => Some(LocalVar(n + "$" + blockCount + "$" + instructionCount, constMap(n)))
      case v if varMap.contains(v) => Some(LocalVar(v, varMap(v)))
      case "SP_EL0" => Some(Register("R31", 64))
      case "_PC" => Some(Register("_PC", 64))
      // ignore the following
      case "TRUE" => throw Exception(s"Boolean literal $name in LExpr ${ctx.getText}")
      case "FALSE" => throw Exception(s"Boolean literal $name in LExpr ${ctx.getText}")
      case "__BranchTaken" => None
      case "BTypeNext" => None
      case "BTypeCompatible" => None
      case _ => throw Exception(s"could not identify variable '$name' in ${ctx.getText}")
    }
  }

  private def visitLExprField(ctx: LExprFieldContext): Register = {
    val name = ctx.lexpr match {
      case l: LExprVarContext => l.ID.getText
      case _ => throw Exception(s"expected ${ctx.getText} to have an LExpr_Var as first parameter")
    }
    val field = ctx.field.getText

    resolveFieldExpr(name, field)
  }

  private def visitLExprArray(ctx: LExprArrayContext): Register = {
    val name = ctx.lexpr match {
      case l: LExprVarContext => l.ID.getText
      case _ => throw Exception(s"expected ${ctx.getText} to have an LExpr_Var as first parameter")
    }
    val index = parseInt(ctx.index)

    resolveArrayExpr(name, index)
  }

  private def resolveFieldExpr(name: String, field: String): Register = {
    name match {
      case "PSTATE" if field == "V" || field == "C" || field == "Z" || field == "N" =>
          Register(field + "F", 1)
      case _ => throw Exception(s"unidentified Expr_Field ($name, $field)")
    }
  }

  private def resolveArrayExpr(name: String, index: Int): Register = {
    name match {
      case "_R" => Register(s"R$index", 64)
      case "_Z" => Register(s"V$index", 128)
      case _ => throw Exception(s"unidentified Expr_Array ($name, $index)")
    }
  }
}