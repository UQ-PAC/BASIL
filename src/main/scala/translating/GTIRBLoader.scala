package translating
import Parsers.ASLpParser.*
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

class GTIRBLoader(parserMap: immutable.Map[String, Array[Array[StmtContext]]]) {

  private val constMap = mutable.Map[String, IRType]()
  private val varMap = mutable.Map[String, IRType]()
  private var instructionCount = 0
  private var blockCount = 0
  private var loadCounter = 0

  private val opcodeSize = 4

  def visitBlock(blockUUID: ByteString, blockCountIn: Int, blockAddress: Option[BigInt]): ArrayBuffer[Statement] = {
    blockCount = blockCountIn
    instructionCount = 0
    val instructions = parserMap(Base64.getEncoder.encodeToString(blockUUID.toByteArray))

    val statements: ArrayBuffer[Statement] = ArrayBuffer()

    for (instruction <- instructions) {
      constMap.clear
      varMap.clear

      for ((s, i) <- instruction.zipWithIndex) {
        
        val label = blockAddress.map {(a: BigInt) =>
          val instructionAddress = a + (opcodeSize * instructionCount)
          instructionAddress.toString + "$" + i
        }

        statements.appendAll(visitStmt(s, label))
      }
      instructionCount += 1
    }
    statements
  }

  private def visitStmt(ctx: StmtContext, label: Option[String] = None): Seq[Statement] = {
    ctx match {
      case a: AssignContext => visitAssign(a, label)
      case c: ConstDeclContext => visitConstDecl(c, label)
      case v: VarDeclContext => visitVarDecl(v, label)
      case v: VarDeclsNoInitContext =>
        visitVarDeclsNoInit(v)
        Seq()
      case a: AssertContext => visitAssert(a, label).toSeq
      case t: TCallContext => visitTCall(t, label).toSeq
      case i: IfContext => visitIf(i, label).toSeq
      case t: ThrowContext => Seq(visitThrow(t, label))
    }
  }

  private def visitAssert(ctx: AssertContext, label: Option[String] = None): Option[Statement] = {
    val expr = visitExprOnly(ctx.expr)
    if (expr.isDefined) {
      Some(Assert(expr.get, None, label))
    } else {
      None
    }
  }

  private def visitThrow(ctx: ThrowContext, label: Option[String] = None): Assert = {
    val message = ctx.ident().asScala.map(visitIdent).mkString(" ,")
    Assert(FalseLiteral, Some(message), label)
  }

  private def visitTCall(ctx: TCallContext, label: Option[String] = None): Option[Statement] = {
    val function = visitIdent(ctx.ident)

    val typeArgs = Option(ctx.tes).toList.flatMap(_.expr.asScala)
    val args = Option(ctx.args).toList.flatMap(_.expr.asScala)

    function match {
      case "Mem.set.0" =>
        checkArgs(function, 1, 4, typeArgs.size, args.size, ctx.getText)
        val mem = SharedMemory("mem", 64, 8) // yanked from BAP
        val size = parseInt(typeArgs.head) * 8
        val index = visitExprOnly(args.head)
        val value = visitExprOnly(args(3))
        val otherSize = parseInt(args(1)) * 8
        val accessType = parseInt(args(2)) // AccType enum in ASLi, not very relevant to us
        if (size != otherSize) {
          throw Exception(s"inconsistent size parameters in Mem.set.0: ${ctx.getText}")
        }

        // LittleEndian is an assumption
        if (index.isDefined && value.isDefined) {
          Some(MemoryStore(mem, index.get, value.get, Endian.LittleEndian, size.toInt, label))
        } else {
          None
        }
      case "unsupported_opcode.0" => {
        val op = args.headOption.flatMap(visitExprOnly) match {
          case Some(IntLiteral(s)) => Some("%08x".format(s))
          case c => c.map(_.toString)
        }
        val comment = " unsupported opcode" + op.map(": " + _).getOrElse("")
        Some(Assert(FalseLiteral, Some(comment)))
      }
      case _ =>
        Logger.error(s"Unidentified function call $function: ${ctx.getText}")
        Some(Assert(FalseLiteral, Some( " unsupported: " + ctx.getText)))
    }
  }

  private def checkArgs(name: String, typeArgsExpected: Int, argsExpected: Int, typeArgsCount: Int, argsCount: Int, token: String): Unit = {
    if (typeArgsExpected != typeArgsCount || argsExpected != argsCount) {
      throw Exception(s"Unexpected argument counts for $name - expected $typeArgsExpected type argument(s) and $argsExpected argument(s), got $typeArgsCount type arguments and $argsCount arguments: $token")
    }
  }

  private def parseInt(ctx: ExprContext) = ctx match {
    case e: ExprLitIntContext => visitInteger(e.integer)
    case _ => throw Exception(s"expected ${ctx.getText} to be an integer literal")
  }

  private def visitIf(ctx: IfContext, label: Option[String] = None): Option[TempIf] = {
    val condition = visitExprOnly(ctx.cond)
    val thenStmts = ctx.thenStmts.stmt.asScala.flatMap(visitStmt(_, label))

    val elseStmts = Option(ctx.elseStmts) match {
      case Some(_) => ctx.elseStmts.stmt.asScala.flatMap(visitStmt(_, label))
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
    val newVars = ctx.lvars.ident.asScala.map(visitIdent(_) -> ty)
    varMap ++= newVars
  }

  private def visitVarDecl(ctx: VarDeclContext, label: Option[String] = None): Seq[Statement] = {
    val ty = visitType(ctx.`type`())
    val name = visitIdent(ctx.lvar)
    varMap += (name -> ty)

    val (expr, load) = visitExpr(ctx.expr)
    if (expr.isDefined) {
      if (load.isDefined) {
        val loadWithLabel = MemoryLoad(load.get.lhs, load.get.mem, load.get.index, load.get.endian, load.get.size, label.map(_ + "$0"))
        val assign = LocalAssign(LocalVar(name, ty), expr.get, label.map(_ + "$1"))
        Seq(loadWithLabel, assign)
      } else {
        val assign = LocalAssign(LocalVar(name, ty), expr.get, label)
        Seq(assign)
      }
    } else {
      Seq()
    }
  }

  private def visitAssign(ctx: AssignContext, label: Option[String] = None): Seq[Statement] = {
    val lhs = visitLexpr(ctx.lexpr)
    val (rhs, load) = visitExpr(ctx.expr)
    if (lhs.isDefined && rhs.isDefined) {
      if (load.isDefined) {
        val loadWithLabel = MemoryLoad(load.get.lhs, load.get.mem, load.get.index, load.get.endian, load.get.size, label.map(_ + "$0"))
        val assign = LocalAssign(lhs.get, rhs.get, label.map(_ + "$1"))
        Seq(loadWithLabel, assign)
      } else {
        val assign = LocalAssign(lhs.get, rhs.get, label)
        Seq(assign)
      }
    } else {
      Seq()
    }
  }

  private def visitConstDecl(ctx: ConstDeclContext, label: Option[String] = None): Seq[Statement] = {
    val ty = visitType(ctx.`type`())
    val name = visitIdent(ctx.lvar)
    constMap += (name -> ty)
    val (expr, load) = visitExpr(ctx.expr)
    if (expr.isDefined) {
      if (load.isDefined) {
        val loadWithLabel = MemoryLoad(load.get.lhs, load.get.mem, load.get.index, load.get.endian, load.get.size, label.map(_ + "$0"))
        val assign = LocalAssign(LocalVar(name + "$" + blockCount + "$" + instructionCount, ty), expr.get, label.map(_ + "$1"))
        Seq(loadWithLabel, assign)
      } else {
        val assign = LocalAssign(LocalVar(name + "$" + blockCount + "$" + instructionCount, ty), expr.get, label)
        Seq(assign)
      }
    } else {
      Seq()
    }
  }

  private def visitType(ctx: TypeContext): IRType = {
    ctx match {
      case e: TypeBitsContext => BitVecType(parseInt(e.size).toInt)
      case r: TypeRegisterContext =>
        // this is a special register - not the same as a register in the IR
        // ignoring the register's fields for now
        BitVecType(visitInteger(r.size).toInt)
      case c: TypeConstructorContext => visitIdent(c.str).match {
        case "FPRounding" => BitVecType(3)
        case "integer" => BitVecType(64)
        case _ => throw Exception(s"unknown type ${ctx.getText}")
      }
      case _ => throw Exception(s"unknown type ${ctx.getText}")
    }
  }

  private def visitExpr(ctx: ExprContext): (Option[Expr], Option[MemoryLoad]) = {
    ctx match {
      case e: ExprVarContext => (visitExprVar(e), None)
      case e: ExprTApplyContext => visitExprTApply(e)
      case e: ExprSlicesContext => visitExprSlices(e)
      case e: ExprFieldContext => (Some(visitExprField(e)), None)
      case e: ExprArrayContext => (Some(visitExprArray(e)), None)
      case e: ExprLitIntContext => (Some(IntLiteral(parseInt(e))), None)
      case e: ExprLitBitsContext => (Some(visitExprLitBits(e)), None)
    }
  }

  private def visitExprOnly(ctx: ExprContext): Option[Expr] = {
    val (expr, load) = visitExpr(ctx)
    if (load.isDefined) {
      throw Exception(s"found load $expr $load")
    } else {
      expr
    }
  }

  private def visitExprVar(ctx: ExprVarContext): Option[Expr] = {
    val name = visitIdent(ctx.ident)
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
      case "TPIDR_EL0" => Some(Register(name, 64))
      // case ov => Some(LocalVar(ov, BitVecType(1)))
      case _ => throw Exception(s"could not identify variable '$name'")
    }
  }

  private def visitExprTApply(ctx: ExprTApplyContext): (Option[Expr], Option[MemoryLoad]) = {
    val function = visitIdent(ctx.ident)

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
        val index = visitExprOnly(args.head) // can't have load inside load
        val size = parseInt(typeArgs.head) * 8

        val otherSize = parseInt(args(1)) * 8
        val accessType = parseInt(args(2)) // AccType enum in ASLi - not too relevant
        if (size != otherSize) {
          throw Exception(s"inconsistent size parameters in Mem.read.0: ${ctx.getText}")
        }

        val temp = LocalVar("load" + loadCounter, BitVecType(size.toInt))
        loadCounter += 1

        if (index.isDefined) {
          // LittleEndian is assumed
          (Some(temp), Some(MemoryLoad(temp, mem, index.get, Endian.LittleEndian, size.toInt, None)))
        } else {
          (None, None)
        }

      case "cvt_bool_bv.0" =>
        checkArgs(function, 0, 1, typeArgs.size, args.size, ctx.getText)
        val (expr, load) = visitExpr(args.head)
        val result : Option[Expr] = expr.map {
            case b: BinaryExpr if b.op == BVEQ => BinaryExpr(BVCOMP, b.arg1, b.arg2)
            case FalseLiteral   => BitVecLiteral(0, 1)
            case TrueLiteral => BitVecLiteral(1, 1)
            case o => UnaryExpr(BoolToBV1, o)
          }
        (result, load)

      case "not_bool.0" => (resolveUnaryOp(BoolNOT, function, 0, typeArgs, args, ctx.getText), None)
      case "eq_enum.0" => (resolveBinaryOp(BoolEQ, function, 0, typeArgs, args, ctx.getText))
      case "or_bool.0" => (resolveBinaryOp(BoolOR, function, 0, typeArgs, args, ctx.getText))
      case "and_bool.0" => (resolveBinaryOp(BoolAND, function, 0, typeArgs, args, ctx.getText))

      case "or_bits.0" => (resolveBinaryOp(BVOR, function, 1, typeArgs, args, ctx.getText))
      case "and_bits.0" => (resolveBinaryOp(BVAND, function, 1, typeArgs, args, ctx.getText))
      case "eor_bits.0" => (resolveBinaryOp(BVXOR, function, 1, typeArgs, args, ctx.getText))
      case "eq_bits.0" => (resolveBinaryOp(BVEQ, function, 1, typeArgs, args, ctx.getText))
      case "add_bits.0" => (resolveBinaryOp(BVADD, function, 1, typeArgs, args, ctx.getText))
      case "sub_bits.0" => (resolveBinaryOp(BVSUB, function, 1, typeArgs, args, ctx.getText))
      case "mul_bits.0" => (resolveBinaryOp(BVMUL, function, 1, typeArgs, args, ctx.getText))
      case "sdiv_bits.0" => (resolveBinaryOp(BVSDIV, function, 1, typeArgs, args, ctx.getText))
      case "slt_bits.0" => (resolveBinaryOp(BVSLT, function, 1, typeArgs, args, ctx.getText))
      case "sle_bits.0" => (resolveBinaryOp(BVSLE, function, 1, typeArgs, args, ctx.getText))

      case "not_bits.0" => (resolveUnaryOp(BVNOT, function, 1, typeArgs, args, ctx.getText), None)

      case "lsl_bits.0" => (resolveBitShiftOp(BVSHL, function, typeArgs, args, ctx.getText), None)
      case "lsr_bits.0" => (resolveBitShiftOp(BVLSHR, function, typeArgs, args, ctx.getText), None)
      case "asr_bits.0" => (resolveBitShiftOp(BVASHR, function, typeArgs, args, ctx.getText), None)

      case "append_bits.0" =>
        (resolveBinaryOp(BVCONCAT, function, 2, typeArgs, args, ctx.getText))

      case "replicate_bits.0" =>
        checkArgs(function, 2, 2, typeArgs.size, args.size, ctx.getText)
        val oldSize = parseInt(typeArgs(0))
        val replications = parseInt(typeArgs(1)).toInt
        // memory loads shouldn't appear here?
        val arg0 = visitExprOnly(args(0))
        val arg1 = parseInt(args(1))
        val newSize = oldSize * replications
        if (arg1 != replications) {
          Exception(s"inconsistent size parameters in replicate_bits.0: ${ctx.getText}")
        }
        if (arg0.isDefined) {
          (Some(Repeat(replications, arg0.get)), None)
        } else {
          (None, None)
        }

      case "ZeroExtend.0" =>
        checkArgs(function, 2, 2, typeArgs.size, args.size, ctx.getText)
        val oldSize = parseInt(typeArgs(0))
        val newSize = parseInt(typeArgs(1))
        val (arg0, load) = visitExpr(args(0))
        val arg1 = parseInt(args(1))
        if (arg1 != newSize) {
          Exception(s"inconsistent size parameters in ZeroExtend.0: ${ctx.getText}")
        }
        if (arg0.isDefined) {
          (Some(ZeroExtend((newSize - oldSize).toInt, arg0.get)), load)
        } else {
          (None, None)
        }

      case "SignExtend.0" =>
        checkArgs(function, 2, 2, typeArgs.size, args.size, ctx.getText)
        val oldSize = parseInt(typeArgs(0))
        val newSize = parseInt(typeArgs(1))
        val (arg0, load) = visitExpr(args(0))
        val arg1 = parseInt(args(1))
        if (arg1 != newSize) {
          Exception(s"inconsistent size parameters in SignExtend.0: ${ctx.getText}")
        }
        if (arg0.isDefined) {
          (Some(SignExtend((newSize - oldSize).toInt, arg0.get)), load)
        } else {
          (None, None)
        }

      case "FPCompareGT.0" | "FPCompareGE.0" | "FPCompareEQ.0" =>
        checkArgs(function, 1, 3, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val size = parseInt(typeArgs(0))
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + size, argsIR, BoolType)), None)

      case "FPAdd.0" | "FPMul.0" | "FPDiv.0" | "FPMulX.0" | "FPMax.0" | "FPMin.0" | "FPMaxNum.0" | "FPMinNum.0" | "FPSub.0" =>
        checkArgs(function, 1, 3, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val size = parseInt(typeArgs(0)).toInt
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + size, argsIR, BitVecType(size))), None)

      case "FPMulAddH.0" | "FPMulAdd.0" |
           "FPRoundInt.0" |
           "FPRoundIntN.0" =>
        checkArgs(function, 1, 4, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val size = parseInt(typeArgs(0)).toInt
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + size, argsIR, BitVecType(size))), None)

      case "FPRecpX.0" | "FPSqrt.0" | "FPRecipEstimate.0" |
           "FPRSqrtStepFused.0" | "FPRecipStepFused.0" =>
        checkArgs(function, 1, 2, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val size = parseInt(typeArgs(0)).toInt
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + size, argsIR, BitVecType(size))), None)

      case "FPCompare.0" =>
        checkArgs(function, 1, 4, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val size = parseInt(typeArgs(0))
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + size, argsIR, BitVecType(4))), None)

      case "FPConvert.0" =>
        checkArgs(function, 2, 3, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val outSize = parseInt(typeArgs(0)).toInt
        val inSize = parseInt(typeArgs(1))
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + outSize + "$" + inSize, argsIR, BitVecType(outSize))), None)

      case "FPToFixed.0" =>
        checkArgs(function, 2, 5, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val outSize = parseInt(typeArgs(0)).toInt
        val inSize = parseInt(typeArgs(1))
        // need to specifically handle the integer parameter
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + outSize + "$" + inSize, argsIR, BitVecType(outSize))), None)

      case "FixedToFP.0" =>
        checkArgs(function, 2, 5, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val inSize = parseInt(typeArgs(0))
        val outSize = parseInt(typeArgs(1)).toInt
        // need to specifically handle the integer parameter
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + outSize + "$" + inSize, argsIR, BitVecType(outSize))), None)

      case "FPConvertBF.0" =>
        checkArgs(function, 0, 3, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name, argsIR, BitVecType(32))), None)

      case "FPToFixedJS_impl.0" =>
        checkArgs(function, 2, 3, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val inSize = parseInt(typeArgs(0))
        val outSize = parseInt(typeArgs(1)).toInt
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name + "$" + outSize + "$" + inSize, argsIR, BitVecType(outSize))), None)

      case "BFAdd.0" | "BFMul.0" =>
        checkArgs(function, 0, 2, typeArgs.size, args.size, ctx.getText)
        val name = function.stripSuffix(".0")
        val argsIR = args.flatMap(visitExprOnly).toSeq
        (Some(UninterpretedFunction(name, argsIR, BitVecType(32))), None)

      case _ =>
        // known ASLp methods not yet handled:
        // FPRoundBase, BFRound - take asl type 'real' as input, need to see this in practice and requires consideration
        // AArch64.MemTag.read, AArch64.MemTag.set - allocation tag operations, can't model as uninterpreted functions
        // and will require some research into their semantics
        // AtomicStart, AtomicEnd - can't model as uninterpreted functions, requires modelling atomic section
        Logger.debug(s"unidentified call to $function: ${ctx.getText}")
        (None, None)
    }

  }

  private def resolveBinaryOp(operator: BinOp,
                              function: String,
                              typeArgsExpected: Int,
                              typeArgs: mutable.Buffer[ExprContext],
                              args: mutable.Buffer[ExprContext],
                              token: String
                             ): (Option[BinaryExpr], Option[MemoryLoad]) = {
    checkArgs(function, typeArgsExpected, 2, typeArgs.size, args.size, token)
    // we don't currently check the size for BV ops which is the type arg
    // memory loads shouldn't appear inside binary operations?
    val (arg0, l0) = visitExpr(args(0))
    val (arg1, l1) = visitExpr(args(1))
    val l = l0.orElse(l1)
    assert(!(l0.isDefined && l1.isDefined), "Multiple loads in expression")
    if (arg0.isDefined && arg1.isDefined) {
      (Some(BinaryExpr(operator, arg0.get, arg1.get)), l)
    } else {
      (None, l)
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
    // memory loads shouldn't appear inside unary operations?
    val arg = visitExprOnly(args.head)
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
    val arg0 = visitExprOnly(args(0))
    val arg1 = visitExprOnly(args(1))
    // memory loads shouldn't appear inside bitshifts?
    if (arg0.isDefined && arg1.isDefined) {
      if (size0 == size1) {
        Some(BinaryExpr(operator, arg0.get, arg1.get))
      } else {
        Some(BinaryExpr(operator, arg0.get, ZeroExtend((size0 - size1).toInt, arg1.get)))
      }
    } else {
      None
    }
  }

  private def visitExprSlices(ctx: ExprSlicesContext): (Option[Extract], Option[MemoryLoad]) = {
    val slices = ctx.slices.slice().asScala
    if (slices.size != 1) {
      // need to determine the semantics for this case
      throw Exception(s"currently unable to handle Expr_Slices that contains more than one slice: ${ctx.getText}")
    }
    val (hi, lo) = visitSliceContext(slices.head)
    val (expr, load) = visitExpr(ctx.expr)
    if (expr.isDefined) {
      (Some(Extract(hi, lo, expr.get)), load)
    } else {
      (None, None)
    }
  }

  private def visitSliceContext(ctx: SliceContext): (Int, Int) = {
    ctx match {
      case s: Slice_HiLoContext =>
        val hi = parseInt(s.hi).toInt
        val lo = parseInt(s.lo).toInt
        (hi + 1, lo)
      case s: Slice_LoWdContext =>
        val lo = parseInt(s.lo).toInt
        val wd = parseInt(s.wd).toInt
        (lo + wd, lo)
    }
  }

  private def visitExprField(ctx: ExprFieldContext): Register = {
    val name = ctx.expr match {
      case e: ExprVarContext => visitIdent(e.ident)
      case _ => throw Exception(s"expected ${ctx.getText} to have an Expr_Var as first parameter")
    }
    val field = visitIdent(ctx.field)

    resolveFieldExpr(name, field)
  }

  private def visitExprArray(ctx: ExprArrayContext): Register = {
    val name = ctx.array match {
      case e: ExprVarContext => visitIdent(e.ident)
      case _ => throw Exception(s"expected ${ctx.getText} to have an Expr_Var as first parameter")
    }
    val index = parseInt(ctx.index).toInt

    resolveArrayExpr(name, index)
  }

  private def visitExprLitBits(ctx: ExprLitBitsContext): BitVecLiteral = {
    visitBits(ctx.bits)
  }

  private def visitLexpr(ctx: LexprContext): Option[Variable] = {
    ctx match {
      case l: LExprVarContext => visitLExprVar(l)
      case l: LExprFieldContext => Some(visitLExprField(l))
      case l: LExprArrayContext => Some(visitLExprArray(l))
    }
  }

  private def visitLExprVar(ctx: LExprVarContext): Option[Variable] = {
    val name = visitIdent(ctx.ident)
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
      case "TPIDR_EL0" => Some(Register(name, 64))
      // case ov => Some(LocalVar(ov, BitVecType(1)))
      case _ => throw Exception(s"could not identify variable '$name' in ${ctx.getText}")
    }
  }

  private def visitLExprField(ctx: LExprFieldContext): Register = {
    val name = ctx.lexpr match {
      case l: LExprVarContext => visitIdent(l.ident)
      case _ => throw Exception(s"expected ${ctx.getText} to have an LExpr_Var as first parameter")
    }
    val field = visitIdent(ctx.field)

    resolveFieldExpr(name, field)
  }

  private def visitLExprArray(ctx: LExprArrayContext): Register = {
    val name = ctx.lexpr match {
      case l: LExprVarContext => visitIdent(l.ident)
      case _ => throw Exception(s"expected ${ctx.getText} to have an LExpr_Var as first parameter")
    }
    val index = parseInt(ctx.index)

    resolveArrayExpr(name, index.toInt)
  }

  private def visitIdent(ctx: IdentContext): String = {
    ctx.ID.getText.stripPrefix("\"").stripSuffix("\"")
  }

  private def visitInteger(ctx: IntegerContext): BigInt = {
    BigInt(ctx.DEC.getText, 10)
  }

  private def visitBits(ctx: BitsContext): BitVecLiteral = {
    val str = ctx.BINARY.getText.stripPrefix("'").stripSuffix("'")
    val width = str.length
    var num = BigInt(str, 2)
    if (num < 0) {
      num = num + (BigInt(1) << width)
    }
    BitVecLiteral(num, width)
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
