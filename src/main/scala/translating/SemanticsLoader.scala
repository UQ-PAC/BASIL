package translating
import Parsers.SemanticsParser.*
import com.google.protobuf.ByteString
import Parsers.*

import java.util.Base64
import scala.jdk.CollectionConverters.*
import ir.*

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import com.grammatech.gtirb.proto.Module.ByteOrder.LittleEndian

import scala.collection.immutable

class SemanticsLoader(targetuuid: ByteString, parserMap: immutable.Map[String, Array[Array[StmtContext]]], blkCount: Int) extends SemanticsBaseVisitor[Any] {

  private var cseMap = mutable.Map[String, IRType]()
  private var varMap = mutable.Map[String, IRType]()
  private var instructionCount = 0

  def chrisifyUUID(uuid: ByteString): String = { // This probably needs a better name, but :/
    Base64.getEncoder.encodeToString(uuid.toByteArray)
  }

  def createStatements(): ArrayBuffer[Statement] = {
    visitInstructions(parserMap(chrisifyUUID(targetuuid)))
  }

  def visitInstructions(stmts: Array[Array[StmtContext]]): ArrayBuffer[Statement] = {

    val statements: Array[Statement] = stmts.flatMap { elem => 
      cseMap = cseMap.empty
      varMap = varMap.empty
      val stmts = elem.flatMap {
        s => s match {
          case a if a.assignment_stmt() != null =>
            visitAssignment_stmt(a.assignment_stmt())
          case c if c.call_stmt() != null =>
            Option(visitCall_stmt(c.call_stmt()))
          case c if c.conditional_stmt() != null =>
            Option(visitConditional_stmt(c.conditional_stmt()))
          case _ => ???
        }
      }
      instructionCount += 1
      stmts
    }

    statements.to(ArrayBuffer)
  }

  def visitAssignment_stmt(ctx: Assignment_stmtContext): Option[Statement] = {
    ctx match
      case a: AssignContext => Option(visitAssign(a))
      case c: ConstDeclContext => Option(visitConstDecl(c))
      case v: VarDeclContext => Option(visitVarDecl(v))
      case v: VarDeclsNoInitContext => visitVarDeclsNoInit(v)
      case a: AssertContext => Option(visitAssert(a))
  }

  override def visitAssert(ctx: AssertContext): Assert = {
    val expr = visitExpr(ctx.expr)
    Assert(expr)
  }

  override def visitCall_stmt(ctx: Call_stmtContext): MemoryAssign = {
    val func: String = if (ctx.METHOD() == null) ctx.SSYMBOL().getText else ctx.METHOD().getText

    func match {
      case "Mem.set.0" => 
        val mem = Memory("mem", 64, 8) // yanked from BAP
        val addr = visitExpr(ctx.expr(1))
        val value = visitExpr(ctx.expr(4))
        val size = visitExpr(ctx.expr(2)).asInstanceOf[IntLiteral].value.toInt * 8
        val memstore = MemoryStore(mem, addr, value, Endian.LittleEndian, size)
        MemoryAssign(mem, memstore)
      
      case "AtomicStart.0" => ??? 

      case "AtomicEnd.0" => ???

      case _ => ???
    }
  }

  override def visitConditional_stmt(ctx: Conditional_stmtContext): TempIf = {
    val totalStmts = ArrayBuffer.newBuilder[ArrayBuffer[Statement]]
    val conds = ArrayBuffer.newBuilder[Expr]
    
    var currentContext = ctx
    var prevContext = currentContext

    while (currentContext != null) {
      val stmts = currentContext.stmt().asScala.flatMap {s => s match {
        case a if a.assignment_stmt() != null =>
          visitAssignment_stmt(a.assignment_stmt())
        case c if c.call_stmt() != null =>
          Option(visitCall_stmt(c.call_stmt()))
        case _ => None //There Shouldn't really be any conditional statements, these are handled by parser + this while loop
      }}

      totalStmts += stmts.to(ArrayBuffer)
      conds += visitExpr(currentContext.expr())
      prevContext = currentContext
      currentContext = currentContext.conditional_stmt()
    }

    val elseBranch = prevContext.else_stmt()
    if (!elseBranch.isEmpty) {
    
      val elseStmt: ArrayBuffer[Statement] = elseBranch.asScala.flatMap { elem =>
           elem.stmt() match {
            case a if a.assignment_stmt() != null =>
            visitAssignment_stmt(a.assignment_stmt())
          case c if c.call_stmt() != null =>
            Option(visitCall_stmt(c.call_stmt()))
          case _ => None 
        }
      }.to(ArrayBuffer)
      TempIf(true, conds.result(), totalStmts.result(), elseStmt)

    } else {
      TempIf(false, conds.result(), totalStmts.result(), ArrayBuffer[Statement]())
    }

  }

  override def visitVarDeclsNoInit(ctx: VarDeclsNoInitContext): Option[LocalAssign] = {
    val ty = visitType(ctx.`type`())
    ctx.METHOD().asScala.foreach(elem => varMap += (elem.getText -> ty))
    None
  }

  override def visitVarDecl(ctx: VarDeclContext): LocalAssign = {
    val ty = visitType(ctx.`type`())
    val name = ctx.METHOD().getText

    varMap += (name -> ty)

    val expr = visitExpr(ctx.expr())
    if (expr != null) {
      LocalAssign(LocalVar(name + "_" + blkCount + instructionCount, ty), expr) 
    } else {
      null
    }
  }

  override def visitAssign(ctx: AssignContext): LocalAssign = {
    val lhs = visitLexpr(ctx.lexpr())
    val rhs = visitExpr(ctx.expr())
    if (lhs == null || rhs == null) {
      null
    } else {
      LocalAssign(lhs, rhs)
    }
  }

  override def visitConstDecl(ctx: ConstDeclContext): LocalAssign = {
    val ty = visitType(ctx.`type`())
    val name = ctx.METHOD().getText

    cseMap += (name -> ty)

    val expr = visitExpr(ctx.expr())
    if (expr != null) {
      LocalAssign(LocalVar(name.dropRight(3) + "_" + blkCount + instructionCount, ty), expr) 
    } else {
      null
    }
  }

  def visitType(ctx: TypeContext): IRType = {
    ctx match
      case e: TypeBitsContext =>
        val size = visitExpr(e.expr()).asInstanceOf[IntLiteral].value.toInt
        BitVecType(size)
      case _ => ??? // can be extended as more types added to grammar
  }

  def visitExpr(ctx: ExprContext): Expr = {
    ctx match
      case e: ExprVarContext    => visitExprVar(e)
      case e: ExprTApplyContext => visitExprTApply(e)
      case e: ExprSlicesContext => visitExprSlices(e)
      case e: ExprFieldContext  => visitExprField(e)
      case e: ExprArrayContext  => visitExprArray(e)
      case e: ExprLitIntContext => visitExprLitInt(e)
      case e: ExprLitHexContext =>
        ??? // not in current semantics, but still unsure how this ports to IR
      case e: ExprLitBitsContext   => visitExprLitBits(e)
      case e: ExprLitMaskContext   => ??? // ditto above comment
      case e: ExprLitStringContext => ??? // ditto
      case _ => ???
  }

  override def visitExprVar(ctx: ExprVarContext): Expr = {
    createExprVar(getExprVarText(ctx))
  }

  def getExprVarText(ctx: ExprVarContext): String = {
    if (ctx.SSYMBOL() != null) {
      ctx.SSYMBOL().getText
    } else {
      ctx.METHOD().getText
    }
  }

  def get_int(ctx: ExprContext): Int = ctx match {
    case e: ExprLitIntContext =>
      if (e.DEC() != null) {
        e.DEC().getText.toInt
      } else {
        e.BINARY().getText.toInt
      }
  }

  // Used to match ZeroExtend sizes to Bap
  def fix_size(expr1: Expr, expr2: Expr): Expr = {

    val size1 = expr1 match {
      case e: Extract => e.end - e.start
      case r: Register => r.size
      case b: BitVecLiteral => b.size
      case z: ZeroExtend => 
        val innerSize = z.body match { 
          case e: Extract => e.end - e.start
          case r: Register => r.size
          case b: BitVecLiteral => b.size
          case _ => ???
        } 
        innerSize + z.extension
      case _ => ???
    }

    val size2 = expr2 match {
      case e: Extract => e.end - e.start
      case r: Register => r.size
      case b: BitVecLiteral => b.size
      case z: ZeroExtend => 
        val innerSize = z.body match { 
          case e: Extract => e.end - e.start
          case r: Register => r.size
          case b: BitVecLiteral => b.size
          case _ => ???
        } 
        innerSize + z.extension
      case _ => ???
    } 

    if (size1 == size2) {
      expr2
    } else {
      ZeroExtend(size1 - size2, expr2)
    }
  }

  override def visitExprTApply(ctx: ExprTApplyContext): Expr = {
    val str = ctx.METHOD.getText.substring(0, ctx.METHOD.getText.lastIndexOf("."))
    // removes everything up to and including the last dot

    str match
      case "Mem.read" =>
        val mem = Memory("mem", 64, 8) // yanked from BAP, hope this never changes
        val addr = visitExpr(ctx.expr(0))
        val size = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt * 8 
        MemoryLoad(mem, addr, Endian.LittleEndian, size)
      case "not_bool"    => UnaryExpr(BoolNOT, visitExpr(ctx.expr(0)))
      case "cvt_bool_bv" =>
      // in ocaml, takes bool and turns to bitvector 
        val expr = visitExpr(ctx.expr(0))
        expr match {
          case b: BinaryExpr if b.op == BVEQ => BinaryExpr(BVCOMP, b.arg1, b.arg2)
          case _ => ???
        }
      case "eq_enum"  => BinaryExpr(BoolEQ, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "or_bool"  => BinaryExpr(BoolOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "and_bool" => BinaryExpr(BoolAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "replicate_bits" =>
        val value = visitExpr(ctx.expr(0)).asInstanceOf[IntLiteral].value
        // not 100% on this, since it hasn't come up yet
        val size = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
        BitVecLiteral(value, size)
      case "not_bits"    => UnaryExpr(BVNOT, visitExpr(ctx.expr(0)))
      case "or_bits"     => BinaryExpr(BVOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "and_bits"    => BinaryExpr(BVAND, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "eor_bits"    => BinaryExpr(BVXOR, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "eq_bits"     => BinaryExpr(BVEQ, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "add_bits"    => BinaryExpr(BVADD, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "sub_bits"    => BinaryExpr(BVSUB, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))
      case "mul_bits"    => BinaryExpr(BVMUL, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1)))  
      case "sdiv_bits"   => BinaryExpr(BVSDIV, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1))) 
      case "lsl_bits"    => 
        val expr1 = visitExpr(ctx.expr(0)) 
        val expr2 = fix_size(expr1, visitExpr(ctx.expr(1)))
        BinaryExpr(BVSHL, expr1, expr2)  // need to fix size here?
      case "lsr_bits"    =>
        val expr1 = visitExpr(ctx.expr(0)) 
        val expr2 = fix_size(expr1, visitExpr(ctx.expr(1)))
        BinaryExpr(BVLSHR, expr1, expr2)  
      case "asr_bits"    =>
        val expr1 = visitExpr(ctx.expr(0)) 
        val expr2 = fix_size(expr1, visitExpr(ctx.expr(1)))
        BinaryExpr(BVASHR, expr1, expr2)  
      case "slt_bits"    =>
        val expr1 = visitExpr(ctx.expr(0)) 
        val expr2 = fix_size(expr1, visitExpr(ctx.expr(1)))
        BinaryExpr(BVSLT, expr1, expr2)  
      case "sle_bits"    =>
        val expr1 = visitExpr(ctx.expr(0)) 
        val expr2 = fix_size(expr1, visitExpr(ctx.expr(1)))
        BinaryExpr(BVSLE, expr1, expr2)  
      case "append_bits" => 
        // there is no append but there is concat, so it probably does the same thing
        BinaryExpr(BVCONCAT, visitExpr(ctx.expr(0)), visitExpr(ctx.expr(1))) 
      case "ZeroExtend" =>
        val init = get_int(ctx.targs().asScala.head.expr())
        val fini = get_int(ctx.targs().asScala.last.expr())
        ZeroExtend(fini - init, visitExpr(ctx.expr(0)))

      case "SignExtend" =>
        val init = get_int(ctx.targs().asScala.head.expr())
        val fini = get_int(ctx.targs().asScala.last.expr())
        SignExtend(fini - init, visitExpr(ctx.expr(0)))

  }

  override def visitExprSlices(ctx: ExprSlicesContext): Expr = {
    val (lo, hi) = visitSlice_expr(ctx.slice_expr())
    val loInt = lo
    val hiInt = hi
    Extract(loInt + hiInt, loInt, visitExpr(ctx.expr()))
  }

  override def visitSlice_expr(ctx: Slice_exprContext): (Int, Int) = {
    val start = visitExpr(ctx.expr(0)).asInstanceOf[IntLiteral].value.toInt
    val end = visitExpr(ctx.expr(1)).asInstanceOf[IntLiteral].value.toInt
    (start, end)
  }

  override def visitExprField(ctx: ExprFieldContext): Expr = {
    var value: String = null
    ctx.expr() match
      case e: ExprVarContext => value = getExprVarText(e)
      case _: Any            => visitExpr(ctx.expr())
    val field: ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText)
    createExprVarArray(field)
  }

  override def visitExprArray(ctx: ExprArrayContext): Expr = {
    val array: ArrayBuffer[String] = ctx
      .expr()
      .asScala
      .map {
        case e: ExprVarContext => getExprVarText(e)
        case e: ExprLitIntContext =>
          if (e.DEC() != null) {
            e.DEC().getText
          } else {
            e.BINARY().getText
          }
        case e: ExprLitBitsContext => Integer.parseInt(e.BINARY().getText, 2).asInstanceOf[String]
      }
      .to(ArrayBuffer)

    createExprVarArray(array)
  }

  override def visitExprLitInt(ctx: ExprLitIntContext): Expr = {
    //I'm too scared to change the grammar at this point in time, so i'm just going to roll with stuff like this for now
    if (ctx.DEC() != null) {
      IntLiteral(ctx.DEC().getText.toInt)
    } else {
      IntLiteral(ctx.BINARY().getText.toInt)
    }

  }

  override def visitExprLitBits(ctx: ExprLitBitsContext): Expr = {
    var num = BigInt(ctx.BINARY().getText, 2) 
    val len = ctx.BINARY().getText.length()
    if (num < 0) {
      num = num + (BigInt(1) << len)
    } 
    BitVecLiteral(num, len)
  }

  def visitLexpr(ctx: LexprContext): Variable = {
    ctx match
      case l: LExprVarContext => visitLExprVar(l)
      case l: LExprFieldContext => visitLExprField(l)
      case l: LExprArrayContext => visitLExprArray(l)
      case _ => ???
  }

  override def visitLExprVar(ctx: LExprVarContext): Variable = {
    createExprVar(getLExprVarText(ctx)).asInstanceOf[Variable]
  }

  def getLExprVarText(ctx: LExprVarContext): String = {
    if (ctx.SSYMBOL() != null) {
      ctx.SSYMBOL().getText
    } else {
      ctx.METHOD().getText
    }
  }

  override def visitLExprField(ctx: LExprFieldContext): Variable = {
    var value: String = null
    ctx.lexpr() match
      case l: LExprVarContext => value = getLExprVarText(l)
      case _: Any             => visitLexpr(ctx.lexpr())
    val field: ArrayBuffer[String] = ArrayBuffer(value, ctx.SSYMBOL().getText)
    createExprVarArray(field)
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
          e.DEC().getText
        } else {
          e.BINARY().getText
        }
      case e: ExprLitBitsContext => Integer.parseInt(e.BINARY().getText, 2).asInstanceOf[String]
      case _                     => ???
    }

    createExprVarArray(array)
  }

  def createExprVar(name: String): Expr = {
    name match
      case n if cseMap.contains(n)  => LocalVar(n.dropRight(3) + "_" + blkCount + instructionCount, cseMap(n))
      case v if varMap.contains(v)  => LocalVar(v + "_" + blkCount + instructionCount, varMap(v))
      case "TRUE"                   => TrueLiteral
      case "FALSE"                  => FalseLiteral
      case "SP_EL0"                 => Register("R31", BitVecType(64))
      case "_PC" =>
        Register(
          "_PC",
          BitVecType(64)
        ) // "_PC" flag, useful for jumps later on
      case "__BranchTaken" => null
      case "BTypeNext" => null
      case "BTypeCompatible" => null
  }

  def createExprVarArray(v: ArrayBuffer[String]): Variable = {
    v(0) match //currently only works for fields & arrays of size 2, but arrays bigger than that don't seem to appear
      case "_R" =>
        val size = getSizeofRegister(v(0))
        val name = "R" + v(1)
        Register(name, BitVecType(size))

      case "_Z" =>
        val size = getSizeofRegister(v(0))
        val name = "V" + v(1)
        Register(name, BitVecType(size))

      case "PSTATE" =>
        val Rname = v(1) + "F"
        LocalVar(Rname, BitVecType(1))
  }

  def getSizeofRegister(name: String): Int = {
    name match
      case "_R" => return 64
      case "_Z" => return 128 // or _V?  vector registers haven't come up yet
  }

}
