package ir.parsing

import util.Freeze
import basil_ir.{Absyn => syntax}
import scala.jdk.CollectionConverters.*

import org.antlr.v4.runtime.{CommonTokenStream, CharStreams}

private type BaseParseTypes = ir.Program | ir.dsl.EventuallyProcedure | ir.dsl.EventuallyBlock | ir.dsl.EventuallyJump |
  ir.dsl.EventuallyStatement | ir.dsl.NonCallStatement | ir.Expr | ir.BinOp | ir.UnOp | ir.IRType | ir.Endian |
  ir.Variable | ir.Memory | String | BigInt

private type DSLStatement = ir.dsl.NonCallStatement | ir.dsl.EventuallyStatement | ir.dsl.EventuallyJump

private type ParseTypes = BaseParseTypes | List[BaseParseTypes] | Option[BaseParseTypes]

case class BasilParseValue(x: ParseTypes) {
  def ty = x.asInstanceOf[ir.IRType]
  def bvty = x.asInstanceOf[ir.BitVecType]
  def binop = x.asInstanceOf[ir.BinOp]
  def unop = x.asInstanceOf[ir.UnOp]
  def expr = x.asInstanceOf[ir.Expr]
  def str = x.asInstanceOf[String]
  def int = x.asInstanceOf[BigInt]
  def int32 =
    val y = int
    assert(y.isValidInt, "BigInt value is out of range for a 32-bit integer")
    y.toInt
  def v = x.asInstanceOf[ir.Variable]
  def endian = x.asInstanceOf[ir.Endian]
  def list[T](f: BasilParseValue => T) =
    x.asInstanceOf[List[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def opt[T](f: BasilParseValue => T) =
    x.asInstanceOf[Option[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def stmt = x.asInstanceOf[DSLStatement]
  def block = x.asInstanceOf[ir.dsl.EventuallyBlock]
  def proc = x.asInstanceOf[ir.dsl.EventuallyProcedure]
}

object BasilParseValue {
  given Conversion[ParseTypes, BasilParseValue] with
    def apply(x: ParseTypes) = BasilParseValue(x)
}

trait GlobalDeclVisitor[A]() extends syntax.Declaration.Visitor[BasilParseValue, A] {
  this: basil_ir.AllVisitor[BasilParseValue, A] =>

  import scala.language.implicitConversions

  private var tempGlobals: Freeze[Map[String, ir.Register]] = Freeze(Map.empty)
  private var tempMemories: Freeze[Map[String, ir.Memory]] = Freeze(Map.empty)

  def freezeGlobals() =
    tempGlobals.freeze()
    tempMemories.freeze()
  def globals() = tempGlobals.get
  def memories() = tempMemories.get

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.LetDecl, arg: A): BasilParseValue = "TODO metadata unimpl"
  override def visit(x: syntax.MemDecl, arg: A): BasilParseValue =
    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg).ty: @unchecked
    val mem = x.bident_ match {
      case "stack" => ir.StackMemory(x.bident_, addrwd, valwd)
      case _ => ir.SharedMemory(x.bident_, addrwd, valwd)
    }
    tempMemories.update(_ + (mem.name -> mem))
    mem
  override def visit(x: syntax.VarDecl, arg: A): BasilParseValue =
    val v = ir.Register(x.bident_, x.type_.accept(this, arg).bvty.size)
    tempGlobals.update(_ + (v.name -> v))
    v
}

case class BasilIRSyntaxVisitor[A]() extends basil_ir.AllVisitor[BasilParseValue, A] with GlobalDeclVisitor[A] {

  import scala.language.implicitConversions

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg).expr).toList

  def stmts(x: syntax.ListStatement, arg: A): List[DSLStatement] =
    x.asScala.map(_.accept(this, arg).stmt).toList

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg).block).toList

  def lvars(x: syntax.ListLVar, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg).v).toList

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg).v).toList

  def localvar(name: String, x: syntax.Type, arg: A): ir.LocalVar =
    ir.LocalVar(name, x.accept(this, arg).ty)

  // Members declared in Type.Visitor
  override def visit(x: syntax.TypeIntType, arg: A): BasilParseValue = x.inttype_.accept(this, arg)
  override def visit(x: syntax.TypeBoolType, arg: A): BasilParseValue = x.booltype_.accept(this, arg)
  override def visit(x: syntax.TypeMapType, arg: A): BasilParseValue = x.maptype_.accept(this, arg)
  override def visit(x: syntax.TypeBVType, arg: A): BasilParseValue = x.bvtype_.accept(this, arg)

  // Members declared in BoolType.Visitor
  override def visit(x: syntax.BoolT, arg: A): BasilParseValue = ir.BoolType

  // Members declared in MapType.Visitor
  override def visit(x: syntax.MapT, arg: A): BasilParseValue =
    ir.MapType(x.type_1.accept(this, arg).ty, x.type_2.accept(this, arg).ty)

  // Members declared in IntType.Visitor
  override def visit(x: syntax.IntT, arg: A): BasilParseValue = ir.IntType

  // Members declared in BVType.Visitor
  override def visit(x: syntax.BVT, arg: A): BasilParseValue = ir.BitVecType(x.bvtype_.stripPrefix("bv").toInt)

  // Members declared in BinOp.Visitor
  override def visit(x: syntax.BinOpBVBinOp, arg: A): BasilParseValue = x.bvbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBVLogicalBinOp, arg: A): BasilParseValue = x.bvlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBoolBinOp, arg: A): BasilParseValue = x.boolbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntLogicalBinOp, arg: A): BasilParseValue = x.intlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntBinOp, arg: A): BasilParseValue = x.intbinop_.accept(this, arg)

  // Members declared in BVLogicalBinOp.Visitor
  override def visit(x: syntax.BVLogicalBinOp_bvule, arg: A): BasilParseValue = ir.BVULE
  override def visit(x: syntax.BVLogicalBinOp_bvugt, arg: A): BasilParseValue = ir.BVUGT
  override def visit(x: syntax.BVLogicalBinOp_bvuge, arg: A): BasilParseValue = ir.BVUGE
  override def visit(x: syntax.BVLogicalBinOp_bvslt, arg: A): BasilParseValue = ir.BVSLT
  override def visit(x: syntax.BVLogicalBinOp_bvsle, arg: A): BasilParseValue = ir.BVSLE
  override def visit(x: syntax.BVLogicalBinOp_bvsgt, arg: A): BasilParseValue = ir.BVSGT
  override def visit(x: syntax.BVLogicalBinOp_bvsge, arg: A): BasilParseValue = ir.BVSGE
  override def visit(x: syntax.BVLogicalBinOp_bveq, arg: A): BasilParseValue = ir.EQ
  override def visit(x: syntax.BVLogicalBinOp_bvneq, arg: A): BasilParseValue = ir.NEQ

  // Members declared in IntLogicalBinOp.Visitor
  override def visit(x: syntax.IntLogicalBinOp_inteq, arg: A): BasilParseValue = ir.EQ
  override def visit(x: syntax.IntLogicalBinOp_intneq, arg: A): BasilParseValue = ir.NEQ
  override def visit(x: syntax.IntLogicalBinOp_intlt, arg: A): BasilParseValue = ir.IntLT
  override def visit(x: syntax.IntLogicalBinOp_intle, arg: A): BasilParseValue = ir.IntLE
  override def visit(x: syntax.IntLogicalBinOp_intgt, arg: A): BasilParseValue = ir.IntGE
  override def visit(x: syntax.IntLogicalBinOp_intge, arg: A): BasilParseValue = ir.IntGE

  // Members declared in BVBinOp.Visitor
  override def visit(x: syntax.BVBinOp_bvand, arg: A): BasilParseValue = ir.BVAND
  override def visit(x: syntax.BVBinOp_bvor, arg: A): BasilParseValue = ir.BVOR
  override def visit(x: syntax.BVBinOp_bvadd, arg: A): BasilParseValue = ir.BVADD
  override def visit(x: syntax.BVBinOp_bvmul, arg: A): BasilParseValue = ir.BVMUL
  override def visit(x: syntax.BVBinOp_bvudiv, arg: A): BasilParseValue = ir.BVUDIV
  override def visit(x: syntax.BVBinOp_bvurem, arg: A): BasilParseValue = ir.BVUREM
  override def visit(x: syntax.BVBinOp_bvshl, arg: A): BasilParseValue = ir.BVSHL
  override def visit(x: syntax.BVBinOp_bvlshr, arg: A): BasilParseValue = ir.BVLSHR
  override def visit(x: syntax.BVBinOp_bvult, arg: A): BasilParseValue = ir.BVULT
  override def visit(x: syntax.BVBinOp_bvnand, arg: A): BasilParseValue = ir.BVNAND
  override def visit(x: syntax.BVBinOp_bvnor, arg: A): BasilParseValue = ir.BVNOR
  override def visit(x: syntax.BVBinOp_bvxor, arg: A): BasilParseValue = ir.BVXOR
  override def visit(x: syntax.BVBinOp_bvxnor, arg: A): BasilParseValue = ir.BVXNOR
  override def visit(x: syntax.BVBinOp_bvcomp, arg: A): BasilParseValue = ir.BVCOMP
  override def visit(x: syntax.BVBinOp_bvsub, arg: A): BasilParseValue = ir.BVSUB
  override def visit(x: syntax.BVBinOp_bvsdiv, arg: A): BasilParseValue = ir.BVSDIV
  override def visit(x: syntax.BVBinOp_bvsrem, arg: A): BasilParseValue = ir.BVSREM
  override def visit(x: syntax.BVBinOp_bvsmod, arg: A): BasilParseValue = ir.BVSMOD
  override def visit(x: syntax.BVBinOp_bvashr, arg: A): BasilParseValue = ir.BVASHR
  /*
   use enum value from name
   f_��5lywAir.pbv$UBxxxj0
   */

  // Members declared in IntBinOp.Visitor
  override def visit(x: syntax.IntBinOp_intadd, arg: A): BasilParseValue = ir.IntADD
  override def visit(x: syntax.IntBinOp_intmul, arg: A): BasilParseValue = ir.IntMUL
  override def visit(x: syntax.IntBinOp_intsub, arg: A): BasilParseValue = ir.IntSUB
  override def visit(x: syntax.IntBinOp_intdiv, arg: A): BasilParseValue = ir.IntDIV
  override def visit(x: syntax.IntBinOp_intmod, arg: A): BasilParseValue = ir.IntMOD

  // Members declared in BoolBinOp.Visitor
  override def visit(x: syntax.BoolBinOp_booleq, arg: A): BasilParseValue = ir.EQ
  override def visit(x: syntax.BoolBinOp_boolneq, arg: A): BasilParseValue = ir.NEQ
  override def visit(x: syntax.BoolBinOp_booland, arg: A): BasilParseValue = ir.BoolAND
  override def visit(x: syntax.BoolBinOp_boolor, arg: A): BasilParseValue = ir.BoolOR
  override def visit(x: syntax.BoolBinOp_boolimplies, arg: A): BasilParseValue = ir.BoolIMPLIES
  override def visit(x: syntax.BoolBinOp_boolequiv, arg: A): BasilParseValue = ??? // XXX: TODO

  // Members declared in BVUnOp.Visitor
  override def visit(x: syntax.BVUnOp_bvnot, arg: A): BasilParseValue = ir.BVNOT
  override def visit(x: syntax.BVUnOp_bvneg, arg: A): BasilParseValue = ir.BVNEG

  // Members declared in UnOp.Visitor
  override def visit(x: syntax.UnOpBVUnOp, arg: A): BasilParseValue = x.bvunop_.accept(this, arg)
  override def visit(x: syntax.UnOp_boolnot, arg: A): BasilParseValue = ir.BoolNOT
  override def visit(x: syntax.UnOp_intneg, arg: A): BasilParseValue = ir.IntNEG

  // Members declared in Endian.Visitor
  override def visit(x: syntax.LittleEndian, arg: A): BasilParseValue = ir.Endian.LittleEndian
  override def visit(x: syntax.BigEndian, arg: A): BasilParseValue = ir.Endian.BigEndian

  // Members declared in IntVal.Visitor
  override def visit(x: syntax.HexInt, arg: A): BasilParseValue =
    BigInt(x.integerhex_.toLowerCase.stripPrefix("0x"), 16)
  override def visit(x: syntax.DecInt, arg: A): BasilParseValue = BigInt(x.integer_) // XXX: int32

  // Members declared in Expr.Visitor
  override def visit(x: syntax.RVar, arg: A): BasilParseValue =
    // handle registers which are declared in the global scope. everything else
    // is localvar
    val ty = x.type_.accept(this, arg).ty
    globals()
      .get(x.bident_)
      .filter(x => ir.BitVecType(x.size) == ty)
      .getOrElse(ir.LocalVar(x.bident_, ty))

  override def visit(x: syntax.BinaryExpr, arg: A): BasilParseValue =
    ir.BinaryExpr(x.binop_.accept(this, arg).binop, x.expr_1.accept(this, arg).expr, x.expr_2.accept(this, arg).expr)
  override def visit(x: syntax.UnaryExpr, arg: A): BasilParseValue =
    ir.UnaryExpr(x.unop_.accept(this, arg).unop, x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.ZeroExtend, arg: A): BasilParseValue =
    ir.ZeroExtend(x.intval_.accept(this, arg).int32, x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.SignExtend, arg: A): BasilParseValue =
    ir.SignExtend(x.intval_.accept(this, arg).int32, x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.Extract, arg: A): BasilParseValue =
    ir.Extract(x.intval_1.accept(this, arg).int32, x.intval_2.accept(this, arg).int32, x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.Concat, arg: A): BasilParseValue =
    ir.BinaryExpr(ir.BVCONCAT, x.expr_1.accept(this, arg).expr, x.expr_2.accept(this, arg).expr)
  override def visit(x: syntax.BVLiteral, arg: A): BasilParseValue =
    ir.BitVecLiteral(x.intval_.accept(this, arg).int, x.bvtype_.accept(this, arg).bvty.size)
  override def visit(x: syntax.IntLiteral, arg: A): BasilParseValue = ir.IntLiteral(x.intval_.accept(this, arg).int)
  override def visit(x: syntax.TrueLiteral, arg: A): BasilParseValue = ir.TrueLiteral
  override def visit(x: syntax.FalseLiteral, arg: A): BasilParseValue = ir.FalseLiteral

  // Members declared in LVar.Visitor
  override def visit(x: syntax.LVarDef, arg: A): BasilParseValue =
    localvar(x.bident_, x.type_, arg)
  override def visit(x: syntax.GlobalLVar, arg: A): BasilParseValue =
    ir.Register(x.bident_, x.type_.accept(this, arg).bvty.size)

  // Members declared in Statement.Visitor
  override def visit(x: syntax.Assign, arg: A): BasilParseValue =
    ir.LocalAssign(x.lvar_.accept(this, arg).v, x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.SLoad, arg: A): BasilParseValue = ir.MemoryLoad(
    x.lvar_.accept(this, arg).v,
    memories()(x.bident_),
    x.expr_.accept(this, arg).expr,
    x.endian_.accept(this, arg).endian,
    x.intval_.accept(this, arg).int32
  )
  override def visit(x: syntax.SStore, arg: A): BasilParseValue = ir.MemoryStore(
    memories()(x.bident_),
    x.expr_1.accept(this, arg).expr,
    x.expr_2.accept(this, arg).expr,
    x.endian_.accept(this, arg).endian,
    x.intval_.accept(this, arg).int32
  )
  override def visit(x: syntax.DirectCall, arg: A): BasilParseValue = ir.dsl.directCall(
    x.calllvars_.accept(this, arg).list(_.v).map(x => "TODO outvarname" -> x),
    x.bident_,
    // TODO: fix var names. in vars need to be obtained from proc definition??
    exprs(x.listexpr_, arg).map("TODO invarname" -> _)
  )
  override def visit(x: syntax.IndirectCall, arg: A): BasilParseValue = ir.dsl.indirectCall(x.expr_.accept(this, arg).v)
  override def visit(x: syntax.Assume, arg: A): BasilParseValue = ir.Assume(x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.Assert, arg: A): BasilParseValue = ir.Assert(x.expr_.accept(this, arg).expr)

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A): BasilParseValue =
    localvar(x.bident_, x.type_, arg)

  // Members declared in Jump.Visitor
  override def visit(x: syntax.GoTo, arg: A): BasilParseValue = ir.dsl.goto(x.listbident_.asScala.toSeq: _*)
  override def visit(x: syntax.Unreachable, arg: A): BasilParseValue = ir.dsl.unreachable
  override def visit(x: syntax.Return, arg: A): BasilParseValue =
    ir.dsl.ret(exprs(x.listexpr_, arg).map("TODO returnvarname" -> _): _*)

  // Members declared in CallLVars.Visitor
  override def visit(x: syntax.NoOutParams, arg: A): BasilParseValue = Nil
  override def visit(x: syntax.LocalVars, arg: A): BasilParseValue = lvars(x.listlvar_, arg)
  override def visit(x: syntax.ListOutParams, arg: A): BasilParseValue = lvars(x.listlvar_, arg)

  // Members declared in AddrAttr.Visitor
  override def visit(x: syntax.AddrAttrSome, arg: A): BasilParseValue =
    Some(x.intval_.accept(this, arg).int)
  override def visit(x: syntax.AddrAttrNone, arg: A): BasilParseValue = None
  override def visit(x: syntax.AddrAttrEmpty, arg: A): BasilParseValue = None

  // Members declared in Block.Visitor
  override def visit(x: syntax.B, arg: A): BasilParseValue = ir.dsl.block(x.bident_, stmts(x.liststatement_, arg): _*)

  // Members declared in PAddress.Visitor
  override def visit(x: syntax.AddrSome, arg: A): BasilParseValue =
    Some(x.intval_.accept(this, arg).int)
  override def visit(x: syntax.AddrNone, arg: A): BasilParseValue = None

  // Members declared in PEntry.Visitor
  override def visit(x: syntax.EntrySome, arg: A): BasilParseValue = Some(x.str_)
  override def visit(x: syntax.EntryNone, arg: A): BasilParseValue = None

  // Members declared in InternalBlocks.Visitor
  override def visit(x: syntax.BSome, arg: A): BasilParseValue = blocks(x.listblock_, arg)
  override def visit(x: syntax.BNone, arg: A): BasilParseValue = Nil

  // Members declared in ProcDef.Visitor
  override def visit(x: syntax.PD, arg: A): BasilParseValue =
    val p = ir.dsl.proc(x.str_, x.internalblocks_.accept(this, arg).list(_.block): _*)
    p.copy(address = x.paddress_.accept(this, arg).opt(_.int), entryBlockLabel = x.pentry_.accept(this, arg).opt(_.str))

  override def visit(x: syntax.Procedure, arg: A): BasilParseValue =
    val p = x.procdef_.accept(this, arg).proc
    val inparams = params(x.listparams_1, arg)
    val outparams = params(x.listparams_2, arg)
    p.copy(in = inparams.map(x => x.name -> x.getType).toMap, out = outparams.map(x => x.name -> x.getType).toMap)

  // Members declared in MExpr.Visitor
  override def visit(x: syntax.MSym, arg: A): BasilParseValue = "TODO msym"
  override def visit(x: syntax.BlockM, arg: A): BasilParseValue = "TODO blockm"

  // Members declared in Program.Visitor
  override def visit(x: syntax.Prog, arg: A): BasilParseValue =
    val (procdecls, otherdecls) = x.listdeclaration_.asScala
      .partition(_.isInstanceOf[syntax.Procedure])
    println(otherdecls.map(_.accept(this, arg)))
    freezeGlobals()
    println(procdecls.map(_.accept(this, arg)))
    "TODO constructprogram"

}

object Run {

  def parse(path: String) = {
    val lexer = new basil_ir.BasilIRLexer(CharStreams.fromFileName(path))
    val parser = new basil_ir.BasilIRParser(new CommonTokenStream(lexer))

    val ctx = parser.start_Program()

    val vis = BasilIRSyntaxVisitor[Unit]()
    val result = ctx.result.accept(vis, ())

  }

  def main(args: Array[String]): Unit = {
    parse(args(0))
  }

}
