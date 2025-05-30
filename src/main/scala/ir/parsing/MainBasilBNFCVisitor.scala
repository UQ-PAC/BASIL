package ir.parsing

import basil_ir.{Absyn => syntax}

import java.io.FileReader
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

/**
 * Parses structures at the block-level and lower, given
 * a particular procName (including address) and declarations.
 *
 * The given typesVisitor is optional. If unspecified, a default instance
 * is constructed.
 *
 * @group mainvisitor
 */
case class InnerBasilBNFCVisitor[A](
  val procName: String,
  val decls: Declarations,
  val typesVisitor: TypesVisitorType[A] = new TypesBNFCVisitor[A]() {}
) extends LiteralsBNFCVisitor[A]
    with basil_ir.AllVisitor[BasilParseValue, A] {

  import scala.language.implicitConversions

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg).block).toList

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg).expr).toList

  def stmts(x: syntax.ListStatement, arg: A): List[ir.dsl.DSLStatement] =
    x.asScala.map(_.accept(this, arg).stmt).toList

  def lvars(x: syntax.ListLVar, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg).v).toList

  def localvar(name: String, x: syntax.Type, arg: A): ir.LocalVar =
    ir.LocalVar(name, x.accept(this, arg).ty)

  // Members declared in Type.Visitor
  override def visit(x: syntax.TypeIntType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeBoolType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeMapType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeBVType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in BoolType.Visitor
  override def visit(x: syntax.BoolT, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in MapType.Visitor
  override def visit(x: syntax.MapT, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in IntType.Visitor
  override def visit(x: syntax.IntT, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in BVType.Visitor
  override def visit(x: syntax.BVT, arg: A) = x.accept(typesVisitor, arg): ir.IRType

  // Members declared in Expr.Visitor
  override def visit(x: syntax.RVar, arg: A): BasilParseValue =
    // handle registers which are declared in the global scope. everything else
    // is localvar
    val ty = x.type_.accept(this, arg).ty
    decls.globals
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
    decls.memories(x.bident_),
    x.expr_.accept(this, arg).expr,
    x.endian_.accept(this, arg).endian,
    x.intval_.accept(this, arg).int32
  )
  override def visit(x: syntax.SStore, arg: A): BasilParseValue = ir.MemoryStore(
    decls.memories(x.bident_),
    x.expr_1.accept(this, arg).expr,
    x.expr_2.accept(this, arg).expr,
    x.endian_.accept(this, arg).endian,
    x.intval_.accept(this, arg).int32
  )
  override def visit(x: syntax.DirectCall, arg: A): BasilParseValue =
    val outs = x.calllvars_.accept(this, arg).list(_.v)
    val ins = exprs(x.listexpr_, arg)
    val proc = decls.procedures(x.bident_)
    ir.dsl.directCall(
      proc.out.keys.zip(outs).toList,
      x.bident_,
      // TODO: fix var names. in vars need to be obtained from proc definition??
      proc.in.keys.zip(ins).toList
    )
  override def visit(x: syntax.IndirectCall, arg: A): BasilParseValue = ir.dsl.indirectCall(x.expr_.accept(this, arg).v)
  override def visit(x: syntax.Assume, arg: A): BasilParseValue = ir.Assume(x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.Assert, arg: A): BasilParseValue = ir.Assert(x.expr_.accept(this, arg).expr)

  // Members declared in Jump.Visitor
  override def visit(x: syntax.GoTo, arg: A): BasilParseValue = ir.dsl.goto(x.listbident_.asScala.toSeq: _*)
  override def visit(x: syntax.Unreachable, arg: A): BasilParseValue = ir.dsl.unreachable
  override def visit(x: syntax.Return, arg: A): BasilParseValue =
    val es = exprs(x.listexpr_, arg)
    val proc = decls.procedures(procName)
    ir.dsl.ret(proc.out.keys.zip(es).toList: _*)

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
  override def visit(x: syntax.B, arg: A): BasilParseValue =
    val ss = stmts(x.liststatement_, arg) :+ x.jump_.accept(this, arg).stmt
    val addr = x.addrattr_.accept(this, arg).opt(_.int)
    ir.dsl.block(x.bident_, ss: _*).copy(address = addr)

  // Members declared in InternalBlocks.Visitor
  override def visit(x: syntax.BSome, arg: A): BasilParseValue = blocks(x.listblock_, arg)
  override def visit(x: syntax.BNone, arg: A): BasilParseValue = Nil

  // Members declared in MExpr.Visitor
  override def visit(x: syntax.MSym, arg: A) = UndefinedParseResult
  override def visit(x: syntax.BlockM, arg: A) = UndefinedParseResult

  // Members declared in Declaration.Visitor
  def visit(x: syntax.LetDecl, arg: A): BasilParseValue = UndefinedParseResult
  def visit(x: syntax.MemDecl, arg: A): BasilParseValue = UndefinedParseResult
  def visit(x: syntax.VarDecl, arg: A): BasilParseValue = UndefinedParseResult
  def visit(x: syntax.Procedure, arg: A): BasilParseValue = UndefinedParseResult

  // Members declared in Params.Visitor
  def visit(x: syntax.Param, arg: A): BasilParseValue = UndefinedParseResult

  // Members declared in Program.Visitor
  def visit(x: syntax.Prog, arg: A): BasilParseValue = UndefinedParseResult

  // Members declared in .ProcDef.Visitor
  def visit(x: syntax.PD, arg: A): BasilParseValue = UndefinedParseResult
}

/**
 * Entry-point for the main visitor, parsing everything from the procedure
 * level and lower, given the pre-parsed global declarations.
 *
 * The optional makeVisitor param is used to construct the parser for the blocks
 * and inner structures. If not given, defaults to constructing a
 * [[ir.parsing.InnerBasilBNFCVisitor]].
 *
 * @group mainvisitor
 */
case class MainBasilBNFCVisitor[A](
  val decls: Declarations,
  val makeVisitor: (String, Declarations) => basil_ir.AllVisitor[BasilParseValue, A] = InnerBasilBNFCVisitor[A](_, _)
) extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Program.Visitor[ir.dsl.EventuallyProgram, A]
    with syntax.Declaration.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.Params.Visitor[ir.LocalVar, A]
    with syntax.ProcDef.Visitor[ir.dsl.EventuallyProcedure, (A, String)] {

  def localvar(name: String, x: syntax.Type, arg: A): ir.LocalVar =
    ir.LocalVar(name, x.accept(this, arg))

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A) =
    localvar(x.bident_, x.type_, arg)

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.LetDecl, arg: A) = throw new Exception("LetDecl should be visited by an earlier visitor")
  override def visit(x: syntax.MemDecl, arg: A) = throw new Exception("MemDecl should be visited by an earlier visitor")
  override def visit(x: syntax.VarDecl, arg: A) = throw new Exception("VarDecl should be visited by an earlier visitor")

  override def visit(x: syntax.Procedure, arg: A) =
    val p = x.procdef_.accept(this, (arg, x.bident_))
    val inparams = params(x.listparams_1, arg)
    val outparams = params(x.listparams_2, arg)
    p.copy(in = inparams.map(x => x.name -> x.getType).toMap, out = outparams.map(x => x.name -> x.getType).toMap)

  // Members declared in ProcDef.Visitor
  override def visit(x: syntax.PD, args: (A, String)) =
    val (arg, procName) = args
    val innervis = makeVisitor(procName, decls)
    val p = ir.dsl.proc(unquote(x.str_, x), x.internalblocks_.accept(innervis, arg).list(_.block): _*)
    p.copy(address = x.paddress_.accept(this, arg).opt(_.int), entryBlockLabel = x.pentry_.accept(this, arg).opt(_.str))

  // Members declared in Program.Visitor
  override def visit(x: syntax.Prog, arg: A) = {
    val ds = x.listdeclaration_.asScala.toList.groupBy(_.getClass).to(mutable.Map)
    val _ = ds.remove(classOf[syntax.LetDecl]).getOrElse(Nil)
    val _ = ds.remove(classOf[syntax.MemDecl]).getOrElse(Nil)
    val _ = ds.remove(classOf[syntax.VarDecl]).getOrElse(Nil)
    val procdecls = ds.remove(classOf[syntax.Procedure]).getOrElse(Nil)
    assert(ds.isEmpty, "parsed decls contains unhandled type: " + ds.keys)

    val entryname = decls.metas.getOrElse("entry_procedure", throw ParseException("missing entry_procedure key", x))

    val procs = procdecls.map(_.accept(this, arg))

    val (mainProc, otherProcs) = procs.partition(_.name == entryname)
    ir.dsl.EventuallyProgram(mainProc.head, otherProcs)
  }
}

object Run {

  def parse(path: String) = {
    val reader = new FileReader(path)
    val lexer = new basil_ir.Yylex(reader);
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pProgram()

    val vis0 = EarlyBasilBNFCVisitor[Unit]()
    val decls = ast.accept(vis0, ())
    println(decls)

    val vis = MainBasilBNFCVisitor[Unit](decls)
    val result = ast.accept(vis, ())
    println(result)
    val prog = result.resolve
    println(translating.PrettyPrinter.pp_prog(prog))
  }

  def main(args: Array[String]): Unit = {
    parse(args(0))
  }
}
