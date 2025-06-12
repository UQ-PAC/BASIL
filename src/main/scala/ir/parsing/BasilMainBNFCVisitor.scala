package ir.parsing

import util.{Logger, PerformanceTimer, LogLevel}
import basil_ir.{Absyn => syntax}

import java.io.{FileReader, StringReader, Reader}
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

  def lvars(x: syntax.ListLVar, arg: A): List[LVarSpec] =
    x.asScala.map(_.accept(this, arg).lvar).toList

  def variable(x: LVarSpec): ir.Variable =
    x match {
      case (name, ty, true) => ir.LocalVar.ofIndexed(name, ty)
      case (name, ty, false) => ir.Register(name, ty.asInstanceOf[ir.BitVecType].size)
    }

  def localvar(x: LVarSpec): ir.LocalVar =
    x match {
      case (name, ty, _) => ir.LocalVar.ofIndexed(name, ty)
    }

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
      .getOrElse(ir.LocalVar.ofIndexed(x.bident_, ty))

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

  // Members declared in Statement.Visitor
  override def visit(x: syntax.Assign, arg: A): BasilParseValue =
    ir.LocalAssign(variable(x.lvar_.accept(this, arg).lvar), x.expr_.accept(this, arg).expr)
  override def visit(x: syntax.SLoad, arg: A): BasilParseValue = ir.MemoryLoad(
    variable(x.lvar_.accept(this, arg).lvar),
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

  // Members declared in LVar.Visitor
  override def visit(x: syntax.LVarDef, arg: A): BasilParseValue =
    (x.bident_, x.type_.accept(this, arg).ty, true)
  override def visit(x: syntax.GlobalLVar, arg: A): BasilParseValue =
    (x.bident_, x.type_.accept(this, arg).ty, false)

  // Members declared in CallLVars.Visitor
  override def visit(x: syntax.NoOutParams, arg: A): BasilParseValue = Nil
  override def visit(x: syntax.LocalVars, arg: A): BasilParseValue =
    val innerlocals = x.listlvar_.asScala.collect { case x: syntax.LVarDef =>
      x
    }.toList
    if (innerlocals.nonEmpty) {
      throw ParseException(
        "`var` declaration used within a `var` out-params list." + " this is redundant and has no effect, the inner var keywords should be removed.",
        x
      )
    }
    lvars(x.listlvar_, arg).map(localvar)
  override def visit(x: syntax.ListOutParams, arg: A): BasilParseValue = lvars(x.listlvar_, arg).map(variable)

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

  private inline def cannotVisitDeclaration(x: HasParsePosition) =
    throw new Exception(
      "this declaration visit method cannot be called on this visitor. it should be handled by another visitor."
    )

  // Members declared in MExpr.Visitor
  override def visit(x: syntax.MSym, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.BlockM, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.LetDecl, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.MemDecl, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.VarDecl, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.Procedure, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Program.Visitor
  override def visit(x: syntax.Prog, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in .ProcDef.Visitor
  override def visit(x: syntax.PD, arg: A): Nothing = cannotVisitDeclaration(x)
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
case class BasilMainBNFCVisitor[A](
  val decls: Declarations,
  val makeVisitor: (String, Declarations) => basil_ir.AllVisitor[BasilParseValue, A] = InnerBasilBNFCVisitor[A](_, _)
) extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Program.Visitor[ir.dsl.EventuallyProgram, A]
    with syntax.Declaration.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.Params.Visitor[ir.LocalVar, A]
    with syntax.ProcDef.Visitor[ir.dsl.EventuallyProcedure, (A, String)] {

  def localvar(name: String, x: syntax.Type, arg: A): ir.LocalVar =
    ir.LocalVar.ofIndexed(name, x.accept(this, arg))

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A) =
    localvar(x.bident_, x.type_, arg)

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.LetDecl, arg: A): Nothing = throw new Exception(
    "LetDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.MemDecl, arg: A): Nothing = throw new Exception(
    "MemDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.VarDecl, arg: A): Nothing = throw new Exception(
    "VarDecl should be visited by an earlier visitor"
  )

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

object ParseBasilIL {
  def loadILReader(reader: Reader) = {
    val timer = PerformanceTimer("ParseBasilIL", LogLevel.DEBUG)

    val lexer = new basil_ir.Yylex(reader);
    timer.checkPoint("lexed")
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pProgram()
    timer.checkPoint("parsed")

    val vis0 = BasilEarlyBNFCVisitor[Unit]()
    val decls = ast.accept(vis0, ())
    timer.checkPoint("early visitor")
    // Logger.debug(decls)

    val vis = BasilMainBNFCVisitor[Unit](decls)
    val result = ast.accept(vis, ())
    timer.checkPoint("main visitor")
    // Logger.debug(result)
    val prog = result.resolve
    timer.checkPoint("dsl resolving")

    Logger.debug {
      import ir.dsl.given
      val s = prog.toScalaLines
      timer.checkPoint("toScalaLines")
      val _ = s.mkString
      timer.checkPoint("mkString")
      "computed mkString"
    }

    prog
  }

  def loadILFile(filePath: String): ir.Program = {
    val reader = new FileReader(filePath)
    loadILReader(reader)
  }

  def loadILString(text: String): ir.Program = {
    val reader = new StringReader(text)
    loadILReader(reader)
  }

  def parse(path: String) = {
    val prog = loadILFile(path)
    println(translating.PrettyPrinter.pp_prog(prog))
  }

  def main(args: Array[String]): Unit = {
    Logger.setLevel(LogLevel.DEBUG)
    parse(args(0))
  }
}
