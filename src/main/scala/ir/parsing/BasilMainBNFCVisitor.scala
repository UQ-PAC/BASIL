package ir.parsing

import util.Logger
import basil_ir.{Absyn => syntax}
import ir.Sigil

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
    with syntax.Module.Visitor[BasilParseValue, A]
    with syntax.GobbleScolon.Visitor[Unit, A]
    with syntax.Declaration.Visitor[Unit, A]
    with syntax.IntType.Visitor[ir.IRType, A]
    with syntax.BoolType.Visitor[ir.IRType, A]
    with syntax.MapType.Visitor[ir.IRType, A]
    with syntax.BVType.Visitor[ir.IRType, A]
    with syntax.Type.Visitor[ir.IRType, A]
    with syntax.Endian.Visitor[ir.Endian, A]
    with syntax.Assignment.Visitor[(ir.Variable, ir.Expr), A]
    with syntax.Statement.Visitor[ir.dsl.DSLStatement, A]
    with syntax.LocalVar.Visitor[ir.LocalVar, A]
    with syntax.GlobalVar.Visitor[ir.Register, A]
    with syntax.CallLVars.Visitor[List[ir.Variable], A]
    with syntax.Jump.Visitor[ir.dsl.EventuallyJump, A]
    with syntax.LVar.Visitor[ir.Variable, A]
    with syntax.Block.Visitor[ir.dsl.EventuallyBlock, A]
    // with syntax.Params.Visitor[R,A]
    with syntax.ProcSig.Visitor[ir.dsl.EventuallyProcedure,A]
    // with syntax.ProcDef.Visitor[R,A]
    // with syntax.Value.Visitor[R,A]
    with syntax.Expr.Visitor[ir.Expr, A]
    // with syntax.BinOp.Visitor[ir.BinOP,A]
    // with syntax.UnOp.Visitor[ir.UnOp,A]
    // with syntax.EqOp.Visitor[ir.BinOp,A]
    // with syntax.BVUnOp.Visitor[ir.UnOp,A]
    // with syntax.BVBinOp.Visitor[ir.BinOp,A]
    // with syntax.BVLogicalBinOp.Visitor[ir.BinOp,A]
    // with syntax.IntBinOp.Visitor[ir.BinOp,A]
    // with syntax.IntLogicalBinOp.Visitor[ir.BinOp,A]
    // with syntax.BoolBinOp.Visitor[R,A]
    with syntax.RequireTok.Visitor[Unit, A]
    with syntax.EnsureTok.Visitor[Unit, A]
    with syntax.FunSpecDecl.Visitor[Unit, A]
    with syntax.ProgSpecDecl.Visitor[Unit, A]
    with AttributeListBNFCVisitor[A]
    {

  import scala.language.implicitConversions

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg)).toList

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg)).toList

  def stmts(x: syntax.ListStatement, arg: A): List[ir.dsl.DSLStatement] =
    x.asScala.map(_.accept(this, arg)).toList

  def lvars(x: syntax.ListLVar, arg: A): List[LVarSpec] =
    x.asScala.map(_.accept(this, arg)).toList

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

  override def visit(x: syntax.LRVar, arg: A) = x.accept(this, arg)

  // Members declared in Expr.Visitor
  override def visit(x: syntax.LocalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
    // is localvar
    val ty = x.type_.accept(this, arg)
    ir.LocalVar.ofIndexed(x.localident_.stripPrefix(Sigil.BASIR.localVar), ty)
  }
  override def visit(x: syntax.GlobalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
    val ty = x.type_.accept(this, arg) match {
      case ir.BitVecType(sz) => sz
      case t => throw Exception(s"Unsupported global var type : $t")
    }
    ir.Register(x.globalident_.stripPrefix(Sigil.BASIR.globalVar), ty)
  }

  override def visit(x: syntax.GRVar, arg: A) = {
    // check register is declared in global scope
    val rvar = x.globalvar_.accept(this, arg).asInstanceOf[ir.Register]
    decls.globals.get(rvar.name) match {
      case None => throw Exception(s"Reference to undefined global variable: $rvar")
      case Some(v) if rvar.irType != v.irType => throw Exception(s"Type mismatch $rvar declared ${v.irType}")
      case Some(v) => rvar
    }
  }

  override def visit(x: syntax.BinaryExpr, arg: A) =
    ir.BinaryExpr(x.binop_.accept(this, arg), x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))
  override def visit(x: syntax.UnaryExpr, arg: A) =
    ir.UnaryExpr(x.unop_.accept(this, arg), x.expr_.accept(this, arg))
  override def visit(x: syntax.ZeroExtend, arg: A) =
    ir.ZeroExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.SignExtend, arg: A) =
    ir.SignExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Extract, arg: A) =
    ir.Extract(x.intval_1.accept(this, arg).toInt, x.intval_2.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Concat, arg: A) =
    ir.BinaryExpr(ir.BVCONCAT, x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))

  // Members declared in Statement.Visitor
  override def visit(x: syntax.Assignment1, arg: A): (ir.Variable, ir.Expr) =
    ((x.lvar_.accept(this, arg)), x.expr_.accept(this, arg))

  override def visit(x: syntax.Assign, arg: A) =
    val assign = x.assignment_.accept(this, arg)
    ir.LocalAssign(assign._1, assign._2)

  override def visit(x: syntax.SLoad, arg: A) = ir.MemoryLoad(
    x.lvar_.accept(this, arg),
    decls.memories(x.globalident_),
    x.expr_.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )
  override def visit(x: syntax.SStore, arg: A) = ir.MemoryStore(
    decls.memories(x.globalident_),
    x.expr_1.accept(this, arg),
    x.expr_2.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )
  override def visit(x: syntax.DirectCall, arg: A) =
    val outs = x.calllvars_.accept(this, arg).list(_.v)
    val ins = exprs(x.listexpr_, arg)
    val proc = decls.procedures(x.procident_)
    ir.dsl.directCall(
      proc.out.keys.zip(outs).toList,
      x.procident_,
      // TODO: fix var names. in vars need to be obtained from proc definition??
      proc.in.keys.zip(ins).toList
    )
  override def visit(x: syntax.IndirectCall, arg: A) = ir.dsl.indirectCall(x.expr_.accept(this, arg))
  override def visit(x: syntax.Assume, arg: A) = ir.Assume(x.expr_.accept(this, arg))
  override def visit(x: syntax.Assert, arg: A) = ir.Assert(x.expr_.accept(this, arg))

  // Members declared in Jump.Visitor
  override def visit(x: syntax.GoTo, arg: A) = ir.dsl.goto(x.listbident_.asScala.toSeq: _*)
  override def visit(x: syntax.Unreachable, arg: A) = ir.dsl.unreachable
  override def visit(x: syntax.Return, arg: A) =
    val es = exprs(x.listexpr_, arg)
    val proc = decls.procedures(procName)
    ir.dsl.ret(proc.out.keys.zip(es).toList: _*)

  // Members declared in LVar.Visitor
  override def visit(x: syntax.LVarDef, arg: A) =
    x.localvar_.accept(this, arg)

  override def visit(x: syntax.GlobalLVar, arg: A) =
    x.globalvar_.accept(this, arg)
    // (x.bident_, x.type_.accept(this, arg), false)

  // Members declared in CallLVars.Visitor
  override def visit(x: syntax.NoOutParams, arg: A) = Nil
  override def visit(x: syntax.LocalVars, arg: A) =
    x.listlocalvar_.asScala.toList.map(_.accept(this, arg))
  override def visit(x: syntax.ListOutParams, arg: A) = 
    x.listlvar_.asScala.toList.map(_.accept(this, arg))


  // Members declared in Block.Visitor
  override def visit(x: syntax.Block1, arg: A) : EventuallyBlock =
    val ss = stmts(x.liststatement_, arg) :+ x.jump_.accept(this, arg)
    val addr = x.attrdeflist_.accept(this, arg).collect {
      case Attrib.ValueAttr("address", ir.IntLiteral(x)) => x
      case Attrib.ValueAttr("address", ir.BitVecLiteral(x, _)) => x
    }.headOption

    ir.dsl.block(x.blockident_.stripPrefix(Sigil.BASIR.block), ss: _*).copy(address = addr)

  private inline def cannotVisitDeclaration(x: HasParsePosition) =
    throw new Exception(
      "this declaration visit method cannot be called on this visitor. it should be handled by another visitor."
    )

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.MemDecl, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.VarDecl, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in .ProcDef.Visitor
  override def visit(x: syntax.ProcedureDef, arg: A): Nothing = cannotVisitDeclaration(x)
  override def visit(x: syntax.ProcedureDecl, arg: A): Nothing = cannotVisitDeclaration(x)
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

def x[T]: AllVisitor[Any, T] = InnerBasilBNFCVisitor[T]("beaneroo", Declarations.empty)

case class BasilMainBNFCVisitor[A](
  val decls: Declarations,
  val makeVisitor: (String, Declarations) => basil_ir.AllVisitor[BasilParseValue, A] = InnerBasilBNFCVisitor[A](_, _)
) extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Module.Visitor[ir.dsl.EventuallyProgram, A]
    with syntax.Declaration.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.Params.Visitor[ir.LocalVar, A]
    with syntax.ProcSig.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.ProcDef.Visitor[ir.dsl.EventuallyProcedure, (A, String)] 
    with AttributeListBNFCVisitor
    {

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A) =
    ir.LocalVar.ofIndexed(x.localident_.stripPrefix(Sigil.BASIR.localVar), x.type_.accept(this, arg))

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.MemDecl, arg: A): Nothing = throw new Exception(
    "MemDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.VarDecl, arg: A): Nothing = throw new Exception(
    "VarDecl should be visited by an earlier visitor"
  )


  override def visit(x: syntax.ProcedureSig, arg: A) : ir.dsl.EventuallyProcedure = {
    val name = x.procident_.stripPrefix(Sigil.BASIR.proc)
    decls.procedures(name)
  }

  override def visit(x: syntax.ProcedureDecl, arg: A) =
    x.procsig_.accept(this, arg)

  override def visit(x: syntax.ProcedureDef, arg: A) =
    val p = x.procsig_.accept(this, arg)
    val blocks = x.listblock_.asScala.toSeq.map(_.accept(this, arg))
    p.copy(blocks = blocks)

  // Members declared in ProcDef.Visitor
  override def visit(x: syntax.ProcedureDecl, args: (A, String)) =
    val (arg, procName) = args
    val innervis = makeVisitor(procName, decls)
    val p = ir.dsl.proc(unquote(x.str_, x), x.internalblocks_.accept(innervis, arg).list(_.block): _*)
    p.copy(address = x.paddress_.accept(this, arg).opt(_.int), entryBlockLabel = x.pentry_.accept(this, arg).opt(_.str))

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A) = {
    val ds = x.listdeclaration_.filter {
      case x : syntax.MemDecl => false 
      case x : syntax.VarDecl => false 
      case x : syntax.ProcedureDecl => true 
      case o => throw Exception("parsed decls contains unhandled type: " + o)
    }

    val prog = x.listdeclaration_.collect {
      case d: ProgDeclSpec => d.attrdeflist_.accept(this, arg)
      case d: ProgDecl => d.attrdeflist_.accept(this, arg)
    }

    val entryname = decls.metas.getOrElse("entry_procedure", throw ParseException("missing entry_procedure key", x))

    val procs = procdecls.map(_.accept(this, arg))

    val (mainProc, otherProcs) = procs.partition(_.name == entryname)
    ir.dsl.EventuallyProgram(mainProc.head, otherProcs)
  }
}

object ParseBasilIL {

  def loadILReader(reader: Reader) = {
    val lexer = new basil_ir.Yylex(reader);
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pProgram()

    val vis0 = BasilEarlyBNFCVisitor[Unit]()
    val decls = ast.accept(vis0, ())
    Logger.debug(decls)

    val vis = BasilMainBNFCVisitor[Unit](decls)
    val result = ast.accept(vis, ())
    Logger.debug(result)
    val prog = result.resolve
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
    parse(args(0))
  }
}
