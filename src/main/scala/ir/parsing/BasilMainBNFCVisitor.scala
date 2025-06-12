package ir.parsing

import util.Logger
import basil_ir.{Absyn => syntax}
import ir.Sigil

import java.io.{FileReader, StringReader, Reader}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

def unsigilBlock(x: String) = x.stripPrefix(Sigil.BASIR.block)
def unsigilProc(x: String) = x.stripPrefix(Sigil.BASIR.proc)
def unsigilLocal(x: String) =
  /* keep sigil as it is optional, to preserve var name */
  x
def unsigilGlobal(x: String) = x.stripPrefix(Sigil.BASIR.globalVar)
def unsigilAttrib(x: String) = x.stripPrefix(Sigil.BASIR.attrib)

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
    with AttributeListBNFCVisitor[A]
    with syntax.Module.Visitor[Unit, A]
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
    with syntax.Expr.Visitor[ir.Expr, A]
    // with syntax.Declaration.Visitor[Unit, A]
    // with syntax.Params.Visitor[R,A]
    // with syntax.ProcSig.Visitor[ir.dsl.EventuallyProcedure,A]
    // with syntax.ProcDef.Visitor[R,A]
    // with syntax.Value.Visitor[R,A]
    // with syntax.BinOp.Visitor[ir.BinOP,A]
    // with syntax.UnOp.Visitor[ir.UnOp,A]
    // with syntax.EqOp.Visitor[ir.BinOp,A]
    // with syntax.BVUnOp.Visitor[ir.UnOp,A]
    // with syntax.BVBinOp.Visitor[ir.BinOp,A]
    // with syntax.BVLogicalBinOp.Visitor[ir.BinOp,A]
    // with syntax.IntBinOp.Visitor[ir.BinOp,A]
    // with syntax.IntLogicalBinOp.Visitor[ir.BinOp,A]
    // with syntax.BoolBinOp.Visitor[R,A]
    // with syntax.RequireTok.Visitor[Unit, A]
    // with syntax.EnsureTok.Visitor[Unit, A]
    {

  import scala.language.implicitConversions

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg)).toList

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg)).toList

  def stmts(x: syntax.ListStatement, arg: A): List[ir.dsl.DSLStatement] =
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

  override def visit(x: syntax.LRVar, arg: A) = x.localvar_.accept(this, arg)

  override def visit(x: basil_ir.Absyn.OldExpr, arg: A) = ir.OldExpr(x.expr_.accept(this, arg))
  // Members declared in Expr.Visitor
  //
  def visit(x0: syntax.Literal, x1: A): ir.Expr = x0.value_.accept(this, x1)
  def visit(x: syntax.FunctionOp, arg: A): ir.Expr = {
    val n = unsigilGlobal(x.globalident_)
    val args = x.listexpr_.asScala.toSeq.map(_.accept(this, arg))
    val rt = x.type_.accept(typesVisitor, arg)
    ir.UninterpretedFunction(n, args, rt)
  }
  override def visit(x: syntax.LocalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
    // is localvar
    val ty = x.type_.accept(this, arg)
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), ty)
  }
  override def visit(x: syntax.GlobalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
    val ty = x.type_.accept(this, arg) match {
      case ir.BitVecType(sz) => sz
      case t => throw Exception(s"Unsupported global var type : $t")
    }
    ir.Register(unsigilGlobal(x.globalident_), ty)
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
  override def visit(x: syntax.SimulAssign, arg: A): ir.dsl.DSLStatement = {
    throw Exception("unsupported")
  }

  override def visit(x: syntax.SLoad, arg: A) = ir.MemoryLoad(
    x.lvar_.accept(this, arg),
    decls.memories(unsigilGlobal(x.globalident_)),
    x.expr_.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )
  override def visit(x: syntax.SStore, arg: A) = ir.MemoryStore(
    decls.memories(unsigilGlobal(x.globalident_)),
    x.expr_1.accept(this, arg),
    x.expr_2.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )
  override def visit(x: syntax.DirectCall, arg: A) =
    val outs = x.calllvars_.accept(this, arg)
    val ins = exprs(x.listexpr_, arg)
    val proc = decls.procedures(unsigilProc(x.procident_))
    ir.dsl.directCall(
      proc.out.keys.zip(outs).toList,
      unsigilProc(x.procident_),
      // TODO: fix var names. in vars need to be obtained from proc definition??
      proc.in.keys.zip(ins).toList
    )
  override def visit(x: syntax.IndirectCall, arg: A) = {
    val v = x.expr_.accept(this, arg) match {
      case v: ir.Variable => v
      case o => throw Exception("Cannot make indir call target arbitrary expression")
    }
    ir.dsl.indirectCall(v)
  }
  override def visit(x: syntax.Assume, arg: A) = {
    val label = getLabelAttr(x.attrdeflist_, arg)
    val comment = getCommentAttr(x.attrdeflist_, arg)
    ir.Assume(x.expr_.accept(this, arg), comment, label, false)
  }
  override def visit(x: syntax.Guard, arg: A) = {
    val label = getLabelAttr(x.attrdeflist_, arg)
    val comment = getCommentAttr(x.attrdeflist_, arg)
    ir.Assume(x.expr_.accept(this, arg), comment, label, true)
  }
  override def visit(x: syntax.Assert, arg: A) = {
    val label = getLabelAttr(x.attrdeflist_, arg)
    val comment = getCommentAttr(x.attrdeflist_, arg)
    ir.Assert(x.expr_.accept(this, arg), comment, label)
  }

  // Members declared in Jump.Visitor
  override def visit(x: syntax.GoTo, arg: A) =
    val ids = x.listblockident_.asScala.toSeq.map(unsigilBlock)
    ir.dsl.goto(ids: _*)
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
  override def visit(x: syntax.Block1, arg: A): ir.dsl.EventuallyBlock =
    val ss = stmts(x.liststatement_, arg) :+ x.jump_.accept(this, arg)
    val addr = getAddrAttr(x.attrdeflist_, arg)
    val origLbl = getStrAttr("originalLabel")(x.attrdeflist_, arg)
    val meta = ir.Metadata(originalLabel = origLbl, address = addr)
    ir.dsl.block(unsigilBlock(x.blockident_), ss: _*).copy(meta = meta)

  private inline def cannotVisitDeclaration(x: HasParsePosition) =
    throw new Exception(
      "this declaration visit method cannot be called on this visitor. it should be handled by another visitor."
    )

  // Members declared in Params.Visitor
  // override def visit(x: syntax.Param, arg: A): Nothing = cannotVisitDeclaration(x)

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A): Nothing = cannotVisitDeclaration(x)

}

case class FunSpec(
  val require: List[ir.Expr] = List(),
  ensure: List[ir.Expr] = List(),
  invariant: Map[String, List[ir.Expr]] = Map()
) {

  def merge(o: FunSpec) = {
    FunSpec(
      require ++ o.require,
      ensure ++ o.ensure,
      util.functional.unionWith(invariant, o.invariant, (a, b) => a ++ b)
    )
  }

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
  val makeVisitor: (String, Declarations) => InnerBasilBNFCVisitor[A] = InnerBasilBNFCVisitor[A](_, _)
) extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Module.Visitor[ir.dsl.EventuallyProgram, A]
    with syntax.Declaration.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.Params.Visitor[ir.LocalVar, A]
    with syntax.ProcSig.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.ProcDef.Visitor[ir.dsl.EventuallyProcedure, (A, String)]
    with AttributeListBNFCVisitor[A]
    with syntax.FunSpecDecl.Visitor[FunSpec, (A, String)]
    // with syntax.ProgSpec.Visitor[Unit, A]
    {

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Params.Visitor
  override def visit(x: syntax.Param, arg: A) =
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), x.type_.accept(this, arg))

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.MemDecl, arg: A): Nothing = throw new Exception(
    "MemDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.VarDecl, arg: A): Nothing = throw new Exception(
    "VarDecl should be visited by an earlier visitor"
  )

  override def visit(x: syntax.ProcedureSig, arg: A): ir.dsl.EventuallyProcedure = {
    val name = unsigilProc(x.procident_)
    decls.procedures(name)
  }

  // Members declared in ProcDef.FunSpecDecl
  def visit(x: syntax.Require, args: (A, String)): FunSpec = {
    val (arg, procName) = args
    val innervis = makeVisitor(procName, decls)
    FunSpec(require = List(x.expr_.accept(innervis, arg)))
  }
  def visit(x: syntax.Ensure, args: (A, String)): FunSpec = {
    val (arg, procName) = args
    val innervis = makeVisitor(procName, decls)
    FunSpec(ensure = List(x.expr_.accept(innervis, arg)))
  }
  def visit(x: syntax.LoopInvariant, args: (A, String)): FunSpec = {
    val (arg, procName) = args
    val bl = unsigilBlock(x.blockident_)
    val innervis = makeVisitor(procName, decls)
    val e = x.expr_.accept(innervis, arg)
    FunSpec(invariant = Map(bl -> List(e)))
  }
  def procSpec(x: syntax.ListFunSpecDecl, args: (A, String)): FunSpec = {
    x.asScala.foldLeft(FunSpec())((acc, sp) => {
      acc.merge(sp.accept(this, args))
    })
  }

  // Members declared in ProcDef.Visitor
  override def visit(x: syntax.ProcedureDecl, args: (A, String)) = {
    val (arg, procName) = args
    val spec = procSpec(x.listfunspecdecl_, args)
    val p = decls.procedures(procName)
    p.copy(requires = spec.require, ensures = spec.ensure)
  }

  override def visit(x: syntax.ProcedureDef, args: (A, String)) = {
    val (arg, procName) = args
    val innervis = makeVisitor(procName, decls)
    val blocks = x.listblock_.asScala.toSeq.map(_.accept(innervis, arg))
    val spec = procSpec(x.listfunspecdecl_, args)
    decls.procedures(procName).copy(blocks = blocks, requires = spec.require, ensures = spec.ensure)
  }

  def visit(x: syntax.Procedure, arg: A): ir.dsl.EventuallyProcedure = {
    val addr = getAddrAttr(x.attrdeflist_, arg)
    val pname = x.procsig_.accept(this, arg).name
    val rname = getStrAttr("name")(x.attrdeflist_, arg).getOrElse(pname)
    val innervis = makeVisitor(rname, decls)
    val p = x.procdef_.accept(this, (arg, pname))
    p.copy(address = addr, label = rname)
  }

  def visit(x: syntax.AxiomDecl, arg: A): ir.dsl.EventuallyProcedure = ???
  def visit(x: syntax.ProgDeclWithSpec, arg: A): ir.dsl.EventuallyProcedure = ???
  def visit(x: syntax.ProgDecl, arg: A): ir.dsl.EventuallyProcedure = ???
  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A) = {
    val ds = x.listdeclaration_.asScala.filter {
      case x: syntax.MemDecl => false
      case x: syntax.VarDecl => false
      case x: syntax.Procedure => true
      case x: syntax.ProgDeclWithSpec => true
      case x: syntax.ProgDecl => true
      case o => throw Exception("parsed decls contains unhandled type: " + o)
    }

    val prog = ds.collect {
      case d: syntax.ProgDeclWithSpec => {
        (getStrAttr("entry")(d.attrdeflist_, arg), None)
      }
      case d: syntax.ProgDecl => {
        (getStrAttr("entry")(d.attrdeflist_, arg), None)
      }
    }

    val entryname: String = prog.headOption match {
      case Some(Some(entr), spec) => entr
      case _ => throw Exception("No main proc specified")
    }

    val procs = x.listdeclaration_.asScala.collect { case p: syntax.Procedure =>
      p.accept(this, arg)
    }

    val (mainProc, otherProcs) = procs.partition(_.name == entryname)
    if (mainProc.headOption.isEmpty) {
      throw Exception(s"Entry procedure not found: \"${entryname}\"\n${procs.map(_.name).mkString("\n")}")
    }
    ir.dsl.EventuallyProgram(mainProc.head, otherProcs)
  }
}

object ParseBasilIL {

  def loadILReader(reader: Reader) = {
    val lexer = new basil_ir.Yylex(reader);
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pModule()

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
