package ir.parsing

import basil_ir.Absyn as syntax
import ir.Sigil
import util.{LogLevel, PerformanceTimer}

import java.io.{FileReader, Reader, StringReader}
import scala.jdk.CollectionConverters.*
import scala.collection.immutable.ListMap

def unsigilBlock(x: String) = Sigil.unsigil(Sigil.BASIR.block)(x)
def unsigilProc(x: String) = Sigil.unsigil(Sigil.BASIR.proc)(x)
def unsigilLocal(x: String) =
  /* keep sigil as it is optional, to preserve var name */
  x
def unsigilGlobal(x: String) = Sigil.unsigil(Sigil.BASIR.globalVar)(x)
def unsigilAttrib(x: String) = Sigil.unsigil(Sigil.BASIR.attrib)(x)

/**
 * Visits items at the expression-level and smaller which are
 * *not* procedure-dependent. This includes [[basil_ir.Absyn.ProgSpec]] and
 * [[basil_ir.Absyn.FunSpec]] which are predicate expressions.
 */
class ExprBNFCVisitor[A](val decls: Declarations)
    extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.LocalVar.Visitor[ir.LocalVar, A]
    with syntax.GlobalVar.Visitor[ir.GlobalVar, A]
    with syntax.Expr.Visitor[ir.Expr, A]
    with syntax.LambdaDef.Visitor[ir.LambdaExpr, A] {

  def exprs(x: syntax.ListExpr, arg: A): List[ir.Expr] =
    x.asScala.map(_.accept(this, arg)).toList

  override def visit(x: syntax.Expr_Local, arg: A) = x.localvar_.accept(this, arg)

  override def visit(x: syntax.LambdaDef1, arg: A): ir.LambdaExpr = {
    val params = x.listlocalvar_.asScala.toList.map(_.accept(this, arg))
    val body = x.expr_.accept(this, arg)
    ir.LambdaExpr(params, body)
  }
  override def visit(x: syntax.Expr_Old, arg: A) = ir.OldExpr(x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Forall, arg: A): ir.Expr = {
    val ld = x.lambdadef_.accept(this, arg)
    ir.QuantifierExpr(ir.QuantifierSort.forall, ld)
  }
  override def visit(x: syntax.Expr_Exists, arg: A): ir.Expr = {
    val ld = x.lambdadef_.accept(this, arg)
    ir.QuantifierExpr(ir.QuantifierSort.exists, ld)
  }

  def visit(x0: syntax.Expr_Literal, x1: A): ir.Expr = x0.value_.accept(this, x1)
  def visit(x: syntax.Expr_FunctionOp, arg: A): ir.Expr = {
    val n = unsigilGlobal(x.globalident_)
    val rt = decls.functions.get(n) match {
      case Some(v) => v.returnType
      case None => throw Exception(s"Undeclared function: ${x.globalident_}")
    }
    val args = x.listexpr_.asScala.toSeq.map(_.accept(this, arg))
    ir.FApplyExpr(n, args, rt)
  }
  override def visit(x: syntax.LocalVar1, arg: A) = {
    val ty = x.type_.accept(this, arg)
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), ty)
  }
  override def visit(x: syntax.GlobalVar1, arg: A) = {
    val ty = x.type_.accept(this, arg)
    ir.GlobalVar(unsigilGlobal(x.globalident_), ty)
  }

  override def visit(x: syntax.Expr_Global, arg: A) = {
    // check register is declared in global scope
    val rvar = x.globalvar_.accept(this, arg)
    decls.globals.get(rvar.name) match {
      case None => throw Exception(s"Reference to undefined global variable: $rvar")
      case Some(v) if rvar.irType != v.irType => throw Exception(s"Type mismatch $rvar declared ${v.irType}")
      case Some(v) => rvar
    }
  }

  override def visit(x: syntax.Expr_Binary, arg: A) =
    ir.BinaryExpr(x.binop_.accept(this, arg), x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))
  override def visit(x: syntax.Expr_Unary, arg: A) =
    ir.UnaryExpr(x.unop_.accept(this, arg), x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_ZeroExtend, arg: A) =
    ir.ZeroExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_SignExtend, arg: A) =
    ir.SignExtend(x.intval_.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Extract, arg: A) =
    ir.Extract(x.intval_1.accept(this, arg).toInt, x.intval_2.accept(this, arg).toInt, x.expr_.accept(this, arg))
  override def visit(x: syntax.Expr_Concat, arg: A) =
    ir.BinaryExpr(ir.BVCONCAT, x.expr_1.accept(this, arg), x.expr_2.accept(this, arg))

}

/**
 * Parses structures at the block-level and lower, given
 * a particular procName (including address) and declarations.
 *
 * @group mainvisitor
 */
class BlockBNFCVisitor[A](val procName: String, private val _decls: Declarations)
    extends ExprBNFCVisitor[A](_decls)
    with AttributeListBNFCVisitor[A]
    with syntax.Assignment.Visitor[(ir.Variable, ir.Expr), A]
    with syntax.LVar.Visitor[ir.Variable, A]
    with syntax.LVars.Visitor[List[ir.Variable], A]
    with syntax.Jump.Visitor[ir.dsl.EventuallyJump, A]
    with syntax.Stmt.Visitor[ir.dsl.DSLStatement, A]
    with syntax.Block.Visitor[ir.dsl.EventuallyBlock, A]
    with syntax.ProcDef.Visitor[Seq[ir.dsl.EventuallyBlock], A] {

  val procSpec = decls.procSpecs(procName)
  val formalOuts = decls.formalOuts(procName)

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg)).toList

  def stmts(x: syntax.ListStmt, arg: A): List[ir.dsl.DSLStatement] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Statement.Visitor
  override def visit(x: syntax.Assignment1, arg: A): (ir.Variable, ir.Expr) =
    ((x.lvar_.accept(this, arg)), x.expr_.accept(this, arg))

  override def visit(x: syntax.Stmt_SingleAssign, arg: A) =
    val assign = x.assignment_.accept(this, arg)
    ir.LocalAssign(assign._1, assign._2)

  override def visit(x: syntax.Stmt_MultiAssign, arg: A): ir.dsl.DSLStatement = {
    throw Exception("unsupported")
  }

  override def visit(x: syntax.Stmt_Load, arg: A) = ir.MemoryLoad(
    x.lvar_.accept(this, arg),
    decls.memories(unsigilGlobal(x.globalident_)),
    x.expr_.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )

  override def visit(x: syntax.Stmt_Store, arg: A) = ir.MemoryStore(
    decls.memories(unsigilGlobal(x.globalident_)),
    x.expr_1.accept(this, arg),
    x.expr_2.accept(this, arg),
    x.endian_.accept(this, arg),
    x.intval_.accept(this, arg).toInt
  )

  override def visit(x: syntax.Stmt_DirectCall, arg: A) =
    val outs = x.lvars_.accept(this, arg)
    val ins = exprs(x.listexpr_, arg)

    val callName = unsigilProc(x.procident_)

    val formalIns = decls.formalIns(callName)
    val formalOuts = decls.formalOuts(callName)

    ir.dsl.directCall(
      formalOuts.keys.zip(outs),
      unsigilProc(x.procident_),
      formalIns.keys.zip(ins)
    )

  override def visit(x: syntax.Stmt_IndirectCall, arg: A) = {
    val v = x.expr_.accept(this, arg) match {
      case v: ir.Variable => v
      case o => throw Exception("Indirect call target must be a variable")
    }
    ir.dsl.indirectCall(v)
  }

  override def visit(x: syntax.Stmt_Assume, arg: A) = {
    val attr = parseAttrMap(x.attribset_, arg)
    val label = getLabelAttr(attr)
    val comment = getCommentAttr(attr)
    ir.Assume(x.expr_.accept(this, arg), comment, label, false)
  }

  override def visit(x: syntax.Stmt_Guard, arg: A) = {
    val attr = parseAttrMap(x.attribset_, arg)
    val label = getLabelAttr(attr)
    val comment = getCommentAttr(attr)
    ir.Assume(x.expr_.accept(this, arg), comment, label, true)
  }

  override def visit(x: syntax.Stmt_Assert, arg: A) = {
    val attr = parseAttrMap(x.attribset_, arg)
    val label = getLabelAttr(attr)
    val comment = getCommentAttr(attr)
    ir.Assert(x.expr_.accept(this, arg), comment, label)
  }

  // Members declared in Jump.Visitor
  override def visit(x: syntax.Jump_GoTo, arg: A) =
    val ids = x.listblockident_.asScala.toSeq.map(unsigilBlock)
    ir.dsl.goto(ids: _*)
  override def visit(x: syntax.Jump_Unreachable, arg: A) = ir.dsl.unreachable
  override def visit(x: syntax.Jump_Return, arg: A) =
    val es = exprs(x.listexpr_, arg)
    ir.dsl.ret(formalOuts.keys.zip(es).toList: _*)

  // Members declared in LVar.Visitor
  override def visit(x: syntax.LVar_Local, arg: A) =
    x.localvar_.accept(this, arg)

  override def visit(x: syntax.LVar_Global, arg: A) =
    x.globalvar_.accept(this, arg)
    // (x.bident_, x.type_.accept(this, arg), false)

  // Members declared in CallLVars.Visitor
  override def visit(x: syntax.LVars_Empty, arg: A) = Nil
  override def visit(x: syntax.LVars_LocalList, arg: A) =
    x.listlocalvar_.asScala.toList.map(_.accept(this, arg))
  override def visit(x: syntax.LVars_List, arg: A) =
    x.listlvar_.asScala.toList.map(_.accept(this, arg))

  // Members declared in Block.Visitor
  override def visit(x: syntax.Block1, arg: A): ir.dsl.EventuallyBlock =
    val ss = stmts(x.liststmt_, arg) :+ x.jump_.accept(this, arg)

    val attr = parseAttrMap(x.attribset_, arg)
    val addr = getAddrAttr(attr)
    val origLbl = getStrAttr("originalLabel")(attr)
    val meta = ir.Metadata(originalLabel = origLbl, address = addr)
    ir.dsl.block(unsigilBlock(x.blockident_), ss: _*).copy(meta = meta)

  override def visit(x: syntax.ProcDef_Empty, arg: A) = Nil
  override def visit(x: syntax.ProcDef_Some, arg: A) =
    x.listblock_.asScala.map(_.accept(this, arg)).toList

  private inline def cannotVisitDeclaration(x: HasParsePosition) =
    throw new Exception(
      "this declaration visit method cannot be called on this visitor. it should be handled by another visitor."
    )

}

/**
 * Entry-point for the main visitor, parsing everything from the procedure
 * level and lower, given the pre-parsed global declarations.
 *
 * The method makeBlockVisitor param is used to construct the parser for the blocks
 * and inner structures. The default implementation constructs a
 * [[ir.parsing.InnerBasilBNFCVisitor]].
 *
 * @group mainvisitor
 */
case class BasilMainBNFCVisitor[A](var decls: Declarations)
    extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Module.Visitor[util.IRContext, A]
    with syntax.Decl.Visitor[Option[ir.dsl.EventuallyProcedure], A]
    with AttributeListBNFCVisitor[A] {

  protected def makeBlockVisitor(
    s: String,
    decls: Declarations
  ): syntax.ProcDef.Visitor[Seq[ir.dsl.EventuallyBlock], A] =
    BlockBNFCVisitor[A](s, decls)

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.Decl_Proc, arg: A) = {
    val procName = unsigilProc(x.procident_)
    val blockvis = makeBlockVisitor(procName, decls)
    val blocks = x.procdef_.accept(blockvis, arg)

    val attr = parseAttrMap(x.attribset_, arg)
    val addr = getAddrAttr(attr)
    val rname = getStrAttr("name")(attr).getOrElse(procName)

    val formalIns = decls.formalIns(procName)
    val formalOuts = decls.formalOuts(procName)
    val procSpec = decls.procSpecs(procName)

    // TODO: use invariants
    val _ = procSpec.invariant

    Some(ir.dsl.EventuallyProcedure(
      label = rname,
      in = formalIns,
      out = formalOuts,
      blocks = blocks,
      entryBlockLabel = None,
      returnBlockLabel = None,
      address = addr,
      requires = procSpec.require,
      ensures = procSpec.ensure
    ))
  }

  override def visit(x: syntax.Decl_SharedMem, arg: A): None.type = None
  override def visit(x: syntax.Decl_UnsharedMem, arg: A): None.type = None
  override def visit(x: syntax.Decl_Var, arg: A): None.type = None

  override def visit(x: syntax.Decl_UninterpFun, arg: A): None.type = None
  override def visit(x: syntax.Decl_Fun, arg: A): None.type = None

  override def visit(x: syntax.Decl_Axiom, arg: A): None.type = None

  override def visit(x: syntax.Decl_ProgEmpty, arg: A): None.type = None
  override def visit(x: syntax.Decl_ProgWithSpec, arg: A): None.type = None

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A) = {

    val procs = x.listdecl_.asScala.flatMap {
      case x => x.accept(this, arg).map(p => p.name -> p)
    }.to(ListMap)

    val progSpec = decls.progSpec
    val mainProcName = progSpec.mainProc.getOrElse(procs.head._1)
    val initialMemory = progSpec.initialMemory.map(_.toMemorySection)

    val mainProcDef = procs(mainProcName)
    val otherProcs = procs.filter { (k,_) => k != mainProcName }

    val prog = ir.dsl.EventuallyProgram(mainProcDef, otherProcs.values, initialMemory)

    util.IRContext(
      List(),
      decls.symtab.externalFunctions,
      decls.symtab.globals,
      decls.symtab.funcEntries,
      decls.symtab.globalOffsets,
      util.IRLoading.emptySpecification(decls.symtab.globals),
      prog.resolve
    )

  }

}

object ParseBasilIL {
  def loadILReader(reader: Reader) = {
    val timer = PerformanceTimer("ParseBasilIL", LogLevel.DEBUG)

    val lexer = new basil_ir.Yylex(reader);
    timer.checkPoint("lexed")
    val parser = new basil_ir.parser(lexer, lexer.getSymbolFactory());

    val ast = parser.pModule()
    timer.checkPoint("parsed")

    val vis0 = BasilEarlyBNFCVisitor[Unit]()
    val decls = ast.accept(vis0, ())
    timer.checkPoint("early visitor")
    // Logger.debug(decls)

    val vis = BasilMainBNFCVisitor[Unit](decls)
    val result = ast.accept(vis, ())
    timer.checkPoint("main visitor & resolve")
    result
  }

  def loadILFile(filePath: String): util.IRContext = {
    val reader = new FileReader(filePath)
    loadILReader(reader)
  }

  def loadILString(text: String): util.IRContext = {
    val reader = new StringReader(text)
    loadILReader(reader)
  }

}
