package ir.parsing

import basil_ir.Absyn as syntax
import ir.Sigil
import util.{LogLevel, PerformanceTimer}

import java.io.{FileReader, Reader, StringReader}
import scala.jdk.CollectionConverters.*

def unsigilBlock(x: String) = Sigil.unsigil(Sigil.BASIR.block)(x)
def unsigilProc(x: String) = Sigil.unsigil(Sigil.BASIR.proc)(x)
def unsigilLocal(x: String) =
  /* keep sigil as it is optional, to preserve var name */
  x
def unsigilGlobal(x: String) = Sigil.unsigil(Sigil.BASIR.globalVar)(x)
def unsigilAttrib(x: String) = Sigil.unsigil(Sigil.BASIR.attrib)(x)

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
    with syntax.IntType.Visitor[ir.IRType, A]
    with syntax.BoolType.Visitor[ir.IRType, A]
    with syntax.MapType.Visitor[ir.IRType, A]
    with syntax.BVType.Visitor[ir.IRType, A]
    with syntax.Type.Visitor[ir.IRType, A]
    with syntax.Endian.Visitor[ir.Endian, A]
    with syntax.Assignment.Visitor[(ir.Variable, ir.Expr), A]
    with syntax.LocalVar.Visitor[ir.LocalVar, A]
    with syntax.GlobalVar.Visitor[ir.GlobalVar, A]
    with syntax.LVar.Visitor[ir.Variable, A]
    with syntax.LVars.Visitor[List[ir.Variable], A]
    with syntax.Jump.Visitor[ir.dsl.EventuallyJump, A]
    with syntax.Expr.Visitor[ir.Expr, A]
    with syntax.LambdaDef.Visitor[ir.LambdaExpr, A]
    with syntax.Stmt.Visitor[ir.dsl.DSLStatement, A]
    with syntax.Block.Visitor[ir.dsl.EventuallyBlock, A]
    with syntax.ProcDef.Visitor[List[ir.dsl.EventuallyBlock], A]
    with syntax.FunSpec.Visitor[FunSpec, A]
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

  def stmts(x: syntax.ListStmt, arg: A): List[ir.dsl.DSLStatement] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Type.Visitor
  override def visit(x: syntax.TypeIntType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeBoolType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeMapType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  override def visit(x: syntax.TypeBVType, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in BoolType.Visitor
  override def visit(x: syntax.BoolType1, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in MapType.Visitor
  override def visit(x: syntax.MapType1, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in IntType.Visitor
  override def visit(x: syntax.IntType1, arg: A) = x.accept(typesVisitor, arg): ir.IRType
  // Members declared in BVType.Visitor
  override def visit(x: syntax.BVType1, arg: A) = x.accept(typesVisitor, arg): ir.IRType

  override def visit(x: syntax.Expr_Local, arg: A) = x.localvar_.accept(this, arg)

  /* Members declared in Expr.Visitor */

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
      case Some(v) => ir.curryFunctionType(v.irType)._2
      case None => throw Exception(s"Undeclared function: ${x.globalident_}")
    }
    val args = x.listexpr_.asScala.toSeq.map(_.accept(this, arg))
    ir.FApplyExpr(n, args, rt)
  }
  override def visit(x: syntax.LocalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
    // is localvar
    val ty = x.type_.accept(this, arg)
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), ty)
  }
  override def visit(x: syntax.GlobalVar1, arg: A) = {
    // handle registers which are declared in the global scope. everything else
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
    val proc = decls.procedures(unsigilProc(x.procident_))
    ir.dsl.directCall(
      proc.out.keys.zip(outs).toList,
      unsigilProc(x.procident_),
      // TODO: fix var names. in vars need to be obtained from proc definition??
      proc.in.keys.zip(ins).toList
    )
  override def visit(x: syntax.Stmt_IndirectCall, arg: A) = {
    val v = x.expr_.accept(this, arg) match {
      case v: ir.Variable => v
      case o => throw Exception("Cannot make indir call target arbitrary expression")
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
    val proc = decls.procedures(procName)
    ir.dsl.ret(proc.out.keys.zip(es).toList: _*)

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

  // Members declared in FunSpec.Visitor
  override def visit(x: syntax.FunSpec_Require, arg: A): FunSpec =
    FunSpec(require = List(x.expr_.accept(this, arg)))
  override def visit(x: syntax.FunSpec_Ensure, arg: A): FunSpec =
    FunSpec(ensure = List(x.expr_.accept(this, arg)))
  override def visit(x: syntax.FunSpec_Invariant, arg: A): FunSpec =
    val bl = unsigilBlock(x.blockident_)
    val e = x.expr_.accept(this, arg)
    FunSpec(invariant = Map(bl -> List(e)))

  override def visit(x: syntax.ProcDef_Empty, arg: A) = Nil
  override def visit(x: syntax.ProcDef_Some, arg: A) =
    x.listblock_.asScala.map(_.accept(this, arg)).toList

  private inline def cannotVisitDeclaration(x: HasParsePosition) =
    throw new Exception(
      "this declaration visit method cannot be called on this visitor. it should be handled by another visitor."
    )

  // Members declared in Params.Visitor
  // override def visit(x: syntax.Param, arg: A): Nothing = cannotVisitDeclaration(x)

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
  var decls: Declarations,
  val makeVisitor: (String, Declarations) => InnerBasilBNFCVisitor[A] = InnerBasilBNFCVisitor[A](_, _)
) extends LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with syntax.Module.Visitor[util.IRContext, A]
    with syntax.Decl.Visitor[ir.dsl.EventuallyProcedure, A]
    with syntax.Params.Visitor[ir.LocalVar, A]
    with AttributeListBNFCVisitor[A]
    // with syntax.ProgSpec.Visitor[Unit, A]
    {

  def params(x: syntax.ListParams, arg: A): List[ir.Variable] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Params.Visitor
  override def visit(x: syntax.Params1, arg: A) =
    ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), x.type_.accept(this, arg))

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.Decl_SharedMem, arg: A): Nothing = throw new Exception(
    "MemDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.Decl_UnsharedMem, arg: A): Nothing = throw new Exception(
    "MemDecl should be visited by an earlier visitor"
  )
  override def visit(x: syntax.Decl_Var, arg: A): Nothing = throw new Exception(
    "VarDecl should be visited by an earlier visitor"
  )

  def visit(x: syntax.Decl_UninterpFun, arg: A): ir.dsl.EventuallyProcedure = ???
  def visit(x: syntax.Decl_Fun, arg: A): ir.dsl.EventuallyProcedure = ???

  // Members declared in ProcDef.Visitor
  override def visit(x: syntax.Decl_Proc, arg: A) = {
    val procName = unsigilProc(x.procident_)
    val innervis = makeVisitor(procName, decls)
    val blocks = x.procdef_.accept(innervis, arg)

    val spec = x.listfunspec_.asScala.foldLeft(FunSpec()) { case (acc, sp) =>
      acc.merge(sp.accept(innervis, arg))
    }

    val attr = parseAttrMap(x.attribset_, arg)
    val addr = getAddrAttr(attr)
    val rname = getStrAttr("name")(attr).getOrElse(procName)

    val p = decls.procedures(procName).copy(blocks = blocks, requires = spec.require, ensures = spec.ensure)
    p.copy(address = addr, label = rname)
  }

  def visit(x: syntax.Decl_Axiom, arg: A): ir.dsl.EventuallyProcedure = ???
  def visit(x: syntax.Decl_ProgWithSpec, arg: A): ir.dsl.EventuallyProcedure = ???
  def visit(x: syntax.Decl_ProgEmpty, arg: A): ir.dsl.EventuallyProcedure = ???

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A) = {

    val ds = x.listdecl_.asScala.filter {
      case x: syntax.Decl_UnsharedMem => false
      case x: syntax.Decl_SharedMem => false
      case x: syntax.Decl_Var => false
      case x: syntax.Decl_Proc => true
      case x: syntax.Decl_ProgWithSpec => true
      case x: syntax.Decl_ProgEmpty => true
      case x: syntax.Decl_UninterpFun => true
      case x: syntax.Decl_Fun => true
      case o => throw Exception("parsed decls contains unhandled type: " + o)
    }

    var initialMemory = Set[ir.MemorySection]()

    def procProgAttrs(attrs: Attrib) = {
      val attrMap = attrs.Map.get
      val newInitialMemory = attrMap
        .get("initial_memory")
        .flatMap(_.List)
        .map(_.map(v =>
          val static = MemoryAttribData
            .fromAttrib(v)
            .getOrElse(throw Exception(s"Ill formed memory section ${v.pprint}"))
          static.toMemorySection
        ).toSet)
      val ctx = for {
        a <- attrMap.get("symbols")
        nctx <- decls.symtab.mergeFromAttrib(a)
      } yield (nctx)
      for (c <- ctx) {
        decls = decls.copy(symtab = c)
      }
      for (n <- newInitialMemory) {
        initialMemory = n
      }
    }

    var mainProc: Option[String] = None

    ds.foreach {
      case d: syntax.Decl_ProgWithSpec => {
        val attrs = parseAttrMap(d.attribset_, arg)
        procProgAttrs(attrs)
        mainProc = Some((unsigilProc(d.procident_)))
      }
      case d: syntax.Decl_ProgEmpty => {
        val attrs = parseAttrMap(d.attribset_, arg)
        procProgAttrs(attrs)
        mainProc = Some((unsigilProc(d.procident_)))
      }
      case _ => ()
    }

    val entryname: String = mainProc.getOrElse(throw Exception("No main proc specified"))

    val procs = x.listdecl_.asScala.collect { case p: syntax.Decl_Proc =>
      p.accept(this, arg)
    }

    val (mainProcDef, otherProcs) = procs.partition(_.name == entryname)
    if (mainProcDef.headOption.isEmpty) {
      throw Exception(s"Entry procedure not found: \"${entryname}\"\n${procs.map(_.name).mkString("\n")}")
    }
    val resolvedProg = ir.dsl.EventuallyProgram(mainProcDef.head, otherProcs, initialMemory).resolve

    util.IRContext(
      List(),
      decls.symtab.externalFunctions,
      decls.symtab.globals,
      decls.symtab.funcEntries,
      decls.symtab.globalOffsets,
      util.IRLoading.emptySpecification(decls.symtab.globals),
      resolvedProg
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
