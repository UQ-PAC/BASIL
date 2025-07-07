package ir.parsing

import basil_ir.Absyn as syntax

import scala.jdk.CollectionConverters.*

type ParsedStatement = ir.dsl.NonCallStatement | ir.dsl.EventuallyCall | ir.dsl.EventuallyIndirectCall

/**
 * Parses structures at the block-level and lower, given
 * the current procedure name and the parsed global declarations.
 * The procedure name is used to look up out parameters for the return
 * statement, and other declarations are used to look up parameters
 * for call statements.
 *
 * See also: [[ExprBNFCVisitor]] for the expressions visitor.
 *
 * @group mainvisitor
 */
class BlockBNFCVisitor[A](val procName: String, private val _decls: Declarations)
    extends ExprBNFCVisitor[A](_decls)
    with syntax.Block.Visitor[ir.dsl.EventuallyBlock, A]
    with syntax.ProcDef.Visitor[Seq[ir.dsl.EventuallyBlock], A]
    with syntax.Stmt.Visitor[ParsedStatement, A]
    with syntax.StmtWithAttrib.Visitor[ParsedStatement, A]
    with syntax.Jump.Visitor[ir.dsl.EventuallyJump, A]
    with syntax.JumpWithAttrib.Visitor[ir.dsl.EventuallyJump, A]
    with syntax.Assignment.Visitor[(ir.Variable, ir.Expr), A]
    with syntax.LVars.Visitor[List[ir.Variable], A]
    with syntax.LVar.Visitor[ir.Variable, A]
    with AttributeListBNFCVisitor[A] {

  val procSpec = decls.procSpecs(procName)
  val formalOuts = decls.formalOuts(procName)

  def blocks(x: syntax.ListBlock, arg: A): List[ir.dsl.EventuallyBlock] =
    x.asScala.map(_.accept(this, arg)).toList

  def stmts(x: syntax.ListStmtWithAttrib, arg: A): List[ir.dsl.DSLStatement] =
    x.asScala.map(_.accept(this, arg)).toList

  // Members declared in Block.Visitor
  override def visit(x: syntax.Block1, arg: A): ir.dsl.EventuallyBlock =
    val ss = stmts(x.liststmtwithattrib_, arg)
    val jump = x.jumpwithattrib_.accept(this, arg)
    val body = ss :+ jump

    val attr = x.attribset_.accept(this, arg)
    val addr = attr.getInt("address")
    val origLbl = attr.getString("originalLabel")
    val meta = ir.Metadata(originalLabel = origLbl, address = addr)

    ir.dsl.block(unsigilBlock(x.blockident_), body: _*).copy(meta = meta)

  private def stmtAttrs(x: syntax.AttribSet, arg: A) =
    val attrs = x.accept(this, arg)
    (attrs.getString("label"), attrs.getString("comment"))

  override def visit(x: syntax.StmtWithAttrib1, arg: A) =
    val (label, comm) = stmtAttrs(x.attribset_, arg)
    x.stmt_.accept(this, arg) match {
      case s: ir.dsl.EventuallyCall => s.copy(label = label, comment = comm)
      case s: ir.dsl.EventuallyIndirectCall => s.copy(label = label, comment = comm)
      case s: ir.Command => s.setLabel(label).setComment(comm)
    }

  override def visit(x: syntax.JumpWithAttrib1, arg: A) =
    val (label, comm) = stmtAttrs(x.attribset_, arg)
    x.jump_.accept(this, arg) match {
      case s: ir.dsl.EventuallyGoto => s.copy(label = label, comment = comm)
      case s: ir.dsl.EventuallyReturn => s.copy(label = label, comment = comm)
      case s: ir.dsl.EventuallyUnreachable => s.copy(label = label, comment = comm)
    }

  override def visit(x: syntax.ProcDef_Empty, arg: A) = Nil
  override def visit(x: syntax.ProcDef_Some, arg: A) = blocks(x.listblock_, arg)

  // Members declared in Statement.Visitor
  override def visit(x: syntax.Assignment1, arg: A): (ir.Variable, ir.Expr) =
    ((x.lvar_.accept(this, arg)), x.expr_.accept(this, arg))

  override def visit(x: syntax.Stmt_SingleAssign, arg: A) =
    val assign = x.assignment_.accept(this, arg)
    ir.LocalAssign(assign._1, assign._2)

  override def visit(x: syntax.Stmt_MultiAssign, arg: A) =
    val assigns = x.listassignment_.asScala.map(_.accept(this, arg))
    ir.SimulAssign(assigns.to(Vector))

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

    ir.dsl.directCall(formalOuts.keys.zip(outs), unsigilProc(x.procident_), formalIns.keys.zip(ins))

  override def visit(x: syntax.Stmt_IndirectCall, arg: A) = {
    val v = x.expr_.accept(this, arg) match {
      case v: ir.Variable => v
      case o => throw Exception("Indirect call target must be a variable")
    }
    ir.dsl.indirectCall(v)
  }

  override def visit(x: syntax.Stmt_Assume, arg: A) =
    ir.Assume(x.expr_.accept(this, arg), None, None, false)

  override def visit(x: syntax.Stmt_Guard, arg: A) =
    ir.Assume(x.expr_.accept(this, arg), None, None, true)

  override def visit(x: syntax.Stmt_Assert, arg: A) =
    ir.Assert(x.expr_.accept(this, arg), None, None)

  // Members declared in Jump.Visitor
  override def visit(x: syntax.Jump_GoTo, arg: A) =
    val ids = x.listblockident_.asScala.toSeq.map(unsigilBlock)
    ir.dsl.goto(ids: _*)
  override def visit(x: syntax.Jump_Unreachable, arg: A) = ir.dsl.unreachable
  override def visit(x: syntax.Jump_Return, arg: A) =
    val es = exprs(x.listexpr_, arg)
    ir.dsl.ret(formalOuts.keys.zip(es).toList: _*)

  // Members declared in LVar.Visitor
  override def visit(x: syntax.LVar_Local, arg: A) = x.localvar_.accept(this, arg)
  override def visit(x: syntax.LVar_Global, arg: A) = x.globalvar_.accept(this, arg)

  // Members declared in CallLVars.Visitor
  override def visit(x: syntax.LVars_Empty, arg: A): Nil.type = Nil
  override def visit(x: syntax.LVars_LocalList, arg: A) =
    x.listlocalvar_.asScala.toList.map(_.accept(this, arg))
  override def visit(x: syntax.LVars_List, arg: A) =
    x.listlvar_.asScala.toList.map(_.accept(this, arg))

}
