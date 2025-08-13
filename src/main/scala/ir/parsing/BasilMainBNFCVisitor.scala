package ir.parsing

import basil_ir.Absyn as syntax

import scala.collection.immutable.TreeSeqMap
import scala.jdk.CollectionConverters.*

/**
 * Entry-point for the main visitor, parsing everything from the procedure
 * level and lower, given the pre-parsed global declarations.
 *
 * The [[makeBlockVisitor]] method is called to construct a procesure-specific
 * [[BlockBNFCVisitor]] for each visited procedure. This block visitor is used
 * to parse the blocks and statements.
 *
 * @group mainvisitor
 */
class BasilMainBNFCVisitor[A](var decls: Declarations)
    extends syntax.Module.Visitor[ir.dsl.EventuallyProgram, A]
    with syntax.Decl.Visitor[Option[ir.dsl.EventuallyProcedure], A]
    with LiteralsBNFCVisitor[A]
    with TypesBNFCVisitor[A]
    with AttributeListBNFCVisitor[A] {

  def makeBlockVisitor(s: String): syntax.ProcDef.Visitor[Seq[ir.dsl.EventuallyBlock], A] =
    BlockBNFCVisitor[A](s, decls)

  /**
   * Parses a procedure declaration, delegating to a procedure-specific [[BlockBNFCVisitor]]
   * to parse the statements within the procedure.
   */
  override def visit(x: syntax.Decl_Proc, arg: A): Some[ir.dsl.EventuallyProcedure] = {
    val procName = unsigilProc(x.procident_)
    val blockvis = makeBlockVisitor(procName)
    val blocks = x.procdef_.accept(blockvis, arg)

    val attr = x.attribset_.accept(this, arg)
    val addr = attr.getInt("address")
    val rname = attr.getString("name").getOrElse(procName)
    val retlabel = attr.getString("returnBlock")

    val formalIns = decls.formalIns(procName)
    val formalOuts = decls.formalOuts(procName)
    val procSpec = decls.procSpecs(procName)

    // TODO: use invariants
    val _ = procSpec.invariant

    Some(
      ir.dsl.EventuallyProcedure(
        label = rname,
        in = formalIns,
        out = formalOuts,
        blocks = blocks,
        entryBlockLabel = None,
        returnBlockLabel = retlabel,
        address = addr,
        requires = procSpec.require,
        ensures = procSpec.ensure
      )
    )
  }

  // Members declared in Program.Visitor
  override def visit(x: syntax.Module1, arg: A) = {

    val procs = x.listdecl_.asScala
      .flatMap { case x =>
        x.accept(this, arg).map(p => p.name -> p)
      }
      .to(TreeSeqMap)

    val progSpec = decls.progSpec

    lazy val defaultProc = procs.values.headOption.getOrElse {
      throw ParseException("module must contain at least one procedure", x)
    }

    val mainProc = progSpec.mainProc.fold(defaultProc)(procs(_))
    val otherProcs = procs.view.values.filter(_ ne mainProc)

    val initialMemory = progSpec.initialMemory.map(_.toMemorySection)

    ir.dsl.EventuallyProgram(mainProc, otherProcs, initialMemory)
  }

  override def visit(x: syntax.Decl_SharedMem, arg: A): None.type = None
  override def visit(x: syntax.Decl_UnsharedMem, arg: A): None.type = None
  override def visit(x: syntax.Decl_Var, arg: A): None.type = None

  override def visit(x: syntax.Decl_UninterpFun, arg: A): None.type = None
  override def visit(x: syntax.Decl_Fun, arg: A): None.type = None

  override def visit(x: syntax.Decl_Axiom, arg: A): None.type = None

  override def visit(x: syntax.Decl_ProgEmpty, arg: A): None.type = None
  override def visit(x: syntax.Decl_ProgWithSpec, arg: A): None.type = None
}
