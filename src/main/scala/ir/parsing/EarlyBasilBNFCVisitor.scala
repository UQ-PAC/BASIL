package ir.parsing

import basil_ir.{Absyn => syntax}

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

object Declarations {
  lazy val empty = Declarations(Map(), Map(), Map(), Map())
}

case class Declarations(
  val globals: Map[String, ir.Register],
  val memories: Map[String, ir.Memory],
  val procedures: Map[String, ir.dsl.EventuallyProcedure],
  val metas: Map[String, String]
) {
  private def isDisjoint[T, U](x: Map[T, U], y: Map[T, U]) =
    x.keySet.intersect(y.keySet).isEmpty

  def merge(other: Declarations) = {
    assert(isDisjoint(globals, other.globals), "attempt to merge non-disjoint declarations")
    assert(isDisjoint(memories, other.memories), "attempt to merge non-disjoint declarations")
    assert(isDisjoint(procedures, other.procedures), "attempt to merge non-disjoint declarations")
    assert(isDisjoint(metas, other.metas), "attempt to merge non-disjoint declarations")
    Declarations(
      globals ++ other.globals,
      memories ++ other.memories,
      procedures ++ other.procedures,
      metas ++ other.metas
    )
  }
}

case class EarlyBasilBNFCVisitor[A]()
    extends syntax.Program.Visitor[Declarations, A],
      syntax.Declaration.Visitor[Declarations, A],
      syntax.MExpr.Visitor[String, A],
      syntax.Params.Visitor[ir.LocalVar, A],
      TypesBNFCVisitor[A] {

  // Members declared in Program.Visitor
  override def visit(x: syntax.Prog, arg: A) =
    x.listdeclaration_.asScala.map(_.accept(this, arg)).foldLeft(Declarations.empty)((x, y) => x.merge(y))

  // Members declared in MExpr.Visitor
  override def visit(x: syntax.MSym, arg: A) = x.bident_
  override def visit(x: syntax.BlockM, arg: A) =
    throw ParseException("block literal as a metadata value is unsupported", x)

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.LetDecl, arg: A) =
    val allowedMetadataKeys = List("entry_procedure")
    val key = x.bident_ match {
      case x if allowedMetadataKeys.contains(x) => x
      case key => throw ParseException("unsupported 'let' metadata key: " + key, x)
    }
    Declarations.empty.copy(metas = Map(key -> x.mexpr_.accept(this, arg)))

  override def visit(x: syntax.MemDecl, arg: A) =

    // TODO: make this narrower in the grammar

    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = x.bident_ match {
      case "stack" => ir.StackMemory(x.bident_, addrwd, valwd)
      case _ => ir.SharedMemory(x.bident_, addrwd, valwd)
    }
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  override def visit(x: syntax.VarDecl, arg: A) =
    val v = ir.Register(x.bident_, x.type_.accept(this, arg).asInstanceOf[ir.BitVecType].size)
    Declarations.empty.copy(globals = Map(v.name -> v))

  override def visit(x: syntax.Param, arg: A): ir.LocalVar =
    ir.LocalVar(x.bident_, x.type_.accept(this, arg))

  private def visitParams(x: syntax.ListParams, arg: A): Map[String, ir.IRType] =
    // XXX: use ListMap instead of SortedMap, because the orders are presumed to
    // match between calls and param lists within the same file.
    x.asScala.map(_.accept(this, arg)).map(x => x.name -> x.getType).to(ListMap)

  override def visit(x: syntax.Procedure, arg: A) =
    val inparams = visitParams(x.listparams_1, arg)
    val outparams = visitParams(x.listparams_2, arg)

    val proc = ir.dsl.EventuallyProcedure(x.bident_, inparams, outparams, Nil)
    Declarations.empty.copy(procedures = Map(proc.label -> proc))
}
