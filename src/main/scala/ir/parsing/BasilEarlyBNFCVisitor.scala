package ir.parsing

import basil_ir.{Absyn => syntax}

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

private object Declarations {
  lazy val empty = Declarations(Map(), Map(), Map(), Map())
}

/**
 * Container for the result of the [[ir.parsing.BasilEarlyBNFCVisitor]].
 * Stores global variable declarations, memory region declarations, procedure signatures,
 * and metadata.
 *
 * Note that the [[ir.dsl.EventuallyProcedure]] structures stored by this class are
 * *incomplete*. Only the procedure name and the formalIn/Out parameters should be used.
 */
case class Declarations(
  val globals: Map[String, ir.Register],
  val memories: Map[String, ir.Memory],
  val procedures: Map[String, ir.dsl.EventuallyProcedure],
  val metas: Map[String, String]
) {
  private def ensureDisjoint[T, U](x: Map[T, U], y: Map[T, U]): Unit =
    val overlap = x.keySet.intersect(y.keySet)
    if (!overlap.isEmpty) {
      throw new IllegalArgumentException(
        "invalid attempt to merge non-disjoint declarations. repeated names: " + overlap
      )
    }

  @throws[IllegalArgumentException]("if the two Declarations have overlapping names")
  def merge(other: Declarations) = {
    ensureDisjoint(globals, other.globals)
    ensureDisjoint(memories, other.memories)
    ensureDisjoint(procedures, other.procedures)
    ensureDisjoint(metas, other.metas)
    Declarations(
      globals ++ other.globals,
      memories ++ other.memories,
      procedures ++ other.procedures,
      metas ++ other.metas
    )
  }
}

/**
 * Performs an initial pass to read global declarations. Importantly, this picks up
 * procedure declarations and their paramter lists, so a later pass can correctly map a
 * direct call's actual arguments to their formal parameters.
 *
 * Visiting a [[basil_ir.Absyn.Prog]] with this visitor will return the complete declarations
 * object for the entire program. Using the other visit methods will only return declarations
 * for that subset of the AST.
 */
case class BasilEarlyBNFCVisitor[A]()
    extends syntax.Program.Visitor[Declarations, A],
      syntax.Declaration.Visitor[Declarations, A],
      syntax.MExpr.Visitor[String, A],
      syntax.Params.Visitor[ir.LocalVar, A],
      TypesBNFCVisitor[A] {

  // Members declared in Program.Visitor
  override def visit(x: syntax.Prog, arg: A) =
    x.listdeclaration_.asScala.foldLeft(Declarations.empty) { case (decls, x) =>
      try {
        decls.merge(x.accept(this, arg))
      } catch {
        case e: IllegalArgumentException =>
          throw ParseException(
            "encountered duplicate declarations with the same name",
            x.asInstanceOf[HasParsePosition],
            e
          )
      }
    }

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
    ir.LocalVar.ofIndexed(x.bident_, x.type_.accept(this, arg))

  private def visitParams(x: syntax.ListParams, arg: A): Map[String, ir.IRType] =
    // NOTE: uses ListMap instead of SortedMap, because the orders are presumed to
    // match between calls and param lists within the same file.
    x.asScala.map(_.accept(this, arg)).map(x => x.name -> x.getType).to(ListMap)

  override def visit(x: syntax.Procedure, arg: A) =
    val inparams = visitParams(x.listparams_1, arg)
    val outparams = visitParams(x.listparams_2, arg)

    val proc = ir.dsl.EventuallyProcedure(x.bident_, inparams, outparams, Nil)
    Declarations.empty.copy(procedures = Map(proc.label -> proc))
}
