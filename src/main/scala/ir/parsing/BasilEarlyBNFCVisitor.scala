package ir.parsing
import ir.Sigil

import basil_ir.{Absyn => syntax}

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

private object Declarations {
  lazy val empty = Declarations(Map(), Map(), Map(), Map())
}

enum Attrib {
  case ValueAttr(name: String, v: ir.Literal)
  case StringAttr(name: String, v: String)
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


trait AttributeListBNFCVisitor[A]()
    extends syntax.AttributeItem.Visitor[Attrib, A],
      syntax.AttrDefList.Visitor[List[Attrib], A],
      LiteralsBNFCVisitor[A] {

  override def visit(p: syntax.AttrDefListSome, arg: A) = {
    p.listattributeitem_.asScala.toList.map(_.accept(this, arg)) 
  }

  override def visit(p: syntax.ValueAttr , arg: A) : Attrib = Attrib.ValueAttr(p.bident_.stripPrefix(Sigil.BASIR.attrib), p.value_.accept(this, arg))
  override def visit(p: syntax.StringAttr , arg: A) : Attrib = Attrib.StringAttr(p.bident_.stripPrefix(Sigil.BASIR.attrib), unquote(p.str_, p))
  override def visit(p: syntax.AttrDefListEmpty, arg: A) : List[Attrib] = List()

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
    extends syntax.Module.Visitor[Declarations, A],
      syntax.Declaration.Visitor[Declarations, A],
      syntax.ProcDef.Visitor[Declarations, A],
      syntax.ProcSig.Visitor[Declarations, A],
      syntax.Params.Visitor[ListMap[String, ir.IRType], A],
      TypesBNFCVisitor[A], AttributeListBNFCVisitor[A] {

  override def visit(x: syntax.Module1, arg: A) =
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


  // Members declared in Declaration.Visitor
  override def visit(x: syntax.ProgDecl, arg: A) = Declarations.empty
  override def visit(x: syntax.ProgDeclSpec, arg: A) = Declarations.empty

  override def visit(x: syntax.MemDecl, arg: A) =

    // TODO: make this narrower in the grammar

    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = x.globalident_ match {
      case "stack" => ir.StackMemory(x.globalident_.stripPrefix(Sigil.BASIR.globalVar), addrwd, valwd)
      case _ => ir.SharedMemory(x.globalident_.stripPrefix(Sigil.BASIR.globalVar), addrwd, valwd)
    }
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  override def visit(x: syntax.VarDecl, arg: A) =
    val v = ir.Register(x.globalident_.stripPrefix(Sigil.BASIR.globalVar), x.type_.accept(this, arg).asInstanceOf[ir.BitVecType].size)
    Declarations.empty.copy(globals = Map(v.name -> v))

  override def visit(x: syntax.Param, arg: A): ir.LocalVar =
    ir.LocalVar.ofIndexed(x.localident_.stripPrefix(Sigil.BASIR.localVar), x.type_.accept(this, arg))

  private def visitParams(x: syntax.ListParams, arg: A): Map[String, ir.IRType] = {
    // NOTE: uses ListMap instead of SortedMap, because the orders are presumed to
    // match between calls and param lists within the same file.
    x.asScala.map(_.accept(this, arg)).map(x => x.name -> x.getType).to(ListMap)
  }

  override def visit(x: syntax.ProcedureSig, arg: A) = {
    val name = x.procident_.stripPrefix(Sigil.BASIR.proc)
    val inparams = visitParams(x.listparams_1, arg)
    val outparams = visitParams(x.listparams_2, arg)
    val proc = ir.dsl.EventuallyProcedure(name, inparams, outparams, Nil)
    Declarations.empty.copy(procedures = Map(name -> proc))
  }

  override def visit(x: syntax.Procedure, arg: A) = x.accept(this, arg)
}
