package ir.parsing

import basil_ir.Absyn as syntax

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

/**
 * Performs an initial pass to read global declarations. Importantly, this picks up
 * procedure declarations and their paramter lists, so a later pass can correctly map a
 * direct call's actual arguments to their formal parameters.
 *
 * The entry point for this visitor is the [[basil_ir.Absyn.Module]] method. When visiting
 * a [[basil_ir.Absyn.Module]] with this visitor, this will return the complete declarations
 * object for the entire program. Using the other visit methods will only return declarations
 * for that subset of the AST.
 *
 * Within this visitor, there are two phases. The first phase parses all declarations,
 * but skips expressions which might refer to other declarations. The second phase then
 * parses all expressions occuring within declarations. This allows for forward declarations
 * to work correctly. The second phase is implemented by the inner class [[DeclsExprVisitor]].
 */
class BasilEarlyBNFCVisitor[A]()
    extends syntax.Module.Visitor[Declarations, A],
      syntax.Decl.Visitor[Declarations, A],
      syntax.Params.Visitor[(String, ir.IRType), A],
      AttributeListBNFCVisitor[A] {

  /**
   * Primary entry point for [[BasilEarlyBNFCVisitor]].
   * This will correctly invoke the two phases as described in this class's documentation
   * The returned declarations will be correctly populated, including expression predicates.
   */
  override def visit(x: syntax.Module1, arg: A) =
    val listdecl = x.listdecl_.asScala

    // phase 1: initial parse of declarations
    var initialDecls = listdecl.foldLeft(Declarations.empty) { case (decls, x) =>
      val newdecls = x.accept(this, arg)
      try {
        decls.merge(newdecls)
      } catch {
        case e: IllegalArgumentException =>
          throw ParseException(
            "encountered duplicate declarations with the same name",
            x.asInstanceOf[HasParsePosition],
            e
          )
      }
    }

    // phase 2: visit expressions appearing in declarations
    val exprvis = makeExprVisitor(initialDecls)

    // XXX: be aware of foldRight and merge order. foldRight means that to keep the original order,
    // the folding function should /prepend/ to the accumulator.
    listdecl.foldRight(initialDecls)((x, decls) =>
      x match {
        // parse program specifications (rely / guarantee) and update decls.
        case x: syntax.Decl_ProgWithSpec =>
          val spec = exprvis.visitProgSpec(x, arg)
          Declarations.empty.copy(progSpec = spec).merge(decls)

        // parse function definition bodies and update decls.
        case x: syntax.Decl_Fun =>
          val (nm, defn) = exprvis.visitFunDef(x, arg)
          decls.updateFunctionDefinition(nm, Some(defn))

        case x: syntax.Decl_Axiom =>
          val ax = exprvis.visitAxiom(x, arg)
          decls.copy(axioms = ax +: decls.axioms)

        case x: syntax.Decl_Proc =>
          val spec = exprvis.visitProcSpec(x, arg)
          decls.merge(Declarations.empty.copy(procSpecs = Map(spec)))

        // declarations which do not contain expressions
        case _ => decls
      }
    )

  // Members declared in Declaration.Visitor
  override def visit(x: syntax.Decl_UnsharedMem, arg: A) =
    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = ir.StackMemory(unsigilGlobal(x.globalident_), addrwd, valwd)
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  override def visit(x: syntax.Decl_SharedMem, arg: A) =
    val ir.MapType(ir.BitVecType(addrwd), ir.BitVecType(valwd)) = x.type_.accept(this, arg): @unchecked
    val mem = ir.SharedMemory(unsigilGlobal(x.globalident_), addrwd, valwd)
    Declarations.empty.copy(memories = Map(mem.name -> mem))

  override def visit(x: syntax.Decl_Var, arg: A) =
    val v = ir.GlobalVar(unsigilGlobal(x.globalident_), x.type_.accept(this, arg))
    Declarations.empty.copy(globals = Map(v.name -> v))

  override def visit(x: syntax.Params1, arg: A): (String, ir.IRType) =
    val lv = ir.LocalVar.ofIndexed(unsigilLocal(x.localident_), x.type_.accept(this, arg))
    lv.name -> lv.irType

  private def visitParamsList(x: syntax.ListParams, arg: A): ListMap[String, ir.IRType] = {
    x.asScala.toList.map(_.accept(this, arg)).to(ListMap)
  }

  override def visit(x: syntax.Decl_Proc, arg: A) = {
    val name = unsigilProc(x.procident_)
    val inparams = visitParamsList(x.listparams_1, arg)
    val outparams = visitParamsList(x.listparams_2, arg)
    Declarations.empty.copy(formalIns = Map(name -> inparams), formalOuts = Map(name -> outparams))
  }

  override def visit(x: syntax.Decl_ProgEmpty, arg: A) =
    parseProgDecl(x.procident_, x.attribset_, arg)

  override def visit(x: syntax.Decl_ProgWithSpec, arg: A) =
    parseProgDecl(x.procident_, x.attribset_, arg)

  private def parseProgDecl(sigilIdent: String, attribs: syntax.AttribSet, arg: A): Declarations = {
    val attrs = attribs.accept(this, arg)
    val attrMap = attrs.Map.get

    val initialMemory = attrMap
      .get("initial_memory")
      .map(_.List.getOrElse(throw Exception("initial_memory must be a list")))
      .map(
        _.map(v =>
          MemoryAttribData
            .fromAttrib(v)
            .getOrElse(throw Exception(s"Ill formed memory section ${v.pprint}"))
        ).toSet
      )

    val symtab = attrMap
      .get("symbols")
      .map(SymbolTableInfo.fromAttrib(_).getOrElse {
        throw Exception("invalid symbols format")
      })

    val mainProc = unsigilProc(sigilIdent)

    Declarations.empty.copy(
      progSpec = ProgSpec(mainProc = Some(mainProc), initialMemory = initialMemory.getOrElse(Set())),
      symtab = symtab.getOrElse(SymbolTableInfo())
    )
  }

  override def visit(x: syntax.Decl_UninterpFun, arg: A): ir.parsing.Declarations = {
    val n = unsigilGlobal(x.globalident_)
    val params = x.listtype_.asScala.view
      .map(_.accept(this, arg))
      .zipWithIndex
      .map { (ty, i) => ir.LocalVar(s"x$i", ty) }
      .toList

    val returnType = x.type_.accept(this, arg)

    val fun = ir.FunctionDecl(n, params, returnType, None)
    Declarations.empty.copy(functions = Map(n -> fun))
  }

  override def visit(x: syntax.Decl_Fun, arg: A): ir.parsing.Declarations = {
    val n = unsigilGlobal(x.globalident_)
    val params = visitParamsList(x.listparams_, arg).map { (n, ty) => ir.LocalVar(n, ty) }.toList
    val returnType = x.type_.accept(this, arg)

    val fun = ir.FunctionDecl(n, params, returnType, None)
    Declarations.empty.copy(functions = Map(n -> fun))
  }

  override def visit(x: syntax.Decl_Axiom, arg: A) =
    Declarations.empty

  def makeExprVisitor(decls: Declarations): DeclsExprVisitor =
    DeclsExprVisitor(decls)

  /**
   * A small second phase within the declarations visitor. This parses
   * declarations which contain *expressions*. Generally, expressions in declarations
   * can reference other declarations, so this visitor has to be run *after*
   * the initial enumeration of declarations.
   */
  class DeclsExprVisitor(val decls: Declarations)
      extends syntax.FunSpec.Visitor[FunSpec, A]
      with syntax.ProgSpec.Visitor[ProgSpec, A] {

    val exprvis: syntax.Expr.Visitor[ir.Expr, A] = ExprBNFCVisitor[A](decls)

    /**
     * Parses an axiom definition.
     */
    def visitAxiom(x: syntax.Decl_Axiom, arg: A) = {

      // TODO: use attribset for axiom declarations
      val _ = x.attribset_

      ir.AxiomDecl(x.expr_.accept(exprvis, arg))
    }

    /**
     * Parses the expression definition for a function with a body (not uninterpreted).
     */
    def visitFunDef(x: syntax.Decl_Fun, arg: A) = {
      val n = unsigilGlobal(x.globalident_)
      n -> x.expr_.accept(exprvis, arg)
    }

    /**
     * Parses the [[FunSpec]] (require/ensures) for a procedure.
     */
    def visitProcSpec(x: syntax.Decl_Proc, arg: A) = {
      val specs = x.listfunspec_.asScala.map(_.accept(this, arg))

      val name = unsigilProc(x.procident_)
      name -> specs.foldRight(FunSpec())(_ merge _)
    }

    // Members declared in FunSpec.Visitor
    override def visit(x: syntax.FunSpec_Require, arg: A): FunSpec =
      FunSpec(require = List(x.expr_.accept(exprvis, arg)))
    override def visit(x: syntax.FunSpec_Ensure, arg: A): FunSpec =
      FunSpec(ensure = List(x.expr_.accept(exprvis, arg)))
    override def visit(x: syntax.FunSpec_Invariant, arg: A): FunSpec =
      val bl = unsigilBlock(x.blockident_)
      val e = x.expr_.accept(exprvis, arg)
      FunSpec(invariant = Map(bl -> List(e)))

    /**
     * Parses the [[ProgSpec]] (rely/guarantee) for the program.
     */
    def visitProgSpec(x: syntax.Decl_ProgWithSpec, arg: A) =
      x.listprogspec_.asScala.foldLeft(ProgSpec()) { case (spec, x) =>
        spec.merge(x.accept(this, arg))
      }

    override def visit(x: syntax.ProgSpec_Rely, arg: A) =
      ProgSpec(rely = List(x.expr_.accept(exprvis, arg)))
    override def visit(x: syntax.ProgSpec_Guarantee, arg: A) =
      ProgSpec(guar = List(x.expr_.accept(exprvis, arg)))
  }

}
