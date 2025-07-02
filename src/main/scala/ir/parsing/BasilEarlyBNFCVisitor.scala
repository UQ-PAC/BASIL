package ir.parsing

import basil_ir.Absyn as syntax

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

trait AttributeListBNFCVisitor[A]
    extends syntax.AttribSet.Visitor[Attrib.Map, A],
      syntax.Attr.Visitor[Attrib, A],
      syntax.AttrKeyValue.Visitor[(String, Attrib), A],
      LiteralsBNFCVisitor[A] {

  def visit(x: syntax.Attr_Map, arg: A): ir.parsing.Attrib =
    Attrib.Map(ListMap.from(x.listattrkeyvalue_.asScala.toList.map(_.accept(this, arg))))
  def visit(x: syntax.Attr_List, arg: A): ir.parsing.Attrib =
    Attrib.List(x.listattr_.asScala.toVector.map(_.accept(this, arg)))
  def visit(x: syntax.Attr_Lit, arg: A): ir.parsing.Attrib =
    Attrib.ValLiteral(x.value_.accept(this, arg))
  def visit(x: syntax.Attr_Str, arg: A): ir.parsing.Attrib = {
    Attrib.ValString(unquote(x.str_, x))
  }

  override def visit(p: syntax.AttrKeyValue1, arg: A): (String, Attrib) =
    (unsigilAttrib(p.bident_), p.attr_.accept(this, arg))

  override def visit(p: syntax.AttribSet_Empty, arg: A): Attrib.Map = Attrib.Map(ListMap())
  override def visit(p: syntax.AttribSet_Some, arg: A): Attrib.Map = {
    Attrib.Map(ListMap.from(p.listattrkeyvalue_.asScala.map(_.accept(this, arg))))
  }

  def getIntCompatAttr(n: String)(attrs: Attrib): Option[BigInt] = {
    attrs match {
      case Attrib.Map(vs) =>
        vs.collect {
          case (attr, Attrib.ValLiteral(ir.IntLiteral(v))) if attr == n => v
          case (attr, Attrib.ValLiteral(ir.BitVecLiteral(v, _))) if attr == n => v
        }.lastOption
      case _ => None
    }
  }

  def parseAttr(a: syntax.Attr, arg: A): Attrib = {
    a.accept(this, arg)
  }

  def parseAttrMap(a: syntax.AttribSet, arg: A): Attrib = {
    a.accept(this, arg)
  }

  def getAddrAttr(attrs: Attrib): Option[BigInt] = getIntCompatAttr("address")(attrs)

  def getStrAttr(n: String)(attrs: Attrib): Option[String] = {
    attrs match {
      case Attrib.Map(vs) =>
        vs.collect {
          case (attr, Attrib.ValString(v)) if attr == n => v
        }.lastOption
      case _ => None
    }
  }
  val getCommentAttr = getStrAttr("comment")
  val getLabelAttr = getStrAttr("label")

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
      syntax.Decl.Visitor[Declarations, A],
      syntax.Params.Visitor[(String, ir.IRType), A],
      AttributeListBNFCVisitor[A] {

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

    // XXX: be aware of foldRight and merge order. foldRight means that to keep the original order,
    // the folding function should /prepend/ to the accumulator.
    listdecl.foldRight(initialDecls)((x, decls) =>
      x match {
        // parse program specifications (rely / guarantee) and update decls.
        case x: syntax.Decl_ProgWithSpec =>
          val spec = visitProgSpec(decls, x, arg)
          Declarations.empty.copy(progSpec = spec).merge(decls)

        // parse function definition bodies and update decls.
        case x: syntax.Decl_Fun =>
          val (nm, defn) = visitFunDef(decls, x, arg)
          decls.updateFunctionDefinition(nm, Some(defn))

        case x: syntax.Decl_Axiom =>
          val ax = visitAxiom(decls, x, arg)
          decls.copy(axioms = ax +: decls.axioms)

        case x: syntax.Decl_Proc =>
          val spec = visitProcSpec(decls, x, arg)
          decls.merge(Declarations.empty.copy(procSpecs = Map(spec)))

        // declarations which do not contain expressions
        case _: syntax.Decl_UnsharedMem | _: syntax.Decl_SharedMem | _: syntax.Decl_Var |
            _: syntax.Decl_ProgEmpty =>
          decls
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

  private def visitParams(x: syntax.ListParams, arg: A): ListMap[String, ir.IRType] = {
    x.asScala.toList.map(_.accept(this, arg)).to(ListMap)
  }

  override def visit(x: syntax.Decl_Proc, arg: A) = {
    val name = unsigilProc(x.procident_)
    val inparams = visitParams(x.listparams_1, arg)
    val outparams = visitParams(x.listparams_2, arg)
    Declarations.empty.copy(
      formalIns = Map(name -> inparams),
      formalOuts = Map(name -> outparams)
    )
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
    val params = visitParams(x.listparams_, arg).map { (n, ty) => ir.LocalVar(n, ty) }.toList
    val returnType = x.type_.accept(this, arg)

    val fun = ir.FunctionDecl(n, params, returnType, None)
    Declarations.empty.copy(functions = Map(n -> fun))
  }

  override def visit(x: syntax.Decl_Axiom, arg: A) =
    Declarations.empty

  protected def makeExprVisitor(
    decls: Declarations
  ): syntax.Expr.Visitor[ir.Expr, A] & syntax.ProgSpec.Visitor[ProgSpec, A] & syntax.FunSpec.Visitor[FunSpec, A] =
    ExprBNFCVisitor[A](decls)

  def visitAxiom(decls: Declarations, x: syntax.Decl_Axiom, arg: A) = {
    val exprvis = makeExprVisitor(decls)

    // TODO: use attribset for axiom declarations
    val _ = x.attribset_

    ir.AxiomDecl(x.expr_.accept(exprvis, arg))
  }

  def visitFunDef(decls: Declarations, x: syntax.Decl_Fun, arg: A) = {
    val exprvis = makeExprVisitor(decls)

    val n = unsigilGlobal(x.globalident_)
    n -> x.expr_.accept(exprvis, arg)
  }

  def visitProcSpec(decls: Declarations, x: syntax.Decl_Proc, arg: A) = {
    val exprvis = makeExprVisitor(decls)
    val specs = x.listfunspec_.asScala.map(_.accept(exprvis, arg))

    val name = unsigilProc(x.procident_)
    name -> specs.foldRight(FunSpec())(_ merge _)
  }

  /**
   * This method is called after the rest of the declarations have been parsed
   * and it produces the [[ProgSpec]] (rely/guarantee) for the program.
   * Because visitng the specification needs to visit expressions, this method
   * calls [[makeProgSpecVisitor]] to construct the inner-visitor.
   */
  def visitProgSpec(decls: Declarations, x: syntax.Decl_ProgWithSpec, arg: A) =
    val specvis = makeExprVisitor(decls)
    x.listprogspec_.asScala.foldLeft(ProgSpec()) { case (spec, x) =>
      spec.merge(x.accept(specvis, arg))
    }

}
