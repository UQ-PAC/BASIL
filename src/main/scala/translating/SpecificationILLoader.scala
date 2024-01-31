package translating

import Parsers.SpecificationsParser._
import boogie._
import specification._
import ir._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

// take symbol table entries as input
// parse all global defs and check sizes
// use these new nameToGlobals going forward when parsing the rest of the spec

case class SpecificationLoaderIL(symbols: Set[Variable]) {
  private val idToSymbol = symbols.map(g => (g.name, g)).toMap

  val nameToGlobals: Map[String, Variable] = symbols.map(s => s.name -> s).toMap

  def visitExpr(
      ctx: ExprContext,
      params: Map[String, Parameter] = Map()
  ): Expr = {
    val exprs = ctx.impliesExpr.asScala.map(e => visitImpliesExpr(e, nameToGlobals, params))
    if (exprs.size > 1) {
      exprs.tail.foldLeft(exprs.head)((opExpr: Expr, next: Expr) => BinaryExpr(BoolEQUIV, opExpr, next))
    } else {
      exprs.head
    }
  }

  def visitImpliesExpr(
      ctx: ImpliesExprContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryExpr(
        BoolIMPLIES,
        visitLogicalExpr(ctx.arg1, nameToGlobals, params),
        visitImpliesExpr(ctx.arg2, nameToGlobals, params)
      )
    case None => visitLogicalExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitLogicalExpr(
      ctx: LogicalExprContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = {
    val rels = ctx.relExpr.asScala.map(r => visitRelExpr(r, nameToGlobals, params))
    if (rels.size > 1) {
      val op = if (ctx.AND_OP.size > 0) {
        BoolAND
      } else {
        BoolOR
      }
      rels.tail.foldLeft(rels.head)((opExpr: Expr, next: Expr) => BinaryExpr(op, opExpr, next))
    } else {
      rels.head
    }
  }

  def visitRelExpr(
      ctx: RelExprContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryExpr(
        visitRelOp(ctx.op),
        visitTerm(ctx.arg1, nameToGlobals, params),
        visitTerm(ctx.arg2, nameToGlobals, params)
      )
    case None => visitTerm(ctx.arg1, nameToGlobals, params)
  }

  def visitTerm(
      ctx: TermContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryExpr(
        visitAddSubOp(ctx.op),
        visitFactor(ctx.arg1, nameToGlobals, params),
        visitFactor(ctx.arg2, nameToGlobals, params)
      )
    case None => visitFactor(ctx.arg1, nameToGlobals, params)
  }

  def visitFactor(
      ctx: FactorContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = Option(ctx.arg2) match {
    case Some(_) =>
      BinaryExpr(
        visitMulDivModOp(ctx.op),
        visitUnaryExpr(ctx.arg1, nameToGlobals, params),
        visitUnaryExpr(ctx.arg2, nameToGlobals, params)
      )
    case None => visitUnaryExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitUnaryExpr(
      ctx: UnaryExprContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = ctx match {
    case n: NegExprContext       => UnaryExpr(BVNEG, visitUnaryExpr(n.unaryExpr, nameToGlobals, params))
    case a: AtomUnaryExprContext => visitAtomExpr(a.atomExpr, nameToGlobals, params)
    case n: NotExprContext       => UnaryExpr(BoolNOT, visitUnaryExpr(n.unaryExpr, nameToGlobals, params))
  }

  def visitAtomExpr(
      ctx: AtomExprContext,
      nameToGlobals: Map[String, Variable],
      params: Map[String, Parameter] = Map()
  ): Expr = ctx match {
    case b: BoolLitExprContext     => visitBoolLit(b.boolLit)
    case i: IdExprContext          => visitId(i.id, nameToGlobals, params)
    //case o: OldExprContext         => visitOldExpr(o, nameToGlobals, params)
    case p: ParenExprContext       => visitExpr(p.expr, params)
    //case i: IfThenElseExprContext  => visitIfThenElseExpr(i, nameToGlobals, params)
    //case a: ArrayAccessExprContext => visitArrayAccess(a.arrayAccess, nameToGlobals, params)
    case b: BvExprContext          => visitBv(b.bv)
  }

  //def visitArrayAccess(
  //    ctx: ArrayAccessContext,
  //    nameToGlobals: Map[String, Variable],
  //    params: Map[String, Parameter] = Map()
  //): ArrayAccess = {
  //  val global = visitId(ctx.id, nameToGlobals, params) match {
  //    case g: Variable => g
  //    case _ => throw new Exception("invalid array access '" + ctx.getText + "' to non-global in specification")
  //  }
  //  ArrayAccess(global, Integer.parseInt(ctx.nat.getText))
  //}

  def visitBv(ctx: BvContext): BitVecLiteral = {
    BitVecLiteral(BigInt(ctx.value.getText), Integer.parseInt(ctx.BVSIZE.getText.stripPrefix("bv")))
  }


  //def visitIfThenElseExpr(
  //    ctx: IfThenElseExprContext,
  //    nameToGlobals: Map[String, Variable],
  //    params: Map[String, Parameter] = Map()
  //): IfThenElse = {
  //  IfThenElse(
  //    visitExpr(ctx.guard, nameToGlobals, params),
  //    visitExpr(ctx.thenExpr, nameToGlobals, params),
  //    visitExpr(ctx.elseExpr, nameToGlobals, params)
  //  )
  //}

  def visitNat(ctx: NatContext): IntBLiteral = IntBLiteral(BigInt(ctx.getText))

  def visitBoolLit(ctx: BoolLitContext): BoolLit = ctx.getText match {
    case "true"  => TrueLiteral
    case "false" => FalseLiteral
  }

  def visitId(ctx: IdContext, nameToGlobals: Map[String, Variable], params: Map[String, Parameter] = Map()): Expr = {
    val id = ctx.getText
    //if (id.startsWith("Gamma_")) {
    //  val gamma_id = id.stripPrefix("Gamma_")
    //  params.get(gamma_id) match {
    //    case Some(p: Parameter) => p.value.toGamma
    //    case None =>
    //      nameToGlobals.get(gamma_id) match {
    //        case Some(g: Variable) => SpecGamma(g)
    //        case None                => throw new Exception(s"unresolvable reference to 'Gamma_$id' in specification")
    //      }
    //  }
    //} else {
    {  params.get(id) match {
        case Some(p: Parameter) =>
          val registerSize = p.value.size
          val paramSize = p.size
          if (paramSize == registerSize) {
            p.value
          } else if (registerSize > paramSize) {
            Extract(registerSize - p.size, 0, p.value)
          } else {
            throw Exception(s"parameter $p doesn't fit in register ${p.value} for ID $id")
          }
        case None =>
          nameToGlobals.get(ctx.getText) match {
            case Some(g: Variable) => g
            case None                => throw new Exception(s"unresolvable reference to '$id' in specification")
          }
      }
    }
  }

  def visitMulDivModOp(ctx: MulDivModOpContext): BVBinOp = ctx.getText match {
    case "*"   => BVMUL
    case "div" => BVSDIV
    case "mod" => BVSMOD
  }

  def visitAddSubOp(ctx: AddSubOpContext): BVBinOp = ctx.getText match {
    case "+" => BVADD
    case "-" => BVSUB
  }

  // may need to make this more sophisticated and check for bool == bool
  def visitRelOp(ctx: RelOpContext): BVBinOp = ctx.getText match {
    case "==" => BVEQ
    case "!=" => BVNEQ
    case ">"  => BVSGT
    case ">=" => BVSGE
    case "<"  => BVSLT
    case "<=" => BVSLE
  }


}
