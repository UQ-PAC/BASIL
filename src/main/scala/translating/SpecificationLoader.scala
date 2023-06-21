package translating

import BilParser.SpecificationsParser._
import boogie._
import specification._
import ir._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

// take symbol table entries as input
// parse all global defs and check sizes
// use these new nameToGlobals going forward when parsing the rest of the spec

case class SpecificationLoader(symbols: Set[SpecGlobal], program: Program) {
  private val idToSymbol = symbols.map(g => (g.name, g)).toMap

  def visitSpecification(ctx: SpecificationContext): Specification = {
    val globals = Option(ctx.globals) match {
      case Some(_) => visitGlobals(ctx.globals)
      case None => Set()
    }
    val nameToGlobals = globals.map(g => (g.name, g)).toMap

    val lPreds = Option(ctx.lPreds) match {
      case Some(_) => visitLPreds(ctx.lPreds, nameToGlobals)
      case None => Map()
    }

    val relies = Option(ctx.relies) match {
      case Some(_) => visitRelies(ctx.relies, nameToGlobals)
      case None => List()
    }

    val guarantees = Option(ctx.guarantees) match {
      case Some(_) => visitGuarantees(ctx.guarantees, nameToGlobals)
      case None => List()
    }

    val subroutines = ctx.subroutine.asScala.map(s => visitSubroutine(s, nameToGlobals)).toList
    Specification(globals, lPreds, relies, guarantees, subroutines)
  }

  def visitGlobals(ctx: GlobalsContext): Set[SpecGlobal] = {
    ctx.globalDef.asScala.map(g => visitGlobalDef(g)).toSet
  }

  def visitGlobalDef(ctx: GlobalDefContext): SpecGlobal = {
    val id = ctx.id.getText
    val symbol = idToSymbol.get(id) match {
      case Some(g: SpecGlobal) => g
      case None => throw new Exception(s"unresolvable reference to '$id' in specification")
    }
    val size = ctx.typeName match {
      case b: BvTypeContext => Integer.parseInt(b.size.getText)
      case _: LongTypeContext => 64
      case _: ShortTypeContext => 16
      case _: IntTypeContext => 32
      case _: CharTypeContext => 8
    }
    val arraySize = Option(ctx.arraySize) match {
      case Some(a: ArraySizeContext) => Some(Integer.parseInt(a.size.getText))
      case None => None
    }
    val bits = size * arraySize.getOrElse(1)
    if (bits != symbol.size) {
      throw new Exception(s"variable $id in specification does not match symbol table")
    }
    SpecGlobal(id, size, arraySize, symbol.address)
  }

  def visitLPred(ctx: LPredContext, nameToGlobals: Map[String, SpecGlobal]): (SpecGlobal, BExpr) = {
    (idToSymbol(ctx.id.getText), visitExpr(ctx.expr, nameToGlobals))
  }

  /*
  def visitGamma(ctx: GammaContext): (SpecGlobal, BoolLit) = {
    (idToGlobals(ctx.id.getText), visitBoolLit(ctx.boolLit))
  }

  def visitGammaInits(ctx: GammaInitsContext): Map[SpecGlobal, BoolLit] = {
    ctx.gamma.asScala.map(g => visitGamma(g)).toMap
  }

  def visitInit(ctx: InitContext): (SpecGlobal, IntLiteral) = {
    (idToGlobals(ctx.id.getText), visitNat(ctx.nat))
  }

  def visitInits(ctx: InitsContext): Map[SpecGlobal, IntLiteral] = {
    ctx.init.asScala.map(i => visitInit(i)).toMap
  }

   */
  def visitLPreds(ctx: LPredsContext, nameToGlobals: Map[String, SpecGlobal]): Map[SpecGlobal, BExpr] = {
    ctx.lPred.asScala.map(l => visitLPred(l, nameToGlobals)).toMap
  }

  def visitRelies(ctx: ReliesContext, nameToGlobals: Map[String, SpecGlobal]): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e, nameToGlobals)).toList
  }

  def visitGuarantees(ctx: GuaranteesContext, nameToGlobals: Map[String, SpecGlobal]): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e, nameToGlobals)).toList
  }

  def visitExpr(ctx: ExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = {
    val exprs = ctx.impliesExpr.asScala.map(e => visitImpliesExpr(e, nameToGlobals, params))
    if (exprs.size > 1) {
      exprs.tail.foldLeft(exprs.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(BoolEQUIV, opExpr, next))
    } else {
      exprs.head
    }
  }

  def visitImpliesExpr(ctx: ImpliesExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(BoolIMPLIES, visitLogicalExpr(ctx.arg1, nameToGlobals, params), visitImpliesExpr(ctx.arg2, nameToGlobals, params))
    case None => visitLogicalExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitLogicalExpr(ctx: LogicalExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = {
    val rels = ctx.relExpr.asScala.map(r => visitRelExpr(r, nameToGlobals, params))
    if (rels.size > 1) {
      val op = if (ctx.AND_OP.size > 0) {
        BoolAND
      } else {
        BoolOR
      }
      rels.tail.foldLeft(rels.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(op, opExpr, next))
    } else {
      rels.head
    }
  }

  def visitRelExpr(ctx: RelExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitRelOp(ctx.op), visitTerm(ctx.arg1, nameToGlobals, params), visitTerm(ctx.arg2, nameToGlobals, params))
    case None => visitTerm(ctx.arg1, nameToGlobals, params)
  }

  def visitTerm(ctx: TermContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitAddSubOp(ctx.op), visitFactor(ctx.arg1, nameToGlobals, params), visitFactor(ctx.arg2, nameToGlobals, params))
    case None => visitFactor(ctx.arg1, nameToGlobals, params)
  }

  def visitFactor(ctx: FactorContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitMulDivModOp(ctx.op), visitUnaryExpr(ctx.arg1, nameToGlobals, params), visitUnaryExpr(ctx.arg2, nameToGlobals, params))
    case None => visitUnaryExpr(ctx.arg1, nameToGlobals, params)
  }

  def visitUnaryExpr(ctx: UnaryExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = ctx match {
    case n: NegExprContext => UnaryBExpr(BVNEG, visitUnaryExpr(n.unaryExpr, nameToGlobals, params))
    case a: AtomUnaryExprContext => visitAtomExpr(a.atomExpr, nameToGlobals, params)
  }

  def visitAtomExpr(ctx: AtomExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = ctx match {
    case b: BoolLitExprContext => visitBoolLit(b.boolLit)
    case i: IdExprContext => visitId(i.id, nameToGlobals, params)
    case g: GammaIdExprContext => visitGammaId(g.gammaId, nameToGlobals, params)
    case o: OldExprContext => visitOldExpr(o, nameToGlobals, params)
    case p: ParenExprContext => visitExpr(p.expr, nameToGlobals, params)
    case i: IfThenElseExprContext => visitIfThenElseExpr(i, nameToGlobals, params)
    case a: ArrayAccessExprContext => visitArrayAccess(a.arrayAccess, nameToGlobals, params)
    case b: BvExprContext => visitBv(b.bv)
  }

  def visitArrayAccess(ctx: ArrayAccessContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): ArrayAccess = {
    val global = visitId(ctx.id, nameToGlobals, params) match {
      case g: SpecGlobal => g
      case _ => throw new Exception("invalid array access '"+ ctx.getText +"' to non-global in specification")
    }
    ArrayAccess(global, Integer.parseInt(ctx.nat.getText))
  }

  def visitBv(ctx: BvContext): BitVecBLiteral = {
    BitVecBLiteral(BigInt(ctx.value.getText), Integer.parseInt(ctx.size.getText))
  }

  def visitOldExpr(ctx: OldExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): Old = Old(visitExpr(ctx.expr, nameToGlobals, params))

  def visitIfThenElseExpr(ctx: IfThenElseExprContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): IfThenElse = {
    IfThenElse(visitExpr(ctx.guard, nameToGlobals, params), visitExpr(ctx.thenExpr, nameToGlobals, params), visitExpr(ctx.elseExpr, nameToGlobals, params))
  }

  def visitNat(ctx: NatContext): IntBLiteral = IntBLiteral(BigInt(ctx.getText))

  def visitBoolLit(ctx: BoolLitContext): BoolBLiteral = ctx.getText match {
    case "true" => TrueBLiteral
    case "false" => FalseBLiteral
  }

  def visitGammaId(ctx: GammaIdContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = {
    val id = ctx.id.getText
    params.get(id) match {
      case Some(p: Parameter) => p.toGamma
      case None => nameToGlobals.get(id) match {
        case Some(g: SpecGlobal) => SpecGamma(g)
        case None => throw new Exception(s"unresolvable reference to 'Gamma_$id' in specification")
      }
    }
  }

  def visitId(ctx: IdContext, nameToGlobals: Map[String, SpecGlobal], params: Map[String, Parameter] = Map()): BExpr = {
    val id = ctx.getText
    params.get(id) match {
      case Some(p: Parameter) => p.toBoogie
      case None => nameToGlobals.get(ctx.getText) match {
        case Some(g: SpecGlobal) => g
        case None => throw new Exception(s"unresolvable reference to '$id' in specification")
      }
    }
  }

  def visitMulDivModOp(ctx: MulDivModOpContext): BVBinOp = ctx.getText match {
    case "*" => BVMUL
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
    case ">" => BVSGT
    case ">=" => BVSGE
    case "<" => BVSLT
    case "<=" => BVSLE
  }

  def visitSubroutine(ctx: SubroutineContext, nameToGlobals: Map[String, SpecGlobal]): SubroutineSpec = {
    val name = ctx.id.getText
    val irProc = program.procedures.collectFirst { case p if p.name == name => p }

    val params: Map[String, Parameter] = irProc match {
      case None => Map()
      case Some(p) => p.in.map { (p: Parameter) => p.name -> p }.toMap ++ p.out.map { (p: Parameter) => p.name -> p }.toMap
    }

    val requires = ctx.requires.asScala.map(r => visitExpr(r.expr, nameToGlobals, params)).toList
    val ensures = ctx.ensures.asScala.map(e => visitExpr(e.expr, nameToGlobals, params)).toList
    SubroutineSpec(ctx.id.getText, requires, ensures)
  }
}
