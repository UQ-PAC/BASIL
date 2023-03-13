package translating

import BilParser.SpecificationsParser._
import boogie._
import specification._
import ir._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

case class SpecificationLoader(globals: Set[SpecGlobal], program: Program) {
  private val idToGlobals = globals.map(g => (g.name, g)).toMap

  def visitSpecification(ctx: SpecificationContext): Specification = {
    val lPreds = Option(ctx.lPreds) match {
      case Some(_) => visitLPreds(ctx.lPreds)
      case None => Map()
    }
    /*
    val gammaInits = Option(ctx.gammaInits) match {
      case Some(_) => visitGammaInits(ctx.gammaInits)
      case None => Map()
    }
    val inits = Option(ctx.inits) match {
      case Some(_) => visitInits(ctx.inits)
      case None => Map()
    }
    */
    val relies = Option(ctx.relies) match {
      case Some(_) => visitRelies(ctx.relies)
      case None => List()
    }
    val guarantees = Option(ctx.guarantees) match {
      case Some(_) => visitGuarantees(ctx.guarantees)
      case None => List()
    }
    val subroutines = ctx.subroutine.asScala.map(s => visitSubroutine(s)).toList
    Specification(globals, lPreds, relies, guarantees, subroutines)
  }

  def visitLPred(ctx: LPredContext): (SpecGlobal, BExpr) = {
    (idToGlobals(ctx.id.getText), visitExpr(ctx.expr))
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
  def visitLPreds(ctx: LPredsContext): Map[SpecGlobal, BExpr] = {
    ctx.lPred.asScala.map(l => visitLPred(l)).toMap
  }

  def visitRelies(ctx: ReliesContext): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e)).toList
  }

  def visitGuarantees(ctx: GuaranteesContext): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e)).toList
  }

  def visitExpr(ctx: ExprContext, params: Map[String, Parameter] = Map()): BExpr = {
    val exprs = ctx.impliesExpr.asScala.map(e => visitImpliesExpr(e, params))
    if (exprs.size > 1) {
      exprs.tail.foldLeft(exprs.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(BoolEQUIV, opExpr, next))
    } else {
      exprs.head
    }
  }

  def visitImpliesExpr(ctx: ImpliesExprContext, params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(BoolIMPLIES, visitLogicalExpr(ctx.arg1, params), visitImpliesExpr(ctx.arg2, params))
    case None => visitLogicalExpr(ctx.arg1, params)
  }

  def visitLogicalExpr(ctx: LogicalExprContext, params: Map[String, Parameter] = Map()): BExpr = {
    val rels = ctx.relExpr.asScala.map(r => visitRelExpr(r, params))
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

  def visitRelExpr(ctx: RelExprContext, params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitRelOp(ctx.op), visitTerm(ctx.arg1, params), visitTerm(ctx.arg2, params))
    case None => visitTerm(ctx.arg1, params)
  }

  def visitTerm(ctx: TermContext, params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitAddSubOp(ctx.op), visitFactor(ctx.arg1, params), visitFactor(ctx.arg2, params))
    case None => visitFactor(ctx.arg1, params)
  }

  def visitFactor(ctx: FactorContext, params: Map[String, Parameter] = Map()): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitMulDivModOp(ctx.op), visitUnaryExpr(ctx.arg1, params), visitUnaryExpr(ctx.arg2, params))
    case None => visitUnaryExpr(ctx.arg1, params)
  }

  def visitUnaryExpr(ctx: UnaryExprContext, params: Map[String, Parameter] = Map()): BExpr = ctx match {
    case n: NegExprContext => UnaryBExpr(IntNEG, visitUnaryExpr(n.unaryExpr, params))
    case a: AtomUnaryExprContext => visitAtomExpr(a.atomExpr, params)
  }

  def visitAtomExpr(ctx: AtomExprContext, params: Map[String, Parameter] = Map()): BExpr = ctx match {
    case b: BoolLitExprContext => visitBoolLit(b.boolLit)
    case n: NatExprContext => visitNat(n.nat)
    case i: IdExprContext => visitId(i.id, params)
    case g: GammaIdExprContext => visitGammaId(g.gammaId, params)
    case o: OldExprContext => visitOldExpr(o, params)
    case p: ParenExprContext => visitExpr(p.expr, params)
    case i: IfThenElseExprContext => visitIfThenElseExpr(i, params)
  }

  def visitOldExpr(ctx: OldExprContext, params: Map[String, Parameter] = Map()): Old = Old(visitExpr(ctx.expr))

  def visitIfThenElseExpr(ctx: IfThenElseExprContext, params: Map[String, Parameter] = Map()): IfThenElse = {
    IfThenElse(visitExpr(ctx.guard, params), visitExpr(ctx.thenExpr, params), visitExpr(ctx.elseExpr, params))
  }

  def visitNat(ctx: NatContext): IntBLiteral = IntBLiteral(BigInt(ctx.getText))

  def visitBoolLit(ctx: BoolLitContext): BoolBLiteral = ctx.getText match {
    case "true" => TrueBLiteral
    case "false" => FalseBLiteral
  }

  def visitGammaId(ctx: GammaIdContext, params: Map[String, Parameter] = Map()): BExpr = {
    val id = ctx.id.getText
    params.get(id) match {
      case Some(p: Parameter) => p.toGamma
      case None => idToGlobals.get(id) match {
        case Some(g: SpecGlobal) => SpecGamma(g)
        case None => throw new Exception(s"unresolveable reference to 'Gamma_$id' in specification")
      }
    }
  }

  def visitId(ctx: IdContext, params: Map[String, Parameter] = Map()): BExpr = {
    val id = ctx.getText
    params.get(id) match {
      case Some(p: Parameter) => p.toBoogie
      case None => idToGlobals.get(ctx.getText) match {
        case Some(g: SpecGlobal) => g
        case None => throw new Exception(s"unresolveable reference to '$id' in specification")
      }
    }
  }

  def visitMulDivModOp(ctx: MulDivModOpContext): IntBinOp = ctx.getText match {
    case "*" => IntMUL
    case "div" => IntDIV
    case "mod" => IntMOD
  }

  def visitAddSubOp(ctx: AddSubOpContext): IntBinOp = ctx.getText match {
    case "+" => IntADD
    case "-" => IntSUB
  }

  // may need to make this more sophisticated and check for bool == bool
  def visitRelOp(ctx: RelOpContext): IntBinOp = ctx.getText match {
    case "==" => IntEQ
    case "!=" => IntNEQ
    case ">" => IntGT
    case ">=" => IntGE
    case "<" => IntLT
    case "<=" => IntLE
  }

  def visitSubroutine(ctx: SubroutineContext): SubroutineSpec = {
    val name = ctx.id.getText
    val irProc = program.procedures.collectFirst { case p if p.name == name => p }

    val params: Map[String, Parameter] = irProc match {
      case None => Map()
      case Some(p) => p.in.map { (p: Parameter) => p.name -> p }.toMap ++ p.out.map { (p: Parameter) => p.name -> p }.toMap
    }

    val requires = ctx.requires.asScala.map(r => visitExpr(r.expr, params)).toList
    val ensures = ctx.ensures.asScala.map(e => visitExpr(e.expr, params)).toList
    SubroutineSpec(ctx.id.getText, requires, ensures)
  }
}
