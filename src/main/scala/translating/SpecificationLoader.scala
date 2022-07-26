package translating

import BilParser.SpecificationsParser._
import boogie._
import specification._

import scala.jdk.CollectionConverters.*

case class SpecificationLoader(globals: Set[SpecGlobal]) {
  private val idToGlobals = globals.map(g => (g.name, g)).toMap

  def visitSpecification(ctx: SpecificationContext): Specification = {
    val lPreds = Option(ctx.lPreds) match {
      case Some(_) => visitLPreds(ctx.lPreds)
      case None => Map()
    }
    val gammaInits = Option(ctx.gammaInits) match {
      case Some(_) => visitGammaInits(ctx.gammaInits)
      case None => Map()
    }
    val relies = Option(ctx.relies) match {
      case Some(_) => visitRelies(ctx.relies)
      case None => List()
    }
    val guarantees = Option(ctx.guarantees) match {
      case Some(_) => visitGuarantees(ctx.guarantees)
      case None => List()
    }
    specification.Specification(globals, lPreds, gammaInits, relies, guarantees)
  }

  def visitLPred(ctx: LPredContext): (SpecGlobal, BExpr) = {
    (idToGlobals(ctx.id.getText), visitExpr(ctx.expr))
  }

  def visitGamma(ctx: GammaContext): (SpecGlobal, BoolLit) = {
    (idToGlobals(ctx.id.getText), visitBoolLit(ctx.boolLit))
  }

  def visitGammaInits(ctx: GammaInitsContext): Map[SpecGlobal, BoolLit] = {
    ctx.gamma.asScala.map(g => visitGamma(g)).toMap
  }

  def visitLPreds(ctx: LPredsContext): Map[SpecGlobal, BExpr] = {
    ctx.lPred.asScala.map(l => visitLPred(l)).toMap
  }

  def visitRelies(ctx: ReliesContext): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e)).toList
  }

  def visitGuarantees(ctx: GuaranteesContext): List[BExpr] = {
    ctx.expr.asScala.map(e => visitExpr(e)).toList
  }

  def visitExpr(ctx: ExprContext): BExpr = {
    val exprs = ctx.impliesExpr.asScala.map(e => visitImpliesExpr(e))
    if (exprs.size > 1) {
      exprs.tail.foldLeft(exprs.head)((opExpr: BExpr, next: BExpr) => BinaryBExpr(BoolEQUIV, opExpr, next))
    } else {
      exprs.head
    }
  }

  def visitImpliesExpr(ctx: ImpliesExprContext): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(BoolIMPLIES, visitLogicalExpr(ctx.arg1), visitImpliesExpr(ctx.arg2))
    case None => visitLogicalExpr(ctx.arg1)
  }

  def visitLogicalExpr(ctx: LogicalExprContext): BExpr = {
    val rels = ctx.relExpr.asScala.map(r => visitRelExpr(r))
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

  def visitRelExpr(ctx: RelExprContext): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitRelOp(ctx.op), visitTerm(ctx.arg1), visitTerm(ctx.arg2))
    case None => visitTerm(ctx.arg1)
  }

  def visitTerm(ctx: TermContext): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitAddSubOp(ctx.op), visitFactor(ctx.arg1), visitFactor(ctx.arg2))
    case None => visitFactor(ctx.arg1)
  }

  def visitFactor(ctx: FactorContext): BExpr = Option(ctx.arg2) match {
    case Some(_) => BinaryBExpr(visitMulDivModOp(ctx.op), visitUnaryExpr(ctx.arg1), visitUnaryExpr(ctx.arg2))
    case None => visitUnaryExpr(ctx.arg1)
  }

  def visitUnaryExpr(ctx: UnaryExprContext): BExpr = ctx match {
    case n: NegExprContext => UnaryBExpr(IntNEG, visitUnaryExpr(n.unaryExpr))
    case a: AtomUnaryExprContext => visitAtomExpr(a.atomExpr)
  }

  def visitAtomExpr(ctx: AtomExprContext): BExpr = ctx match {
    case b: BoolLitExprContext => visitBoolLit(b.boolLit)
    case n: NatContext => visitNat(n)
    case i: IdExprContext => visitId(i.id)
    case g: GammaIdExprContext => visitGammaId(g.gammaId)
    case o: OldExprContext => visitOldExpr(o)
    case p: ParenExprContext => visitExpr(p.expr)
    case i: IfThenElseExprContext => visitIfThenElseExpr(i)
  }

  def visitOldExpr(ctx: OldExprContext): Old = Old(visitExpr(ctx.expr))

  def visitIfThenElseExpr(ctx: IfThenElseExprContext): IfThenElse = {
    IfThenElse(visitExpr(ctx.guard), visitExpr(ctx.thenExpr), visitExpr(ctx.elseExpr))
  }

  def visitNat(ctx: NatContext): IntLiteral = IntLiteral(BigInt(ctx.getText))

  def visitBoolLit(ctx: BoolLitContext): BoolLit = ctx.getText match {
    case "true" => TrueLiteral
    case "false" => FalseLiteral
  }

  def visitGammaId(ctx: GammaIdContext): SpecGamma = {
    SpecGamma(visitId(ctx.id))
  }

  def visitId(ctx: IdContext): SpecGlobal = {
    idToGlobals(ctx.getText)
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
}
