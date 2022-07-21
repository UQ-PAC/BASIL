package translating

import BilParser.{AnnotationsBaseListener, AnnotationsListener}
import BilParser.AnnotationsParser
import BilParser.AnnotationsParser.{GammaContext, LpredContext, RelyContext}
import astnodes.exp.`var`.Register
import astnodes.pred.{BinOp, ExprComp, Pred, UniOp, Var}
import astnodes.sec.{Sec, SecVar}
import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeProperty}

import scala.collection.mutable

class AnnotationsLoader extends AnnotationsBaseListener {

  private val preds = ParseTreeProperty[Pred]
  val lPreds = new mutable.HashMap[Register, Sec]
  val gammaMappings = new mutable.HashMap[Register, SecVar]
  var rely: Option[Pred] = None
  private val secs = ParseTreeProperty[Sec]

  def getPred(node: ParseTree): Pred = {
    if (node == null) throw new NullPointerException("Null pred")
    if (preds.get(node) != null) preds.get(node)
    else throw new Exception("Unparsed pred " + node.getText + " (" + node.getClass + ")")

  }
  private def getSec(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null security")
    if (secs.get(node) != null) secs.get(node)
    else throw new Exception("Unparsed security " + node.getText + " (" + node.getClass + ")")

  override def exitE0(ctx: AnnotationsParser.E0Context): Unit = {
    if ctx.EQUIV_OP != null then preds.put(ctx, new BinOp(ctx.EQUIV_OP.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE1(ctx: AnnotationsParser.E1Context): Unit = {
    if ctx.IMPL_OP != null then preds.put(ctx, new BinOp(ctx.IMPL_OP.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE2(ctx: AnnotationsParser.E2Context): Unit = {
    var pred: Pred = getPred(ctx.exp);

    if (ctx.eOr != null) {
      for (i <- 0 to ctx.eOr.size - 1) {
        pred = new BinOp(ctx.eOr(i).OR_OP.getText, pred, getPred(ctx.eOr(i).exp));
      }
    } else if (ctx.eAnd != null) {
      for (i <- 0 to ctx.eAnd.size - 1) {
        pred = new BinOp(ctx.eAnd(i).AND_OP.getText, pred, getPred(ctx.eAnd(i).exp));
      }
    }
    preds.put(ctx, pred);
  }
  override def exitE3(ctx: AnnotationsParser.E3Context): Unit = {
    if ctx.relOp != null then preds.put(ctx, new BinOp(ctx.relOp.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE4(ctx: AnnotationsParser.E4Context): Unit = {
    if ctx.CONCAT_OP != null then preds.put(ctx, new BinOp(ctx.CONCAT_OP.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE5(ctx: AnnotationsParser.E5Context): Unit = {
    if ctx.ADD_OP != null then preds.put(ctx, new BinOp(ctx.ADD_OP.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE6(ctx: AnnotationsParser.E6Context): Unit = {
    if ctx.MUL_OP != null then preds.put(ctx, new BinOp(ctx.MUL_OP.getText, getPred(ctx.lhs), getPred(ctx.rhs)))
    else preds.put(ctx, getPred(ctx.exp));

  }
  override def exitE7(ctx: AnnotationsParser.E7Context): Unit = {
    var pred: Pred = getPred(ctx.exp);
    for (i <- ctx.UN_OP.size - 1 to 0) {
      pred = new UniOp(ctx.UN_OP(i).getText, pred);
    }
    preds.put(ctx, pred);
  }
  override def exitE8(ctx: AnnotationsParser.E8Context): Unit = {
    // TODO: Implement e8 - MAP operations
    preds.put(ctx, getPred(ctx.exp));
  }
  override def exitE9(ctx: AnnotationsParser.E9Context): Unit = {
    // TODO: Finish implementing e9
    // TODO: Implement literals
    if (ctx.KW_FALSE != null) {
    } else if (ctx.KW_TRUE != null) {
    } else if (ctx.NUMBER != null) {
    } else if (ctx.ID != null) {
      if (ctx.funcApplication != null) {
        // TODO: Implement function call
      } else {
        preds.put(ctx, new Var(ctx.ID.getText))
      }
    } else if (ctx.KW_OLD != null) {
      // TODO: Implement Old nodes (if needd)
    } else if (ctx.QOP != null) {
      // TODO: Implement quantifier nodes
    } else {
      preds.put(ctx, getPred(ctx.pred));
    }
  }
  
  override def exitSecLatticeElem(ctx: BilParser.AnnotationsParser.SecLatticeElemContext): Unit = secs.put(ctx, SecVar(ctx.getText))


  // TODO: Not sure how to handle references to registers? i.e. ctx.var. It seems like the specification being in a
  //  separate file makes it impossible to resolve this.
  override def exitGamma(ctx: GammaContext): Unit = {
    //getExpr(ctx.`var`) match {
    //  case v: Register => gammaMappings.put(v, SecVar(ctx.id(1).getText))
    //}
  }
  // TODO: Not sure how to handle references to registers? i.e. ctx.var. It seems like the specification being in a
  //  separate file makes it impossible to resolve this.
  override def exitLpred(ctx: LpredContext): Unit = {
    //(getExpr(ctx.`var`), getSec(ctx.secExpr)) match {
    //  case (v: Register, p: Sec) => lPreds.put(v, p)
    //}
  }

  override def exitRely(ctx: RelyContext): Unit = rely = Some(getPred(ctx.pred))

}
