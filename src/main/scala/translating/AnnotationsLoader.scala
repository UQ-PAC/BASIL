package translating

import BilParser.{AnnotationsBaseListener, AnnotationsListener}
import BilParser.AnnotationsParser
import astnodes.pred.{BinOp, ExprComp, Pred, UniOp}
import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeProperty}

class AnnotationsLoader extends AnnotationsBaseListener {

  private val preds = ParseTreeProperty[Pred]

  def getPred(node: ParseTree): Pred = {
    if (node == null) throw new NullPointerException("Null pred")
    if (preds.get(node) != null) preds.get(node)
    else throw new Exception("Unparsed pred " + node.getText + " (" + node.getClass + ")")

  }

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
  override def exitE8(ctx: AnnotationsParser.E8Context): Unit = {}
  override def exitE9(ctx: AnnotationsParser.E9Context): Unit = {
    preds.put(ctx, )
  }
  override def exitEOr(ctx: AnnotationsParser.EOrContext): Unit = {}
  override def exitEAnd(ctx: AnnotationsParser.EAndContext): Unit = {}

}
