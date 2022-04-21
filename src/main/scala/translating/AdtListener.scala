package translating

import BilParser.{BilAdtListener, BilAdtParser}
import astnodes.exp.Expr
import astnodes.exp.`var`.Register
import astnodes.pred.Pred
import astnodes.sec.{Sec, SecVar}
import astnodes.stmt.{EnterSub, Stmt}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeProperty, TerminalNode}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AdtListener extends BilAdtListener {
  val stmts = ArrayBuffer[Stmt]()
  private val exprs = ParseTreeProperty[Expr]
  private def getExpr(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null expression")
    if (exprs.get(node) != null)
      exprs.get(node)
    else
      throw new Exception("Unparsed expression " + node.getText + " (" + node.getClass + ")")

  private val preds = ParseTreeProperty[Pred]
  private def getPred(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null pred")
    if (preds.get(node) != null) preds.get(node)
    else throw new Exception("Unparsed pred " + node.getText + " (" + node.getClass + ")")


  private val secs = ParseTreeProperty[Sec]
  private def getSec(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null security")
    if (secs.get(node) != null) secs.get(node)
    else throw new Exception("Unparsed security " + node.getText + " (" + node.getClass + ")")

  val lPreds = new mutable.HashMap[Register, Sec]
  val gammaMappings = new mutable.HashMap[Register, SecVar]


  val varSizes = mutable.Map[String, Int]()

  var rely: Option[Pred] = None

  private var currentFunction: EnterSub = null
  private var pcCount = 0

  var requires: List[Pred] = List()
  var ensures: List[Pred] = List()

  private def getSubItem(ctx: BilAdtParser.ItemContext, name: String): BilAdtParser.ItemContext = {
    for ( i <- 0 to ctx.sequence().item().size() - 1
          if ctx.sequence().item(i).SYMBOL().getText == name) {
        return ctx.sequence.item(i);
      }
    throw new Exception("Could not find SubItem: " + name)
  }

  //private def getAttr(AttrsCtx: BilAdtParser.ItemContext): BilAdtParser.ItemContext = {
  //
  //}

  /**
   * Enter a parse tree produced by {@link BilAdtParser#   item}.
   *
   * @param ctx the parse tree
   */
  override def enterItem(ctx: BilAdtParser.ItemContext): Unit = {
    //ctx.SYMBOL().getText() match {
    //}
  }

  /**
   * Exit a parse tree produced by {@link BilAdtParser# item}.
   *
   * @param ctx the parse tree
   */
  override def exitItem(ctx: BilAdtParser.ItemContext): Unit = {
    ctx.SYMBOL().getText() match {
      case "Sub" =>
        if (currentFunction != null)
          currentFunction.setRequiresEnsures(requires, ensures)

        requires = List()
        ensures = List()

        val name = getSubItem(ctx, "Tid").sequence().item(1);
        //val name = ctx.functionName.getText

        val function = new EnterSub(null, name, List(), List())
        stmts += function
        this.currentFunction = function
      case "Def" =>
        

    }
  }


  /**
   * Enter a parse tree produced by {@link BilAdtParser# sequence}.
   *
   * @param ctx the parse tree
   */
  override def enterSequence(ctx: BilAdtParser.SequenceContext): Unit = ???

  /**
   * Exit a parse tree produced by {@link BilAdtParser# sequence}.
   *
   * @param ctx the parse tree
   */
  override def exitSequence(ctx: BilAdtParser.SequenceContext): Unit = ???

  override def visitTerminal(node: TerminalNode): Unit = ???

  override def visitErrorNode(node: ErrorNode): Unit = ???

  override def enterEveryRule(ctx: ParserRuleContext): Unit = ???

  override def exitEveryRule(ctx: ParserRuleContext): Unit = ???
}
