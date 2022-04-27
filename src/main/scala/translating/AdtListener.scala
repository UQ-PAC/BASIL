package translating

import BilParser.{BilAdtListener, BilAdtParser}
import astnodes.exp.{BinOp, Expr, Extend, Extract, Literal, Pad, UniOp}
import astnodes.exp.`var`.{MemLoad, Register, Var}
import astnodes.pred.Pred
import astnodes.sec.{Sec, SecVar}
import astnodes.stmt.{EnterSub, Stmt}
import org.antlr.v4.runtime
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeProperty, TerminalNode}
import util.AssumptionViolationException

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

  override def enterAdt(ctx: BilAdtParser.AdtContext): Unit = ???

  override def exitAdt(ctx: BilAdtParser.AdtContext): Unit = ???

  override def enterExp(ctx: BilAdtParser.ExpContext): Unit = ???

  override def exitExp(ctx: BilAdtParser.ExpContext): Unit = ???

  override def enterEndian(ctx: BilAdtParser.EndianContext): Unit = ???

  override def exitEndian(ctx: BilAdtParser.EndianContext): Unit = ???

  override def enterLoad(ctx: BilAdtParser.LoadContext): Unit = {
    if (ctx.`var`.STRING().getText == "mem") exprs.put(ctx, new MemLoad(getExpr(ctx.exp), Some(ctx.NUM().getText.toInt)))
    else throw new AssumptionViolationException("Found load on variable other than mem")
  }

  override def exitLoad(ctx: BilAdtParser.LoadContext): Unit = ???

  override def enterStore(ctx: BilAdtParser.StoreContext): Unit = ???

  override def exitStore(ctx: BilAdtParser.StoreContext): Unit = ???

  override def enterBinop(ctx: BilAdtParser.BinopContext): Unit = exprs.put(ctx, new BinOp(ctx.BINOP.getText, getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))

  override def exitBinop(ctx: BilAdtParser.BinopContext): Unit = ???

  override def enterUop(ctx: BilAdtParser.UopContext): Unit = exprs.put(ctx, new UniOp(ctx.UOP.getText, getExpr(ctx.exp)))

  override def exitUop(ctx: BilAdtParser.UopContext): Unit = ???

  override def enterVar(ctx: BilAdtParser.VarContext): Unit = {
    if (ctx.STRING.getText != "mem") {  // Is a register
      if (ctx.`type`.imm == null)
        throw new AssumptionViolationException("Can't find Imm argument for a non-mem variable")
      exprs.put(ctx, new Register(ctx.getText, Some(ctx.`type`.imm.NUM.getText.toInt)))
    }
  }

  override def exitVar(ctx: BilAdtParser.VarContext): Unit = ???

  override def enterIntAdt(ctx: BilAdtParser.IntAdtContext): Unit = {
    exprs.put(ctx, Literal(ctx.NUM(0).getText)) // We also have access to size information if needed. But current AST node doesn't ask for it
  }

  override def exitIntAdt(ctx: BilAdtParser.IntAdtContext): Unit = ???

  // Currently doesn't handle high/low. These could be converted to extracts
  override def enterCast(ctx: BilAdtParser.CastContext): Unit = {
    ctx.CAST.getText match {
      case "pad" => exprs.put(ctx, Pad(getExpr(ctx.exp), ctx.NUM.getText.toInt))
      case "extend" => exprs.put(ctx, Extend(getExpr(ctx.exp), ctx.NUM.getText.toInt))
      case "low" | "high" => exprs.put(ctx, getExpr(ctx.exp))
    }
  }

  override def exitCast(ctx: BilAdtParser.CastContext): Unit = ???

  override def enterExtract(ctx: BilAdtParser.ExtractContext): Unit = {
    val first = ctx.NUM(0).getText.toInt;
    val second = ctx.NUM(1).getText.toInt;
    val exp = getExpr(ctx.exp)
    exprs.put(ctx, new Extract(first, second, exp));
  }

  override def exitExtract(ctx: BilAdtParser.ExtractContext): Unit = ???

  override def enterType(ctx: BilAdtParser.TypeContext): Unit = ???

  override def exitType(ctx: BilAdtParser.TypeContext): Unit = ???

  override def enterImm(ctx: BilAdtParser.ImmContext): Unit = ???

  override def exitImm(ctx: BilAdtParser.ImmContext): Unit = ???

  override def enterMem(ctx: BilAdtParser.MemContext): Unit = ???

  override def exitMem(ctx: BilAdtParser.MemContext): Unit = ???

  override def enterTid(ctx: BilAdtParser.TidContext): Unit = ???

  override def exitTid(ctx: BilAdtParser.TidContext): Unit = ???

  override def enterDef(ctx: BilAdtParser.DefContext): Unit = {
    if (currentFunction != null)
      currentFunction.setRequiresEnsures(requires, ensures)

    requires = List()
    ensures = List()

    val tid = ctx.tid.NUM.getText
    val name = ctx.tid.STRING.getText

    val function = new EnterSub(tid, name, List(), List())
    stmts += function
    this.currentFunction = function
  }

  override def exitDef(ctx: BilAdtParser.DefContext): Unit = ???

  override def enterCall(ctx: BilAdtParser.CallContext): Unit = ???

  override def exitCall(ctx: BilAdtParser.CallContext): Unit = ???

  override def enterList(ctx: BilAdtParser.ListContext): Unit = ???

  override def exitList(ctx: BilAdtParser.ListContext): Unit = ???

  override def enterTuple(ctx: BilAdtParser.TupleContext): Unit = ???

  override def exitTuple(ctx: BilAdtParser.TupleContext): Unit = ???

  override def enterUnimplemented(ctx: BilAdtParser.UnimplementedContext): Unit = ???

  override def exitUnimplemented(ctx: BilAdtParser.UnimplementedContext): Unit = ???

  override def enterSequence(ctx: BilAdtParser.SequenceContext): Unit = ???

  override def exitSequence(ctx: BilAdtParser.SequenceContext): Unit = ???

  override def visitTerminal(node: TerminalNode): Unit = ???

  override def visitErrorNode(node: ErrorNode): Unit = ???

  override def enterEveryRule(ctx: ParserRuleContext): Unit = ???

  override def exitEveryRule(ctx: ParserRuleContext): Unit = ???
}
