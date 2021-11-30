package translating

import BilParser.BilParser.ExpCompContext
import BilParser.BilParser.GammaContext
import BilParser.BilParser.GammasContext
import BilParser.BilParser.LpredsContext
import BilParser.BilParser.PredBinOpContext
import BilParser.BilParser.PredBopContext
import BilParser.BilParser.PredBracketContext
import BilParser.BilParser.PredContext
import BilParser.BilParser.PredExprCompContext
import BilParser.BilParser.PredUniOpContext
import BilParser.BilParser.ProgSpecContext
import facts.parameters.InParameter
import facts.parameters.OutParameter
import facts.exp.*
import facts.stmt.*
import facts.stmt.Assign.Load
import facts.stmt.Assign.Move
import facts.stmt.Assign.Store
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeProperty, TerminalNode}
import FlowGraph.Function
import facts.pred

import java.util
import BilParser.*
import facts.pred.{Bool, ExprComp, High, Low, Pred, Security}
import vcgen.State

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// list of facts to output
// TODO also shouldnt need this
class StatementLoader(var stmts: ArrayBuffer[Stmt]) extends BilBaseListener {
  private val exprs = ParseTreeProperty[Expr]
  private def getExpr(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null expression")
    if (exprs.get(node) != null)
      exprs.get(node)
    else
      throw new Exception("Unparsed expression " + node.getText + " (" + node.getClass + ")")

  private val preds = ParseTreeProperty[Pred]
  private def getPred(node: ParseTree) =
    if (node == null) throw new NullPointerException("Null param")
    if (preds.get(node) != null)
      preds.get(node)
    else
      throw new Exception("Unparsed param " + node.getText + " (" + node.getClass + ")")

  val lPreds = new mutable.HashMap[Var, Pred]
  val gammaMappings = new mutable.HashMap[Var, Security]


  // the last function header parsed; needed for assigning parameters
  // TODO we shouldnt need this
  private var currentFunction: EnterSub = null

  // for generating unique labels
  private var pcCount = 0

  override def exitSub(ctx: BilParser.SubContext): Unit = {
    val address = ctx.addr.getText
    val name = ctx.functionName.getText
    val function = new EnterSub(address, name)
    stmts += function
    this.currentFunction = function
  }
  override def exitParamTypes(ctx: BilParser.ParamTypesContext): Unit = {
    val id = ctx.param.getText // human-readable name
    val variable = ctx.`var`.getText // some register, probably
    if (id.contains("result")) currentFunction.setOutParam(new OutParameter(new Var("out"), new Var(variable)))
    else currentFunction.getInParams.add(new InParameter(new Var(id), new Var(variable)))
  }

  override def exitStmt(ctx: BilParser.StmtContext): Unit = {
    val address = ctx.addr.getText
// statements can be assignments, jumps, conditional jumps or function calls
    if (ctx.assign != null) { // statement is assignment; check which type
      val assignCtx = ctx.assign
      if (assignCtx.exp.getClass == classOf[BilParser.ExpLoadContext]) { // statement is a load assignment
        val loadCtx = assignCtx.exp.asInstanceOf[BilParser.ExpLoadContext]
        val lhs = new Var(loadCtx.exp(1).getText)
        val rhs = getExpr(loadCtx.exp(2))
        if (rhs != null) { // null check is necessary as rhs may not exist for loads
          stmts += new Load(address, lhs, rhs.asInstanceOf[MemExpr])
        }
      } else if (assignCtx.exp.getClass == classOf[BilParser.ExpStoreContext]) { // statement is a store assignment
        val storeCtx = assignCtx.exp.asInstanceOf[BilParser.ExpStoreContext]
        val lhs = new MemExpr(getExpr(storeCtx.exp(1)))
        val rhs = getExpr(storeCtx.exp(2))
        stmts += new Store(address, lhs, rhs)
      } else { // statement is a move assignment
        val lhs = new Var(assignCtx.`var`.getText)
        val rhs = getExpr(assignCtx.exp)
        stmts += new Move(address, lhs, rhs)
      }
    } else if (ctx.jmp != null) { // statement is a jump
      var target = ""
      if (ctx.jmp.`var` != null) target = ctx.jmp.`var`.getText
      else if (ctx.jmp.addr != null) target = ctx.jmp.addr.getText
      stmts += new JmpStmt(address, target)
    } else if (ctx.cjmp != null) { // statement is a conditional jump
      val cond = new Var(ctx.cjmp.`var`.getText) // conditions are always vars
      val target = ctx.cjmp.addr.getText
      stmts += new CJmpStmt(address, target, cond)
    } else if (ctx.call != null) { // statement is a call
      if (ctx.call.functionName == null) { // occasionally this occurs with "call LR with no return" lines
        stmts += new ExitSub(ctx.addr.getText)
      } else {
        val funcName = ctx.call.functionName.getText
        stmts += new CallStmt(address, funcName)

        // handle the case of no return
        if (ctx.call.returnaddr.addr != null) stmts += new JmpStmt(uniquePc(), ctx.call.returnaddr.addr.getText)
      }
    } else { // this statement is empty
      stmts += new SkipStmt(address)
    }
  }

  override def exitExpBracket(ctx: BilParser.ExpBracketContext): Unit = exprs.put(ctx, getExpr(ctx.exp))
  override def exitExpUop(ctx: BilParser.ExpUopContext): Unit = exprs.put(ctx, new UniOp(ctx.uop.getText, getExpr(ctx.exp)))
  override def exitExpBop(ctx: BilParser.ExpBopContext): Unit = exprs.put(ctx, new BinOp(ctx.bop.getText, getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))
  override def exitVar(ctx: BilParser.VarContext): Unit = exprs.put(ctx, new Var(ctx.getText))
  override def exitExpVar(ctx: BilParser.ExpVarContext): Unit = exprs.put(ctx, exprs.get(ctx.`var`))
  override def exitExpLiteral(ctx: BilParser.ExpLiteralContext): Unit = exprs.put(ctx, new Literal(ctx.literal.getText))
  override def exitExpExtract(ctx: BilParser.ExpExtractContext): Unit = {
    // fixme: assumes all bit vectors are 64 bits long
    val firstNat = 64 - ctx.nat(0).getText.toInt
    val secondNat = 63 - ctx.nat(1).getText.toInt
    val exp = getExpr(ctx.exp)
    exprs.put(ctx, new Extract(firstNat, secondNat, exp))
  }
  override def exitExpCast(ctx: BilParser.ExpCastContext): Unit = exprs.put(ctx, getExpr(ctx.exp)) // simply unwrap and throw away casts
  override def exitExpLoad(ctx: BilParser.ExpLoadContext): Unit = if (ctx.exp(0).getText == "mem") exprs.put(ctx, new MemExpr(getExpr(ctx.exp(1))))


  override def exitPredBinOp(ctx: PredBinOpContext): Unit = preds.put(ctx, new pred.BinOp(ctx.predBop.getText, getPred(ctx.pred(0)), getPred(ctx.pred(1))))
  override def exitPredUniOp(ctx: PredUniOpContext): Unit = preds.put(ctx, new pred.UniOp(ctx.uop.getText, getPred(ctx.pred)))
  override def exitPredBracket(ctx: PredBracketContext): Unit = preds.put(ctx, getPred(ctx.pred))
  override def exitPredExprComp(ctx: PredExprCompContext): Unit = preds.put(ctx, new ExprComp(ctx.expComp.getText, getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))
  override def exitPredLiteral(ctx: BilParser.PredLiteralContext): Unit = preds.put(ctx, ctx.getText match {
    case "True" => Bool.True
    case "False" => Bool.False
  })

  override def exitGamma(ctx: GammaContext): Unit = (getExpr(ctx.`var`), ctx.LOW, ctx.HIGH) match {
    case (v: Var, _: TerminalNode, null) => gammaMappings.put(v, Low)
    case (v: Var, null, _: TerminalNode) => gammaMappings.put(v, High)
  }
  
  private def uniquePc () =
    pcCount += 1
    "l" + pcCount
}
