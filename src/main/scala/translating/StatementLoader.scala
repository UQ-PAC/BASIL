package translating

import astnodes.*
import BilParser.BilParser.*
import BilParser.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeProperty, TerminalNode}
import FlowGraph.Function
import vcgen.State
import util.AssumptionViolationException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

// TODO create a statementloaderstate class
class StatementLoader() extends BilBaseListener {
  val stmts: ArrayBuffer[Stmt] = ArrayBuffer[Stmt]()
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


  val varSizes: mutable.Map[String, Int] = mutable.Map[String, Int]()

  var rely: Option[Pred] = None

  /*
  private def getVarSize(name: String, rhs: Option[Expr]): Int = {
    if (!varSizes.contains(name)) {
      if (rhs.isEmpty) throw new AssumptionViolationException("")
      if (name.charAt(0) == 'R') varSizes(name) = 64
      else varSizes(name) = rhs.get.size.get
    }
    varSizes(name)
  }
  */

  /**
   * TODO: fix samples so 'X' is unnecessary; confirm pointer sizes for SP, FP, & LR
   */
  private def setVarSize(name: String, rhs: Expr): Unit = {
    if (!varSizes.contains(name)) {
      if (name.charAt(0) == 'R' || name.charAt(0) == 'X') varSizes(name) = 64
      else if (name.charAt(0) == '#') varSizes(name) = rhs.size.get
      else if (name == "NF" || name == "ZF" || name == "CF" || name == "VF") varSizes(name) = 1
      else if (name == "SP" || name == "FP" || name == "LR") varSizes(name) = 64
      else {
        println(name)
        throw new RuntimeException
      }
    }
  }


  // the last function header parsed; needed for assigning parameters
  private var currentFunction: Option[EnterSub] = None

  // for generating unique labels
  private var pcCount = 0

  def typeToSize(typeStr: String): Int = typeStr match {
    case "u32" => 32
    case "u64" => 64
    case _ => ???
  }

  var requires: List[Pred] = List()
  var ensures: List[Pred] = List()

  override def exitSub(ctx: BilParser.SubContext): Unit = {
    currentFunction match {
      case Some(f) =>
        f.requires = requires
        f.ensures = ensures
      case None =>
    }

    requires = List()
    ensures = List()

    val address = ctx.addr.getText
    val name = ctx.functionName.getText

    val function = EnterSub(address, name, List(), List())
    stmts += function
    this.currentFunction = Some(function)
  }
  
  override def exitParamTypes(ctx: BilParser.ParamTypesContext): Unit = {
    val id = ctx.param.getText // human-readable name
    val variable = ctx.`var`.getText // some register, probably

    val size = typeToSize(ctx.nat.getText)

    if (id.contains("result")) {
      currentFunction.get.outParam = Some(OutParameter(Register(id, size), Register(variable, 64)))
    } else {
      currentFunction.get.inParams += InParameter(Register(id, size), Register(variable, size))
    }
  }

  /*
  TODO!!!!
  override def exitEnsuresSpec(ctx: BilParser.EnsuresSpecContext): Unit = ensures = ensures.appended(getPred(ctx.pred))
  override def exitRequiresSpec(ctx: BilParser.RequiresSpecContext): Unit = requires = requires.appended(getPred(ctx.pred))
  */

  override def exitStmt(ctx: BilParser.StmtContext): Unit = {
    val address = ctx.addr.getText
// statements can be assignments, jumps, conditional jumps or function calls
    if (ctx.assign != null) { // statement is assignment; check which type
      val assignCtx = ctx.assign
      val LHS = getExpr(assignCtx.`var`)
      val RHS = getExpr(assignCtx.exp)

      stmts += ((LHS, RHS) match {
        case (v: Register, m: MemStore) =>
          if (v.name == "mem") MemAssign(address, MemLoad(m.loc, m.size), m.expr)
          else throw new AssumptionViolationException("expected mem for memstore")
        case (v: Register, _) =>
          setVarSize(v.name, RHS)
          RegisterAssign(address, v.copy(size = Some(varSizes(v.name))), RHS)
        case _ => throw new AssumptionViolationException("Unexpected expression")
      })

      /*
      if (assignCtx.exp.getClass == classOf[BilParser.ExpLoadContext]) { // statement is a load assignment
        val loadCtx = assignCtx.exp.asInstanceOf[BilParser.ExpLoadContext]
        val lhs = new Var(loadCtx.exp(0).getText)
        val rhs = getExpr(loadCtx.exp(1))
        if (rhs != null) { // null check is necessary as rhs may not exist for loads
          stmts += new Load(address, lhs, rhs.asInstanceOf[MemExpr])
        }
      } else if (assignCtx.exp.getClass == classOf[BilParser.ExpStoreContext]) { // statement is a store assignment
        val storeCtx = assignCtx.exp.asInstanceOf[BilParser.ExpStoreContext]
        val lhs = new MemExpr(getExpr(storeCtx.exp(0)))
        val rhs = getExpr(storeCtx.exp(1))
        stmts += new Store(address, lhs, rhs)
      } else { // statement is a move assignment
        val lhs = new Var(assignCtx.`var`.getText)
        val rhs = getExpr(assignCtx.exp)
        stmts += new Move(address, lhs, rhs)
      }
      */
    } else if (ctx.jmp != null) { // statement is a jump
      var target = ""
      if (ctx.jmp.`var` != null) target = ctx.jmp.`var`.getText
      else if (ctx.jmp.addr != null) target = ctx.jmp.addr.getText
      stmts += JmpStmt(address, target)
    } else if (ctx.cjmp != null) { // statement is a conditional jump
      val cond = Register(ctx.cjmp.`var`.getText, varSizes(ctx.cjmp.`var`.getText)) // conditions are always vars
      val target = ctx.cjmp.addr.getText
      stmts += CJmpStmt(address, target, "TODO", cond)
    } else if (ctx.call != null) { // statement is a call
      if (ctx.call.functionName == null) { // occasionally this occurs with "call LR with no return" lines
        stmts += ExitSub(ctx.addr.getText, None)
      } else {
        val funcName = ctx.call.functionName.getText
        stmts += CallStmt(address, funcName, Option(ctx.call.returnaddr.addr).map(_.getText), List(), None)
      }
    } else { // this statement is empty
      stmts += SkipStmt(address)
    }
  }

  override def exitExpBracket(ctx: BilParser.ExpBracketContext): Unit = exprs.put(ctx, getExpr(ctx.exp))
  
  override def exitExpUop(ctx: BilParser.ExpUopContext): Unit =
    exprs.put(ctx, UniOp(UniOperator.fromBil(ctx.uop.getText), getExpr(ctx.exp)))
    
  override def exitExpBop(ctx: BilParser.ExpBopContext): Unit =
    exprs.put(ctx, BinOp(BinOperator.fromBil(ctx.bop.getText), getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))
    
  override def exitVar(ctx: BilParser.VarContext): Unit =
    exprs.put(ctx, new Register(ctx.getText, varSizes.get(ctx.getText)))
    
  override def exitExpVar(ctx: BilParser.ExpVarContext): Unit = exprs.put(ctx, exprs.get(ctx.`var`))
  
  override def exitExpLiteral(ctx: BilParser.ExpLiteralContext): Unit = exprs.put(ctx, Literal(ctx.literal.getText))
  
  override def exitExpExtract(ctx: BilParser.ExpExtractContext): Unit = {
    val firstNat = ctx.nat(0).getText.toInt
    val secondNat = ctx.nat(1).getText.toInt
    val exp = getExpr(ctx.exp)
    exprs.put(ctx, Extract(firstNat, secondNat, exp))
  }

  // Currently doesn't handle high/low. these could be converted to extracts
  override def exitExpCast(ctx: BilParser.ExpCastContext): Unit = ctx.CAST().getText match {
    case "pad" => exprs.put(ctx, Pad(getExpr(ctx.exp), ctx.nat.getText.toInt))
    case "extend" => exprs.put(ctx, Extend(getExpr(ctx.exp), ctx.nat.getText.toInt))
    case "low" | "high" => exprs.put(ctx, getExpr(ctx.exp))
  }

  override def exitExpLoad(ctx: BilParser.ExpLoadContext): Unit =
    if (ctx.exp(0).getText == "mem") exprs.put(ctx, MemLoad(getExpr(ctx.exp(1)), Some(typeToSize(ctx.nat.getText))))
    else throw new AssumptionViolationException("Found load on variable other than mem")
    
  override def exitExpLoad8(ctx: BilParser.ExpLoad8Context): Unit =
    if (ctx.exp(0).getText == "mem") exprs.put(ctx, MemLoad(getExpr(ctx.exp(1)), Some(8)))
    else throw new AssumptionViolationException("Found load on variable other than mem")
    
  override def exitExpStore(ctx: BilParser.ExpStoreContext): Unit =
    if (ctx.exp(0).getText == "mem") {
      exprs.put(ctx, MemStore(getExpr(ctx.exp(1)), getExpr(ctx.exp(2)), Some(typeToSize(ctx.nat.getText))))
    } else throw new AssumptionViolationException("Found store on variable other than mem")
    
  override def exitExpStore8(ctx: BilParser.ExpStore8Context): Unit =
    if (ctx.exp(0).getText == "mem") exprs.put(ctx, MemStore(getExpr(ctx.exp(1)), getExpr(ctx.exp(2)), Some(8)))
    else throw new AssumptionViolationException("Found store on variable other than mem")
    
  override def exitExpFunctionCall(ctx: BilParser.ExpFunctionCallContext): Unit =
    exprs.put(ctx, FunctionCall("old", ctx.argList.exp.asScala.map(a => getExpr(a)).toList))

  override def exitPredBinOp(ctx: PredBinOpContext): Unit =
    preds.put(ctx, new PredBinOp(ctx.predBop.getText, getPred(ctx.pred(0)), getPred(ctx.pred(1))))
    
  override def exitPredUniOp(ctx: PredUniOpContext): Unit = 
    preds.put(ctx, PredUniOp(ctx.uop.getText, getPred(ctx.pred)))
    
  override def exitPredBracket(ctx: PredBracketContext): Unit = preds.put(ctx, getPred(ctx.pred))
  
  override def exitPredExprComp(ctx: PredExprCompContext): Unit =
    preds.put(ctx, ExprComp(ctx.expComp.getText, getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))
  /* override def exitPredLiteral(ctx: BilParser.PredLiteralContext): Unit = preds.put(ctx, ctx.getText match {
    case "TRUE" => Bool.True
    case "FALSE" => Bool.False
  }) */

  override def exitSecLatticeElem(ctx: SecLatticeElemContext): Unit = secs.put(ctx, SecVar(ctx.getText))

  override def exitGamma(ctx: GammaContext): Unit = getExpr(ctx.`var`) match {
    case v: Register => gammaMappings.put(v, SecVar(ctx.ID.getText))
  }

  override def exitLpred(ctx: BilParser.LpredContext): Unit = (getExpr(ctx.`var`), getSec(ctx.secExpr)) match {
    case (v: Register, p: Sec) => lPreds.put(v, p)
  }

  override def exitRely(ctx: BilParser.RelyContext): Unit = rely = Some(getPred(ctx.pred))
  
  private def uniquePc() = {
    pcCount += 1
    "l" + pcCount
  }
}
