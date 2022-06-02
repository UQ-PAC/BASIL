package translating

import BilParser.BilAdtParser.{DirectContext, ExpContext, ExpIntAdtContext, ExpVarContext, IndirectContext}
import BilParser.{BilAdtBaseListener, BilAdtListener, BilAdtParser}
import astnodes.exp.{
  BinOp,
  BinOperator,
  Expr,
  Extend,
  Extract,
  FunctionCall,
  Literal,
  MemStore,
  Pad,
  UniOp,
  UniOperator
}
import astnodes.exp.`var`.{MemLoad, Register, Var}
import astnodes.parameters.OutParameter
import astnodes.parameters.InParameter
import astnodes.pred.Pred
import astnodes.sec.{Sec, SecVar}
import astnodes.stmt.assign.{MemAssign, RegisterAssign}
import astnodes.stmt.{CJmpStmt, CallStmt, EnterSub, ExitSub, JmpStmt, SkipStmt, Stmt}
import org.antlr.v4.runtime
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, ParseTree, ParseTreeProperty, TerminalNode}
import util.AssumptionViolationException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AdtListener extends BilAdtBaseListener {

  val stmts = ArrayBuffer[Stmt]()
  private val exprs: ParseTreeProperty[Expr] = ParseTreeProperty[Expr]

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

  /** For some reason, numbers in the ADT are stored as (Num, Type) where Num is the unsigned representation of the
    * number, and type is the type (e.g. a type of 64 means 64-bit two's complement). It's assumed that it is always
    * two's complement.
    *
    * For instance - Int(18446744073709551584,64) means -32.
    *
    * TODO: This takes and returns strings to conform with the current state. It should be converted to take and return
    * BigInts long term
    */
  def castTo2sComplement(num: String, t: String): String = {
    val n = BigInt(num);
    val exponent = t.toInt;
    if (n >= BigInt.apply(2).pow(exponent - 1)) {
      // This is a negative 2's complement number
      return (BigInt.apply(2).pow(exponent) - n).toString
    } else {
      return n.toString
    }
  }

  /** Remove the quotation marks from a string e.g. "\"mem\"" becomes "mem"
    */
  def getStringBody(str: String): String = {
    var newStr: String = str;
    if (newStr.charAt(0) == '"') {
      newStr = newStr.substring(1);
    }
    if (newStr.charAt(newStr.length - 1) == '"') {
      newStr = newStr.substring(0, newStr.length - 1);
    }
    return newStr;
  }

  override def exitExpLoad(ctx: BilAdtParser.ExpLoadContext): Unit = {
    ctx.memexp match {
      case v: ExpVarContext =>
        if (getStringBody(v.STRING.getText) == "mem") {
          exprs.put(ctx, new MemLoad(getExpr(ctx.exp(1)), Some(ctx.NUM().getText.toInt)))
        } else {
          throw new AssumptionViolationException("Found load on on variable other than mem")
        }
      case v: ExpContext => throw new AssumptionViolationException("Expected ExpVarContext, but got " + v.getClass())
    }
  }

  override def exitExpStore(ctx: BilAdtParser.ExpStoreContext): Unit = {
    ctx.memexp match {
      case v: ExpVarContext =>
        if (getStringBody(v.STRING.getText) == "mem") {
          exprs.put(ctx, new MemStore(getExpr(ctx.idx), getExpr(ctx.value), Some(ctx.NUM().getText.toInt)))
        } else {
          throw new AssumptionViolationException("Found store on on variable other than mem")
        }
      case v: ExpContext => throw new AssumptionViolationException("Expected ExpVarContext, but got " + v.getClass())
    }
  }

  override def exitExpBinop(ctx: BilAdtParser.ExpBinopContext): Unit =
    exprs.put(ctx, new BinOp(BinOperator.fromAdt(ctx.BINOP.getText), getExpr(ctx.exp(0)), getExpr(ctx.exp(1))))

  override def exitExpUop(ctx: BilAdtParser.ExpUopContext): Unit =
    exprs.put(ctx, new UniOp(UniOperator.fromAdt(ctx.UOP.getText), getExpr(ctx.exp)))

  override def exitExpVar(ctx: BilAdtParser.ExpVarContext): Unit = {
    if (getStringBody(ctx.STRING.getText) != "mem") { // Is a register
      if (ctx.`type`.imm == null)
        throw new AssumptionViolationException("Can't find Imm argument for a non-mem variable")
      exprs.put(ctx, new Register(getStringBody(ctx.name.getText), Some(ctx.`type`.imm.NUM.getText.toInt)))
      varSizes.put(getStringBody(ctx.name.getText), ctx.`type`.imm.NUM.getText.toInt)
    } else {
      // Old parser treated mem as a register so I've replicated that behaviour here - could be unintentional
      exprs.put(ctx, new Register(getStringBody(ctx.name.getText), Some(ctx.`type`.mem.NUM(1).getText.toInt)))
    }
  }

  override def exitExpIntAdt(ctx: BilAdtParser.ExpIntAdtContext): Unit = {
    exprs.put(
      ctx,
      Literal(ctx.NUM(0).getText)
    ) // We also have access to size information if needed. But current AST node doesn't ask for it
  }

  // Currently doesn't handle high/low. These could be converted to extracts
  // TODO: Adt doesn't understand what pad and extend is - It might infer it from the relative size of the cast and
  // TODO: the thing being casted
  override def exitExpCast(ctx: BilAdtParser.ExpCastContext): Unit = {
    ctx.CAST.getText match {
      case "UNSIGNED"     => exprs.put(ctx, Pad(getExpr(ctx.exp), ctx.NUM.getText.toInt))
      case "SIGNED"       => exprs.put(ctx, Extend(getExpr(ctx.exp), ctx.NUM.getText.toInt))
      case "LOW" | "HIGH" => exprs.put(ctx, getExpr(ctx.exp))
    }
  }

  override def exitExpExtract(ctx: BilAdtParser.ExpExtractContext): Unit = {
    val first = ctx.NUM(0).getText.toInt;
    val second = ctx.NUM(1).getText.toInt;
    val exp = getExpr(ctx.exp)
    exprs.put(ctx, new Extract(first, second, exp));
  }

  override def exitDef(ctx: BilAdtParser.DefContext): Unit = {
    val address = getStringBody(ctx.tid.name.getText);
    val LHS = getExpr(ctx.exp(0))
    val RHS = getExpr(ctx.exp(1))

    stmts += ((LHS, RHS) match {
      case (v: Register, m: MemStore) =>
        if (v.name == "mem") new MemAssign(address, new MemLoad(m.loc, m.size), m.expr)
        else throw new AssumptionViolationException("expected mem for memstore")
      case (v: Register, _) => {
        new RegisterAssign(address, v, RHS)
      }
      case _ => throw new AssumptionViolationException("Unexpected expression")

    })
  }

  override def exitExpParen(ctx: BilAdtParser.ExpParenContext): Unit = {
    exprs.put(ctx, getExpr(ctx.exp))
  }

  override def exitGotoSym(ctx: BilAdtParser.GotoSymContext): Unit = {
    // Note in the ADT, all jumps are actually conditoinal jumps. Regular
    // jumps simply have true/Int(1,1) as the condition.

    val address = getStringBody(ctx.tid.name.getText);
    ctx.cond match {
      case v: ExpVarContext => {
        val cond = Register(getStringBody(v.name.getText), v.asInstanceOf[ExpVarContext].`type`.imm.NUM.getText.toInt);
        if (ctx.target.indirect != null) {
          val variable: ExpVarContext = ctx.target.indirect.exp.asInstanceOf[ExpVarContext]
          stmts += new CJmpStmt(address, getStringBody(variable.name.getText), "TODO", cond);
        } else if (ctx.target.direct != null) {
          stmts += new CJmpStmt(address, getStringBody(ctx.target.direct.tid.name.getText), "TODO", cond);
        }
      }
      case v: ExpIntAdtContext => {
        // Assume that if it's an int, the int evaluates to true. This seems to be the case
        // 99% of the time. It probably doesn't make sense to a cjump based on a falsy literal.
        if (ctx.target.indirect != null) {
          val variable: ExpVarContext = ctx.target.indirect.exp.asInstanceOf[ExpVarContext]
          stmts += new JmpStmt(address, getStringBody(variable.name.getText))
        } else if (ctx.target.direct != null) {
          stmts += new JmpStmt(address, getStringBody(ctx.target.direct.tid.name.getText))
        }
      }
    }
  }

  override def exitBlk(ctx: BilAdtParser.BlkContext): Unit = {
    // COMPATIBLITY - The previous bil parser had no real notion of "blocks" the blocks were simply started by
    // a labelled skip statement
    stmts += new SkipStmt(getStringBody(ctx.tid.name.getText));
  }

  override def exitCall(ctx: BilAdtParser.CallContext): Unit = {
    val address = getStringBody(ctx.tid.name.getText);
    var funcName = "";
    if (ctx.calee.direct != null) {
      funcName = getStringBody(ctx.calee.direct.tid.name.getText);
      stmts += new CallStmt(address, funcName, Option(ctx.returnSym).map(_.getText), List(), None)
    } else if (ctx.calee.indirect != null) {
      // TODO: This mimics the behaviour of the old parser - it could be unintended
      stmts += new ExitSub(address, None)
    }
  }

  override def exitSub(ctx: BilAdtParser.SubContext): Unit = {
    if (currentFunction != null)
      currentFunction.setRequiresEnsures(requires, ensures)

    requires = List()
    ensures = List()
    val address = getStringBody(ctx.tid.name.getText);
    val name = getStringBody(ctx.name.getText)

    val function = new EnterSub(address, name, List(), List())
    stmts += function
    this.currentFunction = function
    for (i <- 0 to ctx.args().arg.size() - 1) {
      var arg = ctx.args.arg(i)

      val id = getStringBody(arg.tid.name.getText)
      arg.rhs match {
        case v: ExpVarContext => {
          val name = v.name.getText
          val size = v.`type`.imm.NUM.getText.toInt
          if (arg.intent.getText.contains("in")) {
            currentFunction.getInParams += new InParameter(Register(id, size), Register(name, 64))
          } else {
            currentFunction.setOutParam(new OutParameter(Register(id, size), Register(name, 64)))
          }
        }
        case _ => throw new AssumptionViolationException("Expected RHS of arg to be a varible")
      }
    }
  }
}
