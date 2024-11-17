package analysis
import ir.*
import util.Logger

import scala.annotation.tailrec

/** Evaluate an expression in a hope of finding a global variable.
  *
  * @param exp
  *   : The expression to evaluate (e.g. R1 + 0x1234)
  * @param n
  *   : The node where the expression is evaluated (e.g. mem[R1 + 0x1234] <- ...)
  * @return:
  *   The evaluated expression (e.g. 0x69632)
  */
def evaluateExpression(exp: Expr, constantPropResult: Map[Variable, FlatElement[BitVecLiteral]]): Option[BitVecLiteral] = {
  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpression(binOp.arg1, constantPropResult)
      val rhs = evaluateExpression(binOp.arg2, constantPropResult)

      (lhs, rhs) match {
        case (Some(l: BitVecLiteral), Some(r: BitVecLiteral)) =>
          val result = binOp.op match {
            case BVADD => BitVectorEval.smt_bvadd(l, r)
            case BVSUB => BitVectorEval.smt_bvsub(l, r)
            case BVMUL => BitVectorEval.smt_bvmul(l, r)
            case BVUDIV => BitVectorEval.smt_bvudiv(l, r)
            case BVSDIV => BitVectorEval.smt_bvsdiv(l, r)
            case BVSREM => BitVectorEval.smt_bvsrem(l, r)
            case BVUREM => BitVectorEval.smt_bvurem(l, r)
            case BVSMOD => BitVectorEval.smt_bvsmod(l, r)
            case BVAND => BitVectorEval.smt_bvand(l, r)
            case BVOR => BitVectorEval.smt_bvxor(l, r)
            case BVXOR => BitVectorEval.smt_bvxor(l, r)
            case BVNAND => BitVectorEval.smt_bvnand(l, r)
            case BVNOR => BitVectorEval.smt_bvnor(l, r)
            case BVXNOR => BitVectorEval.smt_bvxnor(l, r)
            case BVSHL => BitVectorEval.smt_bvshl(l, r)
            case BVLSHR => BitVectorEval.smt_bvlshr(l, r)
            case BVASHR => BitVectorEval.smt_bvashr(l, r)
            case BVCOMP => BitVectorEval.smt_bvcomp(l, r)
            case BVCONCAT => BitVectorEval.smt_concat(l, r)
            case x => throw RuntimeException("Binary operation support not implemented: " + binOp.op)
          }
          Some(result)
        case _ => None
      }
    case extend: ZeroExtend =>
      evaluateExpression(extend.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.smt_zero_extend(extend.extension, b))
        case None                => None
      }
    case extend: SignExtend =>
      evaluateExpression(extend.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.smt_sign_extend(extend.extension, b))
        case None => None
      }
    case e: Extract =>
      evaluateExpression(e.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.boogie_extract(e.end, e.start, b))
        case None               => None
      }
    case variable: Variable =>
      constantPropResult(variable) match {
        case FlatEl(value) => Some(value)
        case Top           => None
        case Bottom        => None
      }
    case b: BitVecLiteral => Some(b)
    case _ => //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
      None
  }
}

def evaluateExpressionWithSSA(exp: Expr, constantPropResult: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], n: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]): Set[BitVecLiteral] = {
  def apply(op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral, a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] = {
    val res = for {
      x <- a
      y <- b
    } yield op(x, y)
    res
  }

  def applySingle(op: BitVecLiteral => BitVecLiteral, a: Set[BitVecLiteral]): Set[BitVecLiteral] = {
    val res = for {
      x <- a
    } yield op(x)
    res
  }

  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpressionWithSSA(binOp.arg1, constantPropResult, n, reachingDefs)
      val rhs = evaluateExpressionWithSSA(binOp.arg2, constantPropResult, n, reachingDefs)
      binOp.op match {
        case BVADD => apply(BitVectorEval.smt_bvadd, lhs, rhs)
        case BVSUB => apply(BitVectorEval.smt_bvsub, lhs, rhs)
        case BVMUL => apply(BitVectorEval.smt_bvmul, lhs, rhs)
        case BVUDIV => apply(BitVectorEval.smt_bvudiv, lhs, rhs)
        case BVSDIV => apply(BitVectorEval.smt_bvsdiv, lhs, rhs)
        case BVSREM => apply(BitVectorEval.smt_bvsrem, lhs, rhs)
        case BVUREM => apply(BitVectorEval.smt_bvurem, lhs, rhs)
        case BVSMOD => apply(BitVectorEval.smt_bvsmod, lhs, rhs)
        case BVAND => apply(BitVectorEval.smt_bvand, lhs, rhs)
        case BVOR => apply(BitVectorEval.smt_bvxor, lhs, rhs)
        case BVXOR => apply(BitVectorEval.smt_bvxor, lhs, rhs)
        case BVNAND => apply(BitVectorEval.smt_bvnand, lhs, rhs)
        case BVNOR => apply(BitVectorEval.smt_bvnor, lhs, rhs)
        case BVXNOR => apply(BitVectorEval.smt_bvxnor, lhs, rhs)
        case BVSHL => apply(BitVectorEval.smt_bvshl, lhs, rhs)
        case BVLSHR => apply(BitVectorEval.smt_bvlshr, lhs, rhs)
        case BVASHR => apply(BitVectorEval.smt_bvashr, lhs, rhs)
        case BVCOMP => apply(BitVectorEval.smt_bvcomp, lhs, rhs)
        case BVCONCAT => apply(BitVectorEval.smt_concat, lhs, rhs)
        case _ => throw RuntimeException("Binary operation support not implemented: " + binOp.op)
      }
    case unaryExpr: UnaryExpr =>
      val result = evaluateExpressionWithSSA(unaryExpr.arg, constantPropResult, n, reachingDefs)
      unaryExpr.op match {
        case BVNEG => applySingle(BitVectorEval.smt_bvneg, result)
        case BVNOT => applySingle(BitVectorEval.smt_bvnot, result)
        case _ => throw RuntimeException("Unary operation support not implemented: " + unaryExpr.op)
      }
    case extend: ZeroExtend =>
      val result = evaluateExpressionWithSSA(extend.body, constantPropResult, n, reachingDefs)
      applySingle(BitVectorEval.smt_zero_extend(extend.extension, _: BitVecLiteral), result)
    case extend: SignExtend =>
      val result = evaluateExpressionWithSSA(extend.body, constantPropResult, n, reachingDefs)
      applySingle(BitVectorEval.smt_sign_extend(extend.extension, _: BitVecLiteral), result)
    case e: Extract =>
      val result = evaluateExpressionWithSSA(e.body, constantPropResult, n, reachingDefs)
      applySingle(BitVectorEval.boogie_extract(e.end, e.start, _: BitVecLiteral), result)
    case variable: Variable =>
      Logger.debug("Variable: " + variable)
      Logger.debug("node: " + n)
      Logger.debug("reachingDefs: " + reachingDefs(n))
      Logger.debug("getUse: " + getUse(variable, n, reachingDefs))
      constantPropResult(RegisterWrapperEqualSets(variable, getUse(variable, n, reachingDefs)))
    case b: BitVecLiteral => Set(b)
    case Repeat(repeats, body) => evaluateExpressionWithSSA(body, constantPropResult, n, reachingDefs)
    case MemoryLoad(mem, index, endian, size) => Set.empty
    case UninterpretedFunction(name, params, returnType) => Set.empty
    case _ => throw RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
  }
}

def getDefinition(variable: Variable, node: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]): Set[Assign] = {
  val (in, _) = reachingDefs(node)
  in.getOrElse(variable, Set())
}

def getUse(variable: Variable, node: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]): Set[Assign] = {
  val (_, out) = reachingDefs(node)
  out.getOrElse(variable, Set())
}

/** Extracts a MemoryLoad from Expr. Assumes that a statement contains only one memory load.
  *
  * @param stmt
  *   The statement to extract the memory load from
  * @return
  *   The memory load if found, None otherwise
 */
def unwrapExpr(expr: Expr): Option[MemoryLoad] = {
  expr match {
    case e: Extract => unwrapExpr(e.body)
    case e: SignExtend => unwrapExpr(e.body)
    case e: ZeroExtend => unwrapExpr(e.body)
    case repeat: Repeat => unwrapExpr(repeat.body)
    case unaryExpr: UnaryExpr => unwrapExpr(unaryExpr.arg)
    case binaryExpr: BinaryExpr =>
      // assume one memory load
      val lhs = unwrapExpr(binaryExpr.arg1)
      if (lhs.isDefined) {
        return lhs
      }
      unwrapExpr(binaryExpr.arg2)
    case memoryLoad: MemoryLoad =>
      Some(memoryLoad)
    case _ =>
      None
  }
}

/** Extracts the variable from an expression (only one variable is expected otherwise None ie. in Binary Expression).
  *
  * @param expr
  *   The expression to extract the variable from
  * @return
  *   The variable if found, None otherwise
 */
@tailrec
def unwrapExprToVar(expr: Expr): Option[Variable] = {
  expr match {
    case variable: Variable =>
      Some(variable)
    case e: Extract => unwrapExprToVar(e.body)
    case e: SignExtend => unwrapExprToVar(e.body)
    case e: ZeroExtend => unwrapExprToVar(e.body)
    case repeat: Repeat => unwrapExprToVar(repeat.body)
    case unaryExpr: UnaryExpr => unwrapExprToVar(unaryExpr.arg)
    case binaryExpr: BinaryExpr => // TODO: handle multiple variables
      None
    case memoryLoad: MemoryLoad => unwrapExprToVar(memoryLoad.index)
    case _ =>
      None
  }
}

def bitVectorOpToBigIntOp(op: BinOp, lhs: BigInt, rhs: BigInt): BigInt = {
  op match {
    case BVADD => lhs + rhs
    case BVSUB => lhs - rhs
    case _ => throw RuntimeException("Binary operation support not implemented: " + op)
  }
}