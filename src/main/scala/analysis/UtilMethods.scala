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
def evaluateExpression(
  exp: Expr,
  constantPropResult: Map[Variable, FlatElement[BitVecLiteral]]
): Option[BitVecLiteral] = {
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
        case None => None
      }
    case extend: SignExtend =>
      evaluateExpression(extend.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.smt_sign_extend(extend.extension, b))
        case None => None
      }
    case e: Extract =>
      evaluateExpression(e.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.boogie_extract(e.end, e.start, b))
        case None => None
      }
    case variable: Variable =>
      constantPropResult(variable) match {
        case FlatEl(value) => Some(value)
        case Top => None
        case Bottom => None
      }
    case b: BitVecLiteral => Some(b)
    case _ => // throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
      None
  }
}

def getDefinition(
  variable: Variable,
  node: CFGPosition,
  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
): Set[Assign] = {
  val (in, _) = reachingDefs(node)
  in.getOrElse(variable, Set())
}

def getUse(
  variable: Variable,
  node: CFGPosition,
  reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
): Set[Assign] = {
  val (_, out) = reachingDefs(node)
  out.getOrElse(variable, Set())
}

def getSSADefinition(
  variable: Variable,
  node: CFGPosition,
  reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
): FlatElement[Int] = {
  val (in, _) = reachingDefs(node)
  in(variable)
}

def getSSAUse(
  variable: Variable,
  node: CFGPosition,
  reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]
): FlatElement[Int] = {
  val (_, out) = reachingDefs(node)
  out(variable)
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
    case _ =>
      None
  }
}

def bitVectorOpToBigIntOp(op: BinOp, lhs: BigInt, rhs: BigInt): BigInt =
  op match {
    case BVADD => lhs + rhs
    case BVSUB => lhs - rhs
    case _ => throw RuntimeException("Binary operation support not implemented: " + op)
  }
