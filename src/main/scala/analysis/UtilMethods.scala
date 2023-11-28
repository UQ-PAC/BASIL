package analysis
import ir.*
import util.Logger

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
  Logger.debug(s"evaluateExpression: $exp")
  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpression(binOp.arg1, constantPropResult)
      val rhs = evaluateExpression(binOp.arg2, constantPropResult)

      (lhs, rhs) match {
        case (Some(l: BitVecLiteral), Some(r: BitVecLiteral)) =>
          binOp.op match {
            case BVADD => Some(BitVectorEval.smt_bvadd(l, r))
            case BVSUB => Some(BitVectorEval.smt_bvsub(l, r))
            case BVASHR => Some(BitVectorEval.smt_bvashr(l, r))
            case BVCOMP => Some(BitVectorEval.smt_bvcomp(l, r))
            case x => {
              Logger.error("Binary operation support not implemented: " + binOp.op)
              None
            }

          }
        case _ => None
      }
    case extend: ZeroExtend =>
      evaluateExpression(extend.body, constantPropResult) match {
        case Some(b: BitVecLiteral) => Some(BitVectorEval.smt_zero_extend(extend.extension, b))
        case None                => None
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
        case Bottom           => None
      }
    case b: BitVecLiteral => Some(b)
    case _ => //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
      None
  }
}
