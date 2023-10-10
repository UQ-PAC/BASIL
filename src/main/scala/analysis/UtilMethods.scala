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
def evaluateExpression(exp: Expr, n: CfgNode, constantPropResult: Map[Variable, ConstantPropagationLattice.type]): Expr = {
  Logger.debug(s"evaluateExpression: $exp")
  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpression(binOp.arg1, n, constantPropResult)
      val rhs = evaluateExpression(binOp.arg2, n, constantPropResult)

      (lhs, rhs) match {
        case (l: BitVecLiteral, r: BitVecLiteral) =>
          binOp.op match {
            case BVADD => BitVectorEval.smt_bvadd(l, r)
            case BVSUB => BitVectorEval.smt_bvsub(l, r)
            case BVASHR => BitVectorEval.smt_bvashr(l, r)
            case BVCOMP => BitVectorEval.smt_bvcomp(l, r)
            case _ => throw new RuntimeException("Binary operation support not implemented: " + binOp.op)
          }
        case _ => exp
      }
    case extend: ZeroExtend =>
      evaluateExpression(extend.body, n, constantPropResult) match {
        case literal: Literal => BitVectorEval.smt_zero_extend(extend.extension, literal)
        case _                => exp
      }
    case e: Extract =>
      evaluateExpression(e.body, n, constantPropResult) match {
        case literal: Literal => BitVectorEval.smt_extract(e.end, e.start, literal)
        case _                => exp
      }
    case variable: Variable =>
      val nodeResult = constantPropResult
      nodeResult(variable).asInstanceOf[ConstantPropagationLattice.Element] match {
        case ConstantPropagationLattice.FlatElement.FlatEl(value) => value.asInstanceOf[BitVecLiteral]
        case ConstantPropagationLattice.FlatElement.Top           => variable
        case ConstantPropagationLattice.FlatElement.Bot           => variable
      }
    case _ => //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
      exp
  }
}
