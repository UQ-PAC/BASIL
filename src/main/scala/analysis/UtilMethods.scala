package analysis.eval
import ir.*
import analysis.solvers.*
import analysis.Lattice
import analysis.*

/**
 * Evaluate an expression in a hope of finding a global variable.
 *
 * @param exp : The expression to evaluate (e.g. R1 + 0x1234)
 * @param n   : The node where the expression is evaluated (e.g. mem[R1 + 0x1234] <- ...)
 * @return: The evaluated expression (e.g. 0x69632) */
def evaluateExpression(exp: Expr, n: CfgNode, constantProp: Map[CfgNode, Map[Variable, Any]]): Expr = {
  println(s"evaluateExpression: $exp")
  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpression(binOp.arg1, n, constantProp)
      val rhs = evaluateExpression(binOp.arg2, n, constantProp)
      if (!lhs.isInstanceOf[BitVecLiteral] || !rhs.isInstanceOf[BitVecLiteral]) {
        return exp
      }
      binOp.op match
        case BVADD => BitVectorEval.smt_bvadd(lhs.asInstanceOf[BitVecLiteral], rhs.asInstanceOf[BitVecLiteral])
        case BVSUB => BitVectorEval.smt_bvsub(lhs.asInstanceOf[BitVecLiteral], rhs.asInstanceOf[BitVecLiteral])
        case BVASHR => BitVectorEval.smt_bvashr(lhs.asInstanceOf[BitVecLiteral], rhs.asInstanceOf[BitVecLiteral])
        case BVCOMP => BitVectorEval.smt_bvcomp(lhs.asInstanceOf[BitVecLiteral], rhs.asInstanceOf[BitVecLiteral])
        case _ => throw new RuntimeException("Binary operation support not implemented: " + binOp.op)
    case extend: ZeroExtend => evaluateExpression(extend.body, n, constantProp) match {
      case literal: Literal => BitVectorEval.smt_zero_extend(extend.extension, literal)
      case _ => exp
    }
    case e: Extract => evaluateExpression(e.body, n, constantProp) match {
      case literal: Literal => BitVectorEval.smt_extract(e.end, e.start, literal)
      case _ => exp
    }
    case variable: Variable => val nodeResult = constantProp(n).asInstanceOf[Map[Variable, ConstantPropagationLattice.type]]
      nodeResult(variable).asInstanceOf[ConstantPropagationLattice.Element] match {
        case ConstantPropagationLattice.FlatElement.FlatEl(value) => value.asInstanceOf[BitVecLiteral]
        case ConstantPropagationLattice.FlatElement.Top => variable
        case ConstantPropagationLattice.FlatElement.Bot => variable
      }
    case _ => //throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
      exp
  }
}
