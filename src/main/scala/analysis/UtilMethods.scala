package analysis
import ir.*
import util.Logger
import ir.eval.BitVectorEval

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
  def value(v: Variable) = constantPropResult(v) match {
        case FlatEl(value) => Some(value)
        case _             => None
  }

  ir.eval.evalBVExpr(exp, value) match {
    case Right(v) => Some(v)
    case Left(_) => None
  }
}

def evaluateExpressionWithSSA(exp: Expr, constantPropResult: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], n: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]): Set[BitVecLiteral] = {
  Logger.debug(s"evaluateExpression: $exp")

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

def unwrapExpr(expr: Expr): Set[Expr] = {
  var buffers: Set[Expr] = Set()
  expr match {
    case e: Extract => buffers ++= unwrapExpr(e.body)
    case e: SignExtend => buffers ++= unwrapExpr(e.body)
    case e: ZeroExtend => buffers ++= unwrapExpr(e.body)
    case repeat: Repeat => buffers ++= unwrapExpr(repeat.body)
    case unaryExpr: UnaryExpr => buffers ++= unwrapExpr(unaryExpr.arg)
    case binaryExpr: BinaryExpr =>
      buffers ++= unwrapExpr(binaryExpr.arg1)
      buffers ++= unwrapExpr(binaryExpr.arg2)
    case memoryLoad: MemoryLoad =>
      buffers += memoryLoad
      buffers ++= unwrapExpr(memoryLoad.index)
    case _ =>
  }
  buffers
}
