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
            case BVCONCAT => Some(BitVectorEval.smt_concat(l, r))
            case x =>
              Logger.error("Binary operation support not implemented: " + binOp.op)
              None
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

def evaluateExpressionWithSSA(exp: Expr, constantPropResult: Map[RegisterWrapperEqualSets, Set[BitVecLiteral]], n: CfgNode, reachingDefs: Map[CfgNode, (Map[Variable, Option[LocalAssign]], Map[Variable, Set[LocalAssign]])]): Set[BitVecLiteral] = {
  Logger.debug(s"evaluateExpression: $exp")

  def apply(op: (BitVecLiteral, BitVecLiteral) => BitVecLiteral, a: Set[BitVecLiteral], b: Set[BitVecLiteral]): Set[BitVecLiteral] =
    val res = for {
      x <- a
      y <- b
    } yield op(x, y)
    res

  def applySingle(op: BitVecLiteral => BitVecLiteral, a: Set[BitVecLiteral]): Set[BitVecLiteral] =
    val res = for {
      x <- a
    } yield op(x)
    res


  exp match {
    case binOp: BinaryExpr =>
      val lhs = evaluateExpressionWithSSA(binOp.arg1, constantPropResult, n, reachingDefs)
      val rhs = evaluateExpressionWithSSA(binOp.arg2, constantPropResult, n, reachingDefs)

      (lhs, rhs) match {
        case (l: Set[BitVecLiteral], r: Set[BitVecLiteral]) =>
          binOp.op match {
            case BVADD => apply(BitVectorEval.smt_bvadd, lhs, rhs)
            case BVSUB => apply(BitVectorEval.smt_bvsub, lhs, rhs)
            case BVASHR => apply(BitVectorEval.smt_bvashr, lhs, rhs)
            case BVCOMP => apply(BitVectorEval.smt_bvcomp, lhs, rhs)
            case BVCONCAT => apply(BitVectorEval.smt_concat, lhs, rhs)
            case BVLSHR => apply(BitVectorEval.smt_bvlshr, lhs, rhs)
            case BVSHL => apply(BitVectorEval.smt_bvshl, lhs, rhs)
            case BVOR => apply(BitVectorEval.smt_bvor, lhs, rhs)
            case _ => throw new RuntimeException("Binary operation support not implemented: " + binOp.op)
          }
      }
    case unaryExpr: UnaryExpr =>
      val result = evaluateExpressionWithSSA(unaryExpr.arg, constantPropResult, n, reachingDefs)
      unaryExpr.op match {
        case BVNEG =>
          applySingle(BitVectorEval.smt_bvneg, result)
        case BVNOT =>
          applySingle(BitVectorEval.smt_bvnot, result)
        case _ => throw new RuntimeException("Unary operation support not implemented: " + unaryExpr.op)
      }
    case extend: ZeroExtend =>
      val result = evaluateExpressionWithSSA(extend.body, constantPropResult, n, reachingDefs)
      applySingle(BitVectorEval.smt_zero_extend(extend.extension, _: BitVecLiteral), result)
    case e: Extract =>
      val result = evaluateExpressionWithSSA(e.body, constantPropResult, n, reachingDefs)
      applySingle(BitVectorEval.boogie_extract(e.end, e.start, _: BitVecLiteral), result)
    case variable: Variable =>
      constantPropResult(RegisterWrapperEqualSets(variable, getDefs(variable, n, reachingDefs)))
    case b: BitVecLiteral => Set(b)
    case _ => throw new RuntimeException("ERROR: CASE NOT HANDLED: " + exp + "\n")
  }
}

def getUses(variable: Variable, node: CfgNode, reachingDefs: Map[CfgNode, (Map[Variable, Option[LocalAssign]], Map[Variable, Set[LocalAssign]])]): Option[LocalAssign] = {
  val (in, _) = reachingDefs(node)
  in(variable)
}

def getDefs(variable: Variable, node: CfgNode, reachingDefs: Map[CfgNode, (Map[Variable, Option[LocalAssign]], Map[Variable, Set[LocalAssign]])]): Set[LocalAssign] = {
  val (_, out) = reachingDefs(node)
  out(variable)
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