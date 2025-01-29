package analysis
import ir.*
import util.Logger
import ir.eval.BitVectorEval

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
  def value(v: Variable) = constantPropResult(v) match {
        case FlatEl(value) => Some(value)
        case _             => None
  }

  ir.eval.evalBVExpr(exp, value) match {
    case Right(v) => Some(v)
    case Left(_) => None
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

def getSSADefinition(variable: Variable, node: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]): FlatElement[Int] = {
  val (in, _) = reachingDefs(node)
  in(variable)
}

def getSSAUse(variable: Variable, node: CFGPosition, reachingDefs: Map[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])]): FlatElement[Int] = {
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

def bitVectorOpToBigIntOp(op: BinOp, lhs: BigInt, rhs: BigInt): BigInt = {
  op match {
    case BVADD => lhs + rhs
    case BVSUB => lhs - rhs
    case _ => throw RuntimeException("Binary operation support not implemented: " + op)
  }
}
