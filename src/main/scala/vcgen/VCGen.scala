package vcgen

import facts.exp.{BinOp, Expr, Literal, Var}
import facts.stmt.Assign.Assign
import facts.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*
import facts.pred.{Bool, Pred}

object VCGen {
  def genVCs(flowGraph: FlowGraph): List[Stmt] = {
    flowGraph.setFunctions(flowGraph.getFunctions.asScala.map(func => {
      func
    }).asJava)
    flowGraph.getViewOfLines.asScala.flatMap(line =>
      List(line, Assert("-1", Literal("0")))
    ).toList
  }

  /**
   * Generate the VC for a given statement
   *
   * @param stmt
   * @return
   */
  def genVC(stmt: Stmt, state: State): Pred = stmt match {
    case cjmpStmt: CJmpStmt => computeGamma(cjmpStmt.getCondition, state)
    case assign: Assign => ??? // TODO do the different assignments need to be different
    case _: Assert => ??? // There should not be an assert here
    case _ => ???
  }

  def computeGamma(expr: Expr, state: State) = Bool.True // expr.vars.map(v => BinOp("&&", v.toGamma, state.getL(v))).conjunct
}