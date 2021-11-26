package vcgen

import facts.exp.{Expr, Literal}
import facts.stmt.Assign.Assign
import facts.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*

object VCGen {
  def genVCs (flowGraph: FlowGraph): List[Stmt] = {
    flowGraph.setFunctions(flowGraph.getFunctions.asScala.map(func => {
      func
    }).asJava)
    flowGraph.getViewOfLines.asScala.flatMap(line =>
      List(line, Assert("-1", Literal("0")))
    ).toList
  }

  /**
   * Generate the VC for a given statement
   * @param stmt
   * @return
   */
  def genVC (stmt: Stmt): Expr = stmt match {
    case cjmpStmt: CJmpStmt => computeGamma(cjmpStmt.getCondition)
    case assign: Assign => ??? // TODO do the different assignments need to be different
    case _: Assert => ???  // There should not be an assert here
    case _ => ???
  }

  def computeGamma(expr: Expr) = ???

}
