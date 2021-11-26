package vcgen

import facts.exp.{Expr, Literal}
import facts.stmt.{Assert, Stmt}
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
    case _ => ???
  }

}
