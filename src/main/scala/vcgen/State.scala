package vcgen

import facts.exp.{Expr, Var}
import translating.FlowGraph.Function
import facts.pred.{Bool, High, Pred, Security}
import facts.stmt.Stmt
import translating.FlowGraph

// TODO overlap between this and the flow graph
// e.g. globals are currently also stored in the flow graph
case class State(
                  flowGraph: FlowGraph,
                  controlledBy: Map[Var, Set[Var]],
                  private val L: Map[Var, Pred],
                  private val Gamma: Map[Var, Security],
                  rely: Expr,
                  guar: Expr
) {
  def apply(stmts: List[Stmt], lPreds: Map[Var, Pred], gamma: Map[Var, Security]): State = {
    // TODO
    // Generate flow graph
    // Find control variables
    new State(flowGraph, controlledBy, L, Gamma, rely, guar)
  }

  def getL(v: Var) = L.getOrElse(v, Bool.True)
  def getGamma(v: Var) = Gamma.getOrElse(v, High)
}
