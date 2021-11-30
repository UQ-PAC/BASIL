package vcgen

import facts.exp.{Expr, Var}
import translating.FlowGraph.Function

// TODO overlap between this and the flow graph
// e.g. globals are currently also stored in the flow graph
case class State(
    functions: List[Function],
    controlledBy: Map[Var, Set[Var]],
    private val L: Map[Var, Set[Var]],
    rely: Expr,
    guar: Expr
) {
  def getL(v: Var) = L.getOrElse(v, Bool.True)
}
