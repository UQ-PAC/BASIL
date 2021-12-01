package vcgen

import astnodes.exp.{Expr, Var}
import translating.FlowGraph.{Block, Function}
import astnodes.pred.{Bool, High, Pred, Security}
import facts.Label
import facts.stmt.Stmt
import translating.FlowGraph

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.ListHasAsScala

// TODO eventually change to use a state object for everything (at the moment the flow graph
// is sort of the state I guess)

// TODO alternatively, does it make more sense to store the state per function
// OR to have a state for the whole program and for each function

// TODO overlap between this and the flow graph
// e.g. globals are currently also stored in the flow graph
case class State(
    functions: List[FunctionState],
    rely: Pred,
    guar: Pred
) {
  def apply(flowGraph: FlowGraph, lPreds: Map[Var, Pred], gamma: Map[Var, Security]): State = {
    // TODO
    // Generate flow graph
    // Find control variables

    new State(List(), rely, guar)
  }

}


case class FunctionState (
                           controls: Map[Var, Set[Var]],
                           blocks: List[Block],
                           vars: List[Var], // TODO do we use this?
                           private val L: Map[Var, Pred],
                           private val gamma: Map[Var, Security],
) {
  def getL(v: Var) = L.getOrElse(v, Bool.True)
  def getGamma(v: Var) = gamma.getOrElse(v, High)
}

case object FunctionState {
  def apply(function: FlowGraph.Function, L: Map[Var, Pred], gamma: Map[Var, Security], vars: List[Var]): FunctionState = {
    val controlledBy = L.map{
      case (v, p) => (v, p.vars)
    }

    val controls = vars.map(v => (v,
      controlledBy.filter{
        case (c, controlled) => controlled.contains(v)
      }.map{
        case (c, _) => c
      }.toSet
    )).toMap

    val blocks = function.getBlocks.asScala.map(b => new Block(b))

    new FunctionState(controls, blocks.toList, vars, L, gamma)
  }
}

case class Block (
  label: String,
  lines: List[Stmt],
) {
  def this(block: FlowGraph.Block) = this(block.getLabel, block.getLines.asScala.toList)
}
