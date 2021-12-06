package vcgen

import astnodes.exp.{Expr, MemLoad, Var}
import translating.FlowGraph.{Block, Function}
import astnodes.pred.{Bool, High, Pred, Security}
import astnodes.Label
import astnodes.stmt.Stmt
import translating.FlowGraph

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.ListHasAsScala

// TODO eventually change to use a state object for everything (at the moment the flow graph
// is sort of the state I guess)
case class State(
    functions: List[FunctionState],
    rely: Pred,
    guar: Pred,
    controls: Map[Var, Set[Var]],
    private val L: Map[Var, Pred],
    private val gamma: Map[Var, Security],
) {
  def apply(flowGraph: FlowGraph, lPreds: Map[Var, Pred], gamma: Map[Var, Security]): State = {
    val controlledBy = L.map{
      case (v, p) => (v, p.vars)
    }

    // TODO alternatively could use the GOT
    val vars = flowGraph.getFunctions.asScala.flatMap(func => func.getInitStmts.asScala.map(init => init.variable)).toSet
  
    val controls = vars.map(v => (v,
      controlledBy.collect{
        case (c, controlled) if (controlled.contains(v)) => c
      }.toSet
    )).toMap

    new State(List(), rely, guar, controls, lPreds, gamma)
  }



  def getL(v: Var): Pred = L.getOrElse(v, Bool.True)
  def getGamma(v: Var): Security = gamma.getOrElse(v, High)
}

// TODO L, controls only need to be defined on state as they are only needed for globals
// TODO gamma needs to be defined for each function and the global state
case class FunctionState (
  private val labelToBlock: Map[String, Block],
  private val labelToChildren: Map[String, Set[String]],
  private val gamma: Map[Var, Security],
) {
  def getGamma(v: Var): Security = gamma.getOrElse(v, High)

  def children(label: String): Option[Set[String]] = labelToChildren.get(label)
  def children(block: Block): Option[Set[String]] = labelToChildren.get(block.label)
  
  def parents(label: String): List[String] = labelToChildren.collect{
    case (l, ls) if ls.contains(label) => l
  }.toList
  def parents(block: Block): List[String] = parents(block.label)
}

case object FunctionState {
  def apply(function: FlowGraph.Function, gamma: Map[Var, Security]): FunctionState = {
    val blocks = function.getBlocks.asScala.map(b => (b.getLabel, new Block(b))).toMap
    

    new FunctionState(blocks, Map.empty, gamma)
  }
}

case class Block (
  label: String,
  lines: List[Stmt],
) {
  def this(block: FlowGraph.Block) = this(block.getLabel, block.getLines.asScala.toList)
}
