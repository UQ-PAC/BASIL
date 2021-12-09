package vcgen

import astnodes.exp.{Expr, MemLoad, Var}
import translating.FlowGraph.{Block, Function}
import astnodes.pred.{Bool, High, Pred, Security}
import astnodes.Label
import astnodes.stmt.{CJmpStmt, CallStmt, EnterSub, ExitSub, InitStmt, JmpStmt, Stmt}
import translating.FlowGraph
import util.Boogie.generateBVHeader

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.ListHasAsScala

// TODO eventually change to use a state object for everything (at the moment the flow graph
// is sort of the state I guess)
case class State(
    functions: List[FunctionState],
    rely: Pred,
    guar: Pred,
    controls: Map[Var, Set[Var]],
    globalInits: List[InitStmt],
    private val L: Map[Var, Pred],
    private val gamma: Map[Var, Security],
) {
  def getL(v: Var): Pred = L.getOrElse(v, Bool.True)
  def getGamma(v: Var): Security = gamma.getOrElse(v, High)

  override def toString: String = generateBVHeader(1) + generateBVHeader(32) + generateBVHeader(64)
    + globalInits.map(_.toBoogieString).mkString("\n") + functions.mkString("")

}

case object State {
  def apply(flowGraph: FlowGraph, rely: Pred, guar: Pred, lPreds: Map[Var, Pred], gamma: Map[Var, Security]): State = {
    val controlledBy = lPreds.map{
      case (v, p) => (v, p.vars)
    }

    // TODO alternatively could use the GOT
    // TODO could globalInits be used??
    val vars = flowGraph.getFunctions.asScala.flatMap(func => func.getInitStmts.asScala.map(init => init.variable)).toSet

    val controls = vars.map(v => (v,
      controlledBy.collect{
        case (c, controlled) if (controlled.contains(v)) => c
      }.toSet
    )).toMap[Var, Set[Var]]

    // TODO gamma for each function
    val functions = flowGraph.functions.asScala.map(f => FunctionState(f, Map.empty)).toList

    State(functions, rely, guar, controls, flowGraph.getGlobalInits.asScala.toList, lPreds, gamma)
  }
}

// TODO L, controls only need to be defined on state as they are only needed for globals
// TODO gamma needs to be defined for each function and the global state
case class FunctionState (
                           labelToBlock: Map[String, Block],
                           initStmts: List[InitStmt],
                           header: EnterSub,
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


  // TOOD could sbst("    \n", "            \n")
  override def toString: String = header.toString + "\n" + initStmts.map(_.toBoogieString).mkString("\n") + labelToBlock.values.mkString("") + "}"
}

case object FunctionState {
  def apply(function: FlowGraph.Function, gamma: Map[Var, Security]): FunctionState = {
    val blocks = function.getBlocks.asScala.map(b => (b.getLabel, new Block(b))).toMap
    val labelToChildren = function.getBlocks.asScala.map(b => (b.label, b.lastLine match {
      case cjmp: CJmpStmt => Set(cjmp.trueTarget, cjmp.falseTarget)
      case jmp: JmpStmt => Set(jmp.target)
      case call: CallStmt => call.returnTarget.toSet
      case _: ExitSub => Set()
    })).toMap

    new FunctionState(blocks, function.getInitStmts.asScala.toList, function.getHeader, labelToChildren, gamma)
  }
}

case class Block (
  label: String,
  lines: List[Stmt],
) {
  def this(block: FlowGraph.Block) = this(block.getLabel, block.getLines.asScala.toList)

  override def toString: String = "\nlabel" + label + ":\n    " + lines.map(l => l.toBoogieString).mkString("\n    ")
}
