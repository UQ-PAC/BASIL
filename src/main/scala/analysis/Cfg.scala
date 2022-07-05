package analysis

import scala.collection.mutable

import astnodes._

object CfgNode:

  var id: Int = 0

  def nextId: Int =
    id += 1
    id

/** Control-flow graph node
  */
trait CfgNode:

  /** Predecessors of the node.
    */
  def pred: mutable.Set[CfgNode]

  /** Successors of the node.
    */
  def succ: mutable.Set[CfgNode]

  /** Node ID
    */
  def id: Int

  override def equals(obj: scala.Any): Boolean =
    obj match
      case o: CfgNode => o.id == this.id
      case _          => false

  override def hashCode(): Int = id

trait CfgNodeWithData[T] extends CfgNode:
  def data: T

/** Node in a CFG representing a statement
  */
case class CfgStatementNode(
    override val id: Int = CfgNode.nextId,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Statement
) extends CfgNodeWithData[Statement]

/** Node in a CFG representing the entry of a function.
  */
case class CfgFunctionEntryNode(
    override val id: Int = CfgNode.nextId,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNodeWithData[FunctionNode]

/** Node in a CFG representing the exit of a function.
  */
case class CfgFunctionExitNode(
    override val id: Int = CfgNode.nextId,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNodeWithData[FunctionNode]

object Cfg:

  private def unit(): Cfg = Cfg(Set(), Set())
  def nodeToGraph(node: CfgNode): Cfg = Cfg(Set(node), Set(node))

  def generateCfg(program: Program): Map[FunctionNode, Cfg] =
    program.functions.map(f => f -> generateCfg(f)).toMap

  def generateCfg(function: FunctionNode): Cfg =
    val entryNode = CfgFunctionEntryNode(data = function)
    val exitNode = CfgFunctionExitNode(data = function)
    val blocks = function.blocks.foldLeft(unit())((acc, block) => acc.concat(generateCfg(block)))

    nodeToGraph(entryNode).concat(blocks).concat(nodeToGraph(exitNode))

  def generateCfg(block: Block): Cfg =
    block.instructions.foldLeft(unit())((acc, inst) => acc.concat(generateCfg(inst)))

  def generateCfg(instruction: Instruction): Cfg =
    instruction.statements.foldLeft(unit())((acc, stmt) => acc.concat(generateCfg(stmt)))

  def generateCfg(statement: Statement): Cfg =
    val stmtNode = CfgStatementNode(data = statement)

    nodeToGraph(stmtNode)

class Cfg(val graphEntries: Set[CfgNode], val graphExits: Set[CfgNode]):

  def isUnit: Boolean = graphEntries.isEmpty && graphExits.isEmpty

  def concat(after: Cfg): Cfg =
    if isUnit then after
    else if after.isUnit then this
    else
      graphExits.foreach(_.succ ++= after.graphEntries)
      after.graphEntries.foreach(_.pred ++= graphExits)
      Cfg(graphEntries, after.graphExits)

  def nodes: Set[CfgNode] =
    graphEntries.flatMap(entry => nodesRec(entry).toSet)

  def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set()): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach(n => nodesRec(n, visited))
    visited
