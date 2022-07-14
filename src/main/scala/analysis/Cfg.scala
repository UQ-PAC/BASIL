package analysis
import scala.collection.mutable

import astnodes._

object CfgNode:

  var id: Int = 0

  def nextId(): Int =
    id += 1
    id

/** Node in the control-flow graph.
  */
trait CfgNode:

  /** Predecessors of the current node. */
  val pred: mutable.Set[CfgNode]

  /** Successors of the current node. */
  val succ: mutable.Set[CfgNode]

  /** Unique identifier. */
  val id: Int

  /** Add an outgoing edge from the current node.
    */
  def addEdge(other: CfgNode): Unit =
    succ += other
    other.pred += this

  override def equals(obj: scala.Any): Boolean =
    obj match
      case o: CfgNode => o.id == this.id
      case _          => false

  override def hashCode(): Int = id

/** Control-flow graph node that additionally stores an AST node.
  */
trait CfgNodeWithData[T] extends CfgNode:

  def data: T

/** Control-flow graph node for the entry of a function.
  */
case class CfgFunctionEntryNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNodeWithData[FunctionNode]

/** Control-flow graph node for the exit of a function.
  */
case class CfgFunctionExitNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNodeWithData[FunctionNode]

/** Control-flow graph node for a block.
  */
case class CfgBlockNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNodeWithData[Block]:
  def getStatements(): List[Statement] =
    data.instructions.flatMap(_.statements)

/** A control-flow graph.
  * @param entries
  *   the entries to the graph (sources)
  * @param exits
  *   the exits of the graph (sinks)
  */
class Cfg(val entries: Set[CfgNode], val exits: Set[CfgNode]):

  /** Returns whether or not the graph is empty.
    */
  def isUnit: Boolean = entries.isEmpty && exits.isEmpty

  /** Returns all of the nodes in the graph.
    */
  def nodes: Set[CfgNode] =
    entries.flatMap(entry => dfs(entry, mutable.Set()).toSet)

  def concat(other: Cfg): Cfg =
    if isUnit then other
    else if other.isUnit then this
    else
      exits.foreach(exit => other.entries.foreach(entry => exit.addEdge(entry)))
      Cfg(entries, other.exits)

  /** Performs a depth-first traversal of the graph.
    */
  def dfs(n: CfgNode, visited: mutable.Set[CfgNode]): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach(n => dfs(n, visited))
    visited

  /** Find the cfg block node that corresponds to the given label.
    */
  def lookup(branchLabel: String): Option[CfgNode] =
    nodes.find {
      case CfgBlockNode(_, _, _, Block(branchLabel, _, _)) => true
      case _                                               => false
    }

object Cfg:

  /** Creates an empty cfg.
    */
  def unit(): Cfg = Cfg(Set(), Set())

  /** Creates a cfg consisting of a single node.
    */
  def singletonGraph(node: CfgNode): Cfg = Cfg(Set(node), Set(node))

  /** Generate the cfg for each function of the program.
    */
  def generateCfgProgram(program: Program): Map[FunctionNode, Cfg] =
    program.functions.map(f => f -> generateCfgFunc(f)).toMap

  /** Generate the cfg for a function.
    */
  def generateCfgFunc(func: FunctionNode): Cfg =
    val entryNode = CfgFunctionEntryNode(data = func)
    val exitNode = CfgFunctionExitNode(data = func)
    val blocks = func.blocks.foldLeft(unit())((acc, block) => acc.concat(singletonGraph(CfgBlockNode(data = block))))

    val cfg = singletonGraph(entryNode).concat(blocks).concat(singletonGraph(exitNode))
    addBranches(cfg)

    cfg

  /** Get all branches from the current block.
    */
  def getBranches(block: Block): Set[String] =
    val stmts = block.instructions.flatMap(_.statements)
    var branches: Set[String] = Set()

    stmts.foreach {
      case GoTo(target, condition) => branches += target
      case _                       =>
    }

    branches

  /** Add edge for GoTo statements.
    */
  def addBranches(cfg: Cfg): Unit =
    cfg.nodes.foreach {
      case b: CfgBlockNode => getBranches(b.data).foreach(label => cfg.lookup(label).map(_.addEdge(b)))
      case _               =>
    }
