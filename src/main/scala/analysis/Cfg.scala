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
case class CfgBlockEntryNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNodeWithData[Block]

/** Control-flow graph node for a block.
  */
case class CfgBlockExitNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNodeWithData[Block]

/** Control-flow graph node for a statement.
  */
case class CfgStatementNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Statement
) extends CfgNodeWithData[Statement]

/** A control-flow graph.
  * @param entry
  *   the entry to the graph (source)
  * @param exits
  *   the exit to he graph (sink)
  */
class Cfg(val entry: Option[CfgNode], val exit: Option[CfgNode]):

  /** Returns whether or not the graph is empty.
    */
  def isUnit: Boolean = entry.isEmpty && exit.isEmpty

  /** Returns all of the nodes in the graph.
    */
  def nodes: Set[CfgNode] =
    entry match
      case Some(node) => dfs(node, mutable.Set()).toSet
      case None       => Set()

  def concat(other: Cfg): Cfg =
    if isUnit then other
    else if other.isUnit then this
    else
      exit.get.addEdge(other.entry.get)
      Cfg(entry, other.exit)

  /** Performs a depth-first traversal of the graph.
    */
  def dfs(n: CfgNode, visited: mutable.Set[CfgNode]): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach(n => dfs(n, visited))
    visited

object Cfg:

  /** Creates an empty cfg.
    */
  def unit(): Cfg = Cfg(None, None)

  /** Creates a cfg consisting of a single node.
    */
  def singletonGraph(node: CfgNode): Cfg = Cfg(Some(node), Some(node))

  /** Generate the cfg for each function of the program.
    */
  def generateCfgProgram(program: Program): Cfg =
    // generate cfg for main only for now
    generateCfgFunc(program.functions.head)

  /** Generate the cfg for a function.
    */
  def generateCfgFunc(func: FunctionNode): Cfg =
    val entryNode = CfgFunctionEntryNode(data = func)
    val exitNode = CfgFunctionExitNode(data = func)

    val blocks = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap

    val cfgs = mutable.Map[String, Cfg]()

    // generate cfg for a statement
    def generateCfgStatement(stmt: Statement): Cfg =
      val node = CfgStatementNode(data = stmt)

      stmt match
        case GoTo(target, condition) =>
          blocks.get(target) match
            case Some(blockNode) => node.addEdge(blockNode)
            case _               =>
        case _ =>

      singletonGraph(node)

    for (_, entryNode) <- blocks do
      val exitNode = CfgBlockExitNode(data = entryNode.data)
      val stmts = entryNode.data.instructions.flatMap(_.statements)
      val body = stmts.foldLeft(unit())((acc, stmt) => acc.concat(generateCfgStatement(stmt)))

      val cfg = singletonGraph(entryNode).concat(body).concat(singletonGraph(exitNode))
      cfgs += (entryNode.data.label -> cfg)

    cfgs.get("lmain") match
      case Some(cfg) => singletonGraph(entryNode).concat(cfg).concat(singletonGraph(exitNode))
      case _         => throw new RuntimeException("no main block detected")
