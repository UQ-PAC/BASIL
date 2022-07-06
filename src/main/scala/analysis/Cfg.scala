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

/** Control-flow graph node for a statement.
  */
case class CfgStatementNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Statement
) extends CfgNodeWithData[Statement]

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

/** Control-flow graph node for the entry of a block.
  */
case class CfgBlockEntryNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNodeWithData[Block]

/** Control-flow graph node for the exit of a block.
  */
case class CfgBlockExitNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNodeWithData[Block]

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

  /** Performs the concatenation of two cfgs by adding edges between the two.
    */
  def concat(other: Cfg): Cfg =
    if isUnit then other
    else if other.isUnit then this
    else
      exits.foreach(exitNode => other.entries.foreach(entryNode => exitNode.addEdge(entryNode)))
      Cfg(entries, other.exits)

  /** Returns all of the nodes in the graph.
    */
  def nodes: Set[CfgNode] =
    entries.flatMap(entry => dfs(entry, mutable.Set()).toSet)

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
      case CfgBlockEntryNode(_, _, _, Block(branchLabel, _, _)) => true
      case _                                                    => false
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
    val blocks = func.blocks.foldLeft(unit())((acc, block) => acc.concat(generateCfgBlock(block)))
    addBranches(blocks)

    singletonGraph(entryNode).concat(blocks).concat(singletonGraph(exitNode))

  /** Generate the cfg for a block.
    */
  def generateCfgBlock(block: Block): Cfg =
    val entryNode = CfgBlockEntryNode(data = block)
    val exitNode = CfgBlockExitNode(data = block)
    val instrs = block.instructions.foldLeft(unit())((acc, instr) => acc.concat(generateCfgInstr(instr)))

    singletonGraph(entryNode).concat(instrs).concat(singletonGraph(exitNode))

  /** Generate the cfg for an instruction.
    */
  def generateCfgInstr(instr: Instruction): Cfg =
    instr.statements.foldLeft(unit())((acc, stmt) => acc.concat(generateCfgStmt(stmt)))

  /** Generate the cfg for a statement.
    */
  def generateCfgStmt(stmt: Statement): Cfg =
    val node = CfgStatementNode(data = stmt)

    singletonGraph(node)

  /** Add edge for branching caused by a GoTo statement.
    */
  def addBranches(cfg: Cfg): Unit =
    cfg.nodes.foreach {
      case n @ CfgStatementNode(_, _, _, GoTo(target, condition)) => cfg.lookup(target).map(n.addEdge(_))
      case _                                                      =>
    }
