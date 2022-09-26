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
class Cfg(val entries: Set[CfgNode], val exits: Set[CfgNode]):

  /** Returns whether or not the graph is empty.
    */
  def isUnit: Boolean = entries.isEmpty && exits.isEmpty

  /** Returns the concatenation of this CFG with `after`.
    */
  def concat(after: Cfg): Cfg =
    if isUnit then after
    else if after.isUnit then this
    else
      exits.foreach(_.succ ++= after.entries)
      after.entries.foreach(_.pred ++= exits)
      new Cfg(entries, after.exits)

  /** Returns the union of this CFG with `other`.
    */
  def union(other: Cfg): Cfg =
    new Cfg(other.entries.union(entries), other.exits.union(exits))

  /** Returns the set of nodes in the CFG.
    */
  def nodes: Set[CfgNode] =
    entries.flatMap { entry =>
      nodesRec(entry).toSet
    }

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set()): mutable.Set[CfgNode] =
    if (!visited.contains(n)) then
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    visited

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

/** Control-flow graph for an entire program.
  *
  * @param prog
  *   AST of the program
  * @param funEntries
  *   map from AST function declarations to CFG function entry nodes
  * @param funExits
  *   map from AST function declarations to CFG function exit nodes
  */
abstract class ProgramCfg(
    val prog: Program,
    val funEntries: Map[FunctionNode, CfgFunctionEntryNode],
    val funExits: Map[FunctionNode, CfgFunctionExitNode]
) extends Cfg(funEntries.values.toSet, funExits.values.toSet)

object IntraproceduralProgramCfg:
  /** Generates an [[IntraproceduralProgramCfg]] from a program.
    */
  def generateFromProgram(prog: Program): IntraproceduralProgramCfg =
    val funGraphs = Cfg.generateCfgProgram(prog)
    val allEntries = funGraphs.view.mapValues(cfg => cfg.entries.head.asInstanceOf[CfgFunctionEntryNode]).toMap
    val allExits = funGraphs.view.mapValues(cfg => cfg.exits.head.asInstanceOf[CfgFunctionExitNode]).toMap

    new IntraproceduralProgramCfg(prog, allEntries, allExits)

/** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
  * nodes.
  */
class IntraproceduralProgramCfg(
    prog: Program,
    funEntries: Map[FunctionNode, CfgFunctionEntryNode],
    funExits: Map[FunctionNode, CfgFunctionExitNode]
) extends ProgramCfg(prog, funEntries, funExits)
