package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

object CfgNode:

  var id: Int = 0

  def nextId(): Int =
    id += 1
    id

// EdgeType
abstract class Edge(from: CfgNode, to: CfgNode):
  def getFrom: CfgNode = from
  def getTo: CfgNode = to
case class conditionalEdge(cond: Expr, from: CfgNode, to: CfgNode) extends Edge(from, to) {
  override def toString: String = s"conditionalEdge(cond: $cond, From: $from, To: $to)"
}

case class unconditionalEdge(from: CfgNode, to: CfgNode) extends Edge(from = from, to = to) {
  override def toString: String = s"unconditionalEdge(From: $from, To: $to)"
}

/** Node in the control-flow graph.
 */
trait CfgNode:

  /** Predecessors of the current node. */
  val pred: mutable.Set[CfgNode]

  /** Successors of the current node. */
  val succ: mutable.Set[CfgNode]

  /** Unique identifier. */
  val id: Int

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
                                 data: Procedure
                               ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionEntry] $data"

/** Control-flow graph node for the exit of a function.
 */
case class CfgFunctionExitNode(
                                override val id: Int = CfgNode.nextId(),
                                override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                                override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                                data: Procedure
                              ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionExit] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockEntryNode(
                              override val id: Int = CfgNode.nextId(),
                              override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                              override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                              data: Block
                            ) extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockEntry] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockExitNode(
                             override val id: Int = CfgNode.nextId(),
                             override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             data: Block
                           ) extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockExit] $data"

/** Control-flow graph node for a command (statement or jump).
 */
case class CfgCommandNode(
                             override val id: Int = CfgNode.nextId(),
                             override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             data: Command
                           ) extends CfgNodeWithData[Command]:
  override def toString: String = s"[Stmt] $data"

/** A control-flow graph.
 * @param entry
 *   the entry to the graph (source)
 * @param exits
 *   the exit to he graph (sink)
 */
class Cfg(cfg: Cfg = null):
  var edges: ListBuffer[Edge] = ListBuffer[Edge]()
  var entries: ListBuffer[CfgNode] = ListBuffer[CfgNode]()
  if (cfg != null) then
    this.edges = cfg.edges
    this.entries = cfg.entries

  def concatBefore(before: CfgNode): Unit =
    before.succ += entries.head
    entries.head.pred += before

  def concatAfter(after: CfgNode): Unit =
    after.pred += entries.last
    entries.last.succ += after

  def getEdges: ListBuffer[Edge] = {
    edges
  }

  /** Add an outgoing edge from the current node.
   */
  def addEdge(from: CfgNode, to: CfgNode, cond: Option[Expr] = None): Unit =
    from.succ += to
    to.pred += from
    cond match {
      case Some(c) => edges += conditionalEdge(c, from, to)
      case None => edges += unconditionalEdge(from, to)
    }

  def addNode(node: CfgNode): Unit =
    entries += node

  /** Returns the set of nodes in the CFG.
   */
  def nodes: Set[CfgNode] =
    entries.flatMap {
      entry =>
        nodesRec(entry).toSet
    }.toSet

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set[CfgNode]()): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    visited

  override def toString: String = {
    val sb = StringBuilder()
    sb.append("CFG {")
    sb.append(" nodes: ")
    sb.append(nodes)
    sb.append(" edges: ")
    sb.append(edges)
    sb.append("}")
    sb.toString()
  }

case class NodePool() {
  val pool = mutable.Map[(Command, CfgBlockEntryNode), CfgNode]()
  var latestAdded: Option[(Command, CfgBlockEntryNode)] = None

  def get(command: Command, context: CfgBlockEntryNode, fromCall: Boolean = false): CfgNode = {
    command match {
      case g: GoTo if fromCall =>
      case _ => latestAdded = Some(command, context)
    }
    if (pool.contains((command, context))) {
      pool((command, context))
    }
    else {
      val node = CfgCommandNode(data = command)
      pool((command, context)) = node
      node
    }
  }

  def getLatestAdded: Option[CfgNode] = {
    latestAdded.map { l => pool(l) }
  }
}

object Cfg:

  var cfg: Cfg = Cfg()
  val nodePool = NodePool()

  /** Generate the cfg for each function of the program.
   */
  def generateCfgProgram(program: Program): Cfg = {
    program.procedures.map(f => f -> generateCfgFunc(f))
    cfg
  }

  /** Generate the cfg for a function.
   */

  def generateCfgFunc(func: Procedure): Cfg = {
    val blocks = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap
    val functionEntryNode = CfgFunctionEntryNode(data = func)
    val functionExitNode = CfgFunctionExitNode(data = func)
    cfg.addNode(functionEntryNode)
    cfg.addNode(functionExitNode)

    // generate cfg for a statement
    def generateCfgStatement(stmt: Statement, context: CfgBlockEntryNode): CfgNode =
      println(stmt)
      val previous: Option[CfgNode] = nodePool.getLatestAdded
      val node: CfgNode = nodePool.get(stmt, context)
      previous match {
        case Some(p) => cfg.addEdge(p, node)
        case None => cfg.addEdge(functionEntryNode, node)
      }
      node

    def generateCfgJump(jump: Jump, context: CfgBlockEntryNode): CfgNode =
      println(jump)
      var previous: Option[CfgNode] = nodePool.getLatestAdded
      val node: CfgNode = nodePool.get(jump, context)
      jump match {
        case g: GoTo =>
        case _ =>
          previous match {
            case Some(p) => cfg.addEdge(p, node)
            case None => cfg.addEdge(functionEntryNode, node)
          }
      }
      jump match {
        case goTo: GoTo =>
          blocks.get(goTo.target.label) match
            case Some(blockNode) => cfg.addEdge(cfg.entries.last, nodePool.get(blockNode.data.statements.head, blockNode, true), goTo.condition)
            case _ => print(s"ERROR: goto target in '${goTo}' not found\n")

        case directCall: DirectCall =>
          // edge between current -> target
          blocks.get(directCall.target.name) match
            case Some(blockNode) => cfg.addEdge(node, blockNode)
            case _ => print(s"ERROR: direct call target in '${directCall}' not found\n")
          directCall.returnTarget match
            case Some(returnTarget) =>
              blocks.get(returnTarget.label) match
                case Some(returnBlockNode) =>
                  blocks.get(directCall.target.name) match
                    // edge between target -> return target
                    case Some(blockNode) => cfg.addEdge(blockNode, returnBlockNode)
                    case _ =>
                case _ =>
                  print(s"ERROR: direct call return target in '${directCall}' not found\n")
            case _ =>
              // edge between target -> current (if no return target)
              blocks.get(directCall.target.name) match
                case Some(blockNode) => cfg.addEdge(blockNode, node)
                case _ =>

        case indirectCall: IndirectCall =>
          // edge between current -> unknown block
          //val unknownBlockNode = CfgBlockEntryNode(data = Block(label = s"Unknown: ${indirectCall.locals.toString()}", address = null, statements = ArrayBuffer()))
          //node.addEdge(unknownBlockNode)
          if (indirectCall.target.name == "R30") {
            cfg.addEdge(node, functionExitNode)
          }
          print(s"ERROR: indirect call target in '${indirectCall}' not found\n")
          indirectCall.returnTarget match
            case Some(returnTarget) =>
              // edge between unknown block -> return target
              blocks.get(returnTarget.label) match
                case Some(returnBlockNode) => //unknownBlockNode.addEdge(returnBlockNode)
                case _ => print(s"ERROR: indirect call return target in '${indirectCall}' not found\n")
            // edge between unknown block -> current (if no return target)
            case _ => //unknownBlockNode.addEdge(node)
        case _ =>
      }
      node

    for (_, entryNode) <- blocks do
      val stmts = entryNode.data.statements
      stmts.foreach(stmt => cfg.addNode(generateCfgStatement(stmt, entryNode)))
      entryNode.data.jumps.foreach {
        case g: GoTo => generateCfgJump(g, entryNode)
        case j:_ => cfg.addNode(generateCfgJump(j, entryNode))
      }

    if (blocks.isEmpty) {
      cfg.addEdge(functionEntryNode, functionExitNode)
    }

    //nodePool.pool.foreach(p => print(s"\n Pool(${p})\n"))
    cfg
  }

/** Control-flow graph for an entire program.
 *
 * @param prog
 *   AST of the program
 * @param funEntries
 *   map from AST function declarations to CFG function entry nodes
 * @param funExits
 *   map from AST function declarations to CFG function exit nodes
 */
abstract class ProgramCfg(val prog: Program, cfg: Cfg) extends Cfg(cfg):
  /**
   * Returns a Graphviz dot representation of the CFG.
   * Each node is labeled using the given function labeler.
   */
  def toDot(labeler: CfgNode => String, idGen: (CfgNode, Int) => String): String = {
    val dotNodes = mutable.Map[CfgNode, DotNode]()
    var dotArrows = mutable.ListBuffer[DotArrow]()
    var uniqueId = 0
    nodes.foreach { n =>

      dotNodes += (n -> new DotNode(s"${idGen(n, uniqueId)}", labeler(n)))
      uniqueId += 1
    }
    nodes.foreach { n =>
      n.succ.foreach { dest =>
        dotArrows += new DotDirArrow(dotNodes(n), dotNodes(dest))
      }
    }
    dotArrows = dotArrows.sortBy(arr => arr.fromNode.id + "-" + arr.toNode.id)
    val allNodes = dotNodes.values.seq.toList.sortBy(n => n.id)
    new DotGraph("CFG", allNodes, dotArrows).toDotString
  }

object IntraproceduralProgramCfg:

  /** Generates an [[IntraproceduralProgramCfg]] from a program.
   */
  def generateFromProgram(prog: Program): IntraproceduralProgramCfg =
    val cfg: Cfg = Cfg.generateCfgProgram(prog)
    new IntraproceduralProgramCfg(prog, cfg)

/** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
 * nodes.
 */
class IntraproceduralProgramCfg(prog: Program, cfg: Cfg) extends ProgramCfg(prog, cfg)

