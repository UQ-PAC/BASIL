package analysis
import analysis.Cfg.nodePool

import scala.collection.mutable
import bap._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

object CfgNode:

  var id: Int = 0

  def nextId(): Int =
    id += 1
    id

// EdgeType
abstract class Edge(from: CfgNode, to: CfgNode):
  def getFrom(): CfgNode = from
  def getTo():CfgNode = to
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

/** Control-flow graph node for a statement.
 */
case class CfgStatementNode(
                             override val id: Int = CfgNode.nextId(),
                             override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
                             data: Statement
                           ) extends CfgNodeWithData[Statement]:
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
  def addEdge(from: CfgNode, to: CfgNode, cond: Expr): Unit =
    from.succ += to
    to.pred += from
    if cond != null then
      edges += conditionalEdge(cond, from, to)
    else
      edges += unconditionalEdge(from, to)

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
    val sb = new StringBuilder()
    sb.append("CFG {")
    sb.append(" nodes: ")
    sb.append(nodes)
    sb.append(" edges: ")
    sb.append(edges)
    sb.append("}")
    sb.toString()
  }

case class NodePool() {
  val pool = mutable.Map[(Statement, CfgBlockEntryNode), CfgNode]()
  var latestAdded: (Statement, CfgBlockEntryNode) = (null: Statement, null: CfgBlockEntryNode)

  def get(statement: Statement, context: CfgBlockEntryNode, fromCall: Boolean = false): CfgNode = {
    if (!statement.isInstanceOf[GoTo] && !fromCall) {
      latestAdded = (statement, context)
    }
    if (pool.contains((statement, context))) {
      pool((statement, context))
    }
    else {
      val node = CfgStatementNode(data = statement)
      pool((statement, context)) = node
      node
    }
  }

  def getLatestAdded(): CfgNode = {
    if (latestAdded == (null, null)) {
      return null
    }
    pool(latestAdded)
  }
}

object Cfg:

  var cfg: Cfg = Cfg()
  val nodePool = NodePool()

  /** Generate the cfg for each function of the program.
   */
  def generateCfgProgram(program: Program): Cfg = {
    program.functions.map(f => f -> generateCfgFunc(f))
    cfg
  }

  /** Generate the cfg for a function.
   */

  def generateCfgFunc(func: Subroutine): Cfg = {
    val blocks = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap
    val functionEntryNode = CfgFunctionEntryNode(data = func)
    val functionExitNode = CfgFunctionExitNode(data = func)
    cfg.addNode(functionEntryNode)
    cfg.addNode(functionExitNode)

    // generate cfg for a statement
    def generateCfgStatement(stmt: Statement, context: CfgBlockEntryNode): CfgNode =
      println(stmt)
      var previous: CfgNode = nodePool.getLatestAdded()
      val node: CfgNode = nodePool.get(stmt, context)
      if (!stmt.isInstanceOf[GoTo]) {
        if (previous == null) {
          cfg.addEdge(functionEntryNode, node, null)
        } else {
          cfg.addEdge(previous, node, null)
        }
      }
      stmt match {
        case goTo: GoTo =>
          blocks.get(goTo.target) match
            case Some(blockNode) => cfg.addEdge(cfg.entries.last, nodePool.get(blockNode.data.statements.head, blockNode, true), goTo.condition)
            case _ => print(s"ERROR: goto target in '${goTo}' not found\n")

        case directCall: DirectCall =>
          // edge between current -> target
          blocks.get(directCall.target) match
            case Some(blockNode) => cfg.addEdge(node, blockNode, null)
            case _ => print(s"ERROR: direct call target in '${directCall}' not found\n")
          directCall.returnTarget match
            case Some(returnTarget) =>
              blocks.get(returnTarget) match
                case Some(returnBlockNode) =>
                  blocks.get(directCall.target) match
                    // edge between target -> return target
                    case Some(blockNode) => cfg.addEdge(blockNode, returnBlockNode, null)
                    case _ =>
                case _ =>
                  print(s"ERROR: direct call return target in '${directCall}' not found\n")
            case _ =>
              // edge between target -> current (if no return target)
              blocks.get(directCall.target) match
                case Some(blockNode) => cfg.addEdge(blockNode, node, null)
                case _ =>

        case indirectCall: IndirectCall =>
          // edge between current -> unknown block
          val unknownBlockNode = CfgBlockEntryNode(data = Block(label = s"Unknown: ${indirectCall.locals.toString()}", address = null, statements = List()))
          //node.addEdge(unknownBlockNode)
          if (indirectCall.target.name == "R30") {
            cfg.addEdge(node, functionExitNode, null)
          }
          print(s"ERROR: indirect call target in '${indirectCall}' not found\n")
          indirectCall.returnTarget match
            case Some(returnTarget) =>
              // edge between unknown block -> return target
              blocks.get(returnTarget) match
                case Some(returnBlockNode) => //unknownBlockNode.addEdge(returnBlockNode)
                case _ => print(s"ERROR: indirect call return target in '${indirectCall}' not found\n")
            // edge between unknown block -> current (if no return target)
            case _ => //unknownBlockNode.addEdge(node)
        case _ =>
      }
      node

    for (_, entryNode) <- blocks do
      val stmts = entryNode.data.statements

      stmts.foreach(stmt => {
        if (stmt.isInstanceOf[GoTo]) {
          generateCfgStatement(stmt, entryNode)
        } else {
          cfg.addNode(generateCfgStatement(stmt, entryNode))
        }
      })

    if (blocks.isEmpty) {
      cfg.addEdge(functionEntryNode, functionExitNode, null)
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

