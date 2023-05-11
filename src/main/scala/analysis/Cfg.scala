package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.break;

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
  private var latestAdded: CfgNode = null;

  def get(command: Command): CfgNode = {
    CfgCommandNode(data = command)
  }

  def setLatestAdded(node: CfgNode): Unit = {
    latestAdded = node
  }

  def getLatestAdded: CfgNode = {
    latestAdded
  }
}

object Cfg:

  var cfg: Cfg = Cfg()
  private var nodePool = NodePool()
  private val funcEntryExit: mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)] = mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)]()
  private var interProc: Boolean = false

  /** Generate the cfg for each function of the program.
   */
  def generateCfgProgram(program: Program, interProc: Boolean): Cfg = {
    print("\nGenerating CFG... \n")
    cfg = Cfg()
    nodePool = NodePool()
    funcEntryExit.clear()
    this.interProc = interProc
    for (func <- program.procedures) {
      val functionEntryNode = CfgFunctionEntryNode(data = func)
      val functionExitNode = CfgFunctionExitNode(data = func)
      cfg.addNode(functionEntryNode)
      cfg.addNode(functionExitNode)
      funcEntryExit.addOne(func -> (functionEntryNode, functionExitNode))
    }
    program.procedures.map(f => f -> generateCfgFunc(f))
    cfg
  }

  /** Transforms blocks into lists of stmt nodes */
  def transformBlocks(blocks: Map[String, CfgBlockEntryNode]): Map[String, mutable.ArrayBuffer[CfgNode]] = {
    blocks.map(block => block._1 -> block._2.data.statements.map(stmt => nodePool.get(stmt)))
  }


  /** Generate the cfg for a function.
   */
  def generateCfgFunc(func: Procedure): Cfg = {
    val blocks: Map[String, CfgBlockEntryNode] = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap
    val functionEntryNode = funcEntryExit(func)._1
    val functionExitNode = funcEntryExit(func)._2
    val processedBlocks: mutable.Set[String] = mutable.Set[String]()

    def visitBlocks(blocks: Map[String, CfgBlockEntryNode]): Unit = {
      val newBlocks: Map[String, mutable.ArrayBuffer[CfgNode]] = transformBlocks(blocks)
      visitBlock("l" + func.name)

      def visitBlock(blockName: String): Unit = {
        if (!processedBlocks.contains(blockName)) {
          processedBlocks += blockName
        } else {
          return
        }

        val statementNodes = newBlocks(blockName)
        statementNodes.foreach(stmtNode =>
          if (!(stmtNode.pred.nonEmpty && stmtNode == statementNodes.head)) {
            cfg.addNode(stmtNode)
            cfg.addEdge(nodePool.getLatestAdded, stmtNode)
          }
          nodePool.setLatestAdded(stmtNode)
        )
        val lastAdded: CfgNode = nodePool.getLatestAdded

        if (interProc) {
          blocks(blockName).data.jumps.foreach {
            case g: GoTo =>
              if (newBlocks(g.target.label).isEmpty) {
                visitBlock(g.target.label)
              } else {
                val target: CfgNode = newBlocks(g.target.label).head
                cfg.addEdge(lastAdded, target)
                nodePool.setLatestAdded(target)
                visitBlock(g.target.label)
              }
            case d: DirectCall =>
              val call = nodePool.get(d)
              val (localEntry, localExit): (CfgFunctionEntryNode, CfgFunctionExitNode) = funcEntryExit(d.target)
              cfg.addNode(call)
              cfg.addEdge(lastAdded, call)
              cfg.addEdge(call, localEntry)
              nodePool.setLatestAdded(localExit)
              if (d.returnTarget.isDefined) {
                visitBlock(d.returnTarget.get.label)
              }
            case i: IndirectCall =>
              //            val call = nodePool.get(i)
              //            cfg.addNode(call)
              //            cfg.addEdge(lastAdded, call)
              //            cfg.addEdge(call, functionExitNode)
              print(s"Indirect call not supported yet ${i}\n")

              cfg.addEdge(lastAdded, functionExitNode)
          }
        } else {
          blocks(blockName).data.jumps.foreach {
            case g: GoTo =>
              if (newBlocks(g.target.label).isEmpty) {
                visitBlock(g.target.label)
              } else {
                val target: CfgNode = newBlocks(g.target.label).head
                cfg.addEdge(lastAdded, target)
                nodePool.setLatestAdded(target)
                visitBlock(g.target.label)
              }
            case d: DirectCall =>
              val call = nodePool.get(d)
              cfg.addNode(call)
              cfg.addEdge(lastAdded, call)
              nodePool.setLatestAdded(call)
              if (d.returnTarget.isDefined) {
                visitBlock(d.returnTarget.get.label)
              }
            case i: IndirectCall =>
              //            val call = nodePool.get(i)
              //            cfg.addNode(call)
              //            cfg.addEdge(lastAdded, call)
              //            cfg.addEdge(call, functionExitNode)

              cfg.addEdge(lastAdded, functionExitNode)
          }
        }
      }
    }



    nodePool.setLatestAdded(functionEntryNode)
    if (blocks.nonEmpty) {
      visitBlocks(blocks)
    }
    cfg.addEdge(nodePool.getLatestAdded, functionExitNode)
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
    val cfg: Cfg = Cfg.generateCfgProgram(prog, false)
    new IntraproceduralProgramCfg(prog, cfg)

/** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
 * nodes.
 */
class IntraproceduralProgramCfg(prog: Program, cfg: Cfg) extends ProgramCfg(prog, cfg)


object InterproceduralProgramCfg:

  /** Generates an [[InterproceduralProgramCfg]] from a program.
   */
  def generateFromProgram(prog: Program): InterproceduralProgramCfg =
    val cfg: Cfg = Cfg.generateCfgProgram(prog, true)
    new InterproceduralProgramCfg(prog, cfg)

/** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
 * nodes.
 */
class InterproceduralProgramCfg(prog: Program, cfg: Cfg) extends ProgramCfg(prog, cfg)

