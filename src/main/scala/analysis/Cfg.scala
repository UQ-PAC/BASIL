package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.break;

import logging.Logger

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
                                 override val pred: mutable.Set[CfgNode] = mutable.Set(),
                                 override val succ: mutable.Set[CfgNode] = mutable.Set(),
                                 data: Procedure
                               ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionEntry] $data"

/** Control-flow graph node for the exit of a function.
 */
case class CfgFunctionExitNode(
                                override val id: Int = CfgNode.nextId(),
                                override val pred: mutable.Set[CfgNode] = mutable.Set(),
                                override val succ: mutable.Set[CfgNode] = mutable.Set(),
                                data: Procedure
                              ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionExit] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockEntryNode(
                              override val id: Int = CfgNode.nextId(),
                              override val pred: mutable.Set[CfgNode] = mutable.Set(),
                              override val succ: mutable.Set[CfgNode] = mutable.Set(),
                              data: Block
                            ) extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockEntry] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockExitNode(
                             override val id: Int = CfgNode.nextId(),
                             override val pred: mutable.Set[CfgNode] = mutable.Set(),
                             override val succ: mutable.Set[CfgNode] = mutable.Set(),
                             data: Block
                           ) extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockExit] $data"

/** Control-flow graph node for a command (statement or jump).
 */
case class CfgCommandNode(
                             override val id: Int = CfgNode.nextId(),
                             override val pred: mutable.Set[CfgNode] = mutable.Set(),
                             override val succ: mutable.Set[CfgNode] = mutable.Set(),
                             data: Command
                           ) extends CfgNodeWithData[Command]:
  override def toString: String = s"[Stmt] $data"

/** A control-flow graph.
 * @param entry
 *   the entry to the graph (source)
 * @param exits
 *   the exit to he graph (sink)
 */
class Cfg(cfg: Option[Cfg] = None):
  var nodeToBlock: mutable.Map[CfgNode, Block] = mutable.Map()
  var edges: ListBuffer[Edge] = ListBuffer()
  var entries: ListBuffer[CfgNode] = ListBuffer()
  cfg match {
    case Some(cfg) =>
      this.edges = cfg.edges
      this.entries = cfg.entries
      this.nodeToBlock = cfg.nodeToBlock
    case _ =>
  }

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

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set()): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    visited

  override def toString: String = "CFG { nodes: " + nodes + " edges : " + edges + "}"

// maybe refactor this to avoid global state in instance of Cfg?
// E.g. move into Cfg class or a CfgBuilder class?
object Cfg:

  var cfg: Cfg = Cfg()
  private var latestAdded: Option[CfgNode] = None // to track latest added node.
  private val funcEntryExit: mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)] = mutable.HashMap()
  private var interProc: Boolean = false
  private var functionCloningLimit: Int = 1

  /** Generate the cfg for each function of the program.
   */
  def generateCfgProgram(program: Program, interProc: Boolean): Cfg = {
    Logger.info("\nGenerating CFG...")
    cfg = Cfg()
    latestAdded = None

    funcEntryExit.clear()
    this.interProc = interProc
    for (func <- program.procedures) {
      if (!funcEntryExit.contains(func)) {
        generateCfgFunc(func)
      }
    }
    cfg
  }

  /** Transforms blocks into lists of stmt nodes */
  def transformBlocks(blocks: Map[String, CfgBlockEntryNode]): Map[String, mutable.ArrayBuffer[CfgNode]] = {
    blocks.map(block =>
      block._1 -> block._2.data.statements.map(
        stmt => CfgCommandNode(data = stmt).asInstanceOf[CfgNode]).to(mutable.ArrayBuffer
      )
    )
  }


  /** Generate the cfg for a function.
   */
  def generateCfgFunc(func: Procedure): Cfg = {
    val blocks: Map[String, CfgBlockEntryNode] = func.blocks.map(block =>
      block.label -> CfgBlockEntryNode(data = block)
    ).toMap
    val functionEntryNode = CfgFunctionEntryNode(data = func)
    val functionExitNode = CfgFunctionExitNode(data = func)
    cfg.addNode(functionEntryNode)
    cfg.addNode(functionExitNode)
    funcEntryExit.addOne(func -> (functionEntryNode, functionExitNode))
    val processedBlocks: mutable.Set[String] = mutable.Set()
    val otherLatestAdded: mutable.Set[CfgNode] = mutable.Set() // used if multiple DirectCalls need to join at the same node

    def visitBlocks(blocks: Map[String, CfgBlockEntryNode]): Unit = {
      val newBlocks: Map[String, mutable.ArrayBuffer[CfgNode]] = transformBlocks(blocks)
      visitBlock("l" + func.name.stripPrefix("#"))

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
            cfg.addEdge(latestAdded.get, stmtNode)
            if (otherLatestAdded.nonEmpty) {
              otherLatestAdded.foreach(otherLatestAddedNode => cfg.addEdge(otherLatestAddedNode, stmtNode))
              otherLatestAdded.clear()
            }
          }
          latestAdded = Some(stmtNode)
        )
        val lastAdded: CfgNode = latestAdded.get
        otherLatestAdded.clear()
        val clonedFunctions: mutable.Set[String] = mutable.Set()
        if (interProc) {
          blocks(blockName).data.jumps.foreach {
            case g: GoTo =>
              if (newBlocks(g.target.label).isEmpty) {
                visitBlock(g.target.label)
              } else {
                val target: CfgNode = newBlocks(g.target.label).head
                cfg.addEdge(lastAdded, target)
                latestAdded = Some(target)
                visitBlock(g.target.label)
              }
            case d: DirectCall =>
              val originalLastAdded = lastAdded
              if (!clonedFunctions.contains(d.target.name)) {
                clonedFunctions.add(d.target.name)
                val call = CfgCommandNode(data = d)
                cfg.addNode(call)
                cfg.addEdge(lastAdded, call)
                latestAdded = Some(call)
                if (functionEntryNode.data.name.equals(d.target.name)) {
                  cfg.addEdge(call, functionEntryNode)
                } else {
                  if (functionCloningLimit > 0) {
                    functionCloningLimit -= 1
                    generateCfgFunc(d.target)
                  }
                  //cfg.addEdge(call, funcEntryExit.getOrElse(d.target, )) // From DirectCall to FunctionEntry of called function
                  if (funcEntryExit.contains(d.target)) {
                    cfg.addEdge(call, funcEntryExit(d.target)._1) // From DirectCall to FunctionEntry of called function
                  } else {
                    val functionEntryNode = CfgFunctionEntryNode(data = d.target)
                    val functionExitNode = CfgFunctionExitNode(data = d.target)
                    funcEntryExit.addOne(d.target -> (functionEntryNode, functionExitNode))
                  }
                }
              } else {
                clonedFunctions.remove(d.target.name)
              }

              //nodePool.setLatestAdded(funcEntryExit(d.target)._2)
              if (d.returnTarget.isDefined) {
                if (processedBlocks.contains(d.returnTarget.get.label)) {
                  otherLatestAdded.add(latestAdded.get)
                } else {
                  visitBlock(d.returnTarget.get.label)
                }
              } else {
                otherLatestAdded.add(latestAdded.get)
              }
            case i: IndirectCall =>
              val call = CfgCommandNode(data = i)
              cfg.nodeToBlock += (call -> func.blocks.find(block => block.label == blockName).get)
              cfg.addNode(call)
              cfg.addEdge(lastAdded, call)
              latestAdded = Some(call)
              //print(s"Indirect call not supported yet ${i} ${func.name}\n")
//              if (i.returnTarget.isDefined) {
//                visitBlock(i.returnTarget.get.label)
//              } else {
//                cfg.addEdge(nodePool.getLatestAdded, functionExitNode)
//              }
              if (i.target.name.equals("R30")) {
                cfg.addEdge(latestAdded.get, functionExitNode)
              } else {
                Logger.info(s"Cannot resolve indirect call ${i} ${func.name}")
              }
              if (i.returnTarget.isDefined) {
                if (processedBlocks.contains(i.returnTarget.get.label)) {
                  otherLatestAdded.add(latestAdded.get)
                } else {
                  visitBlock(i.returnTarget.get.label)
                }
              } else {
                otherLatestAdded.add(latestAdded.get)
              }
          }
        } else {
          blocks(blockName).data.jumps.foreach {
            case g: GoTo =>
              if (newBlocks(g.target.label).isEmpty) {
                visitBlock(g.target.label)
              } else {
                val target: CfgNode = newBlocks(g.target.label).head
                cfg.addEdge(lastAdded, target)
                latestAdded = Some(target)
                visitBlock(g.target.label)
              }
            case d: DirectCall =>
              val call = CfgCommandNode(data = d)
              cfg.addNode(call)
              cfg.addEdge(lastAdded, call)
              latestAdded = Some(call)
              if (d.returnTarget.isDefined) {
                if (processedBlocks.contains(d.returnTarget.get.label)) {
                  otherLatestAdded.add(latestAdded.get)
                } else {
                  visitBlock(d.returnTarget.get.label)
                }
              }
            case i: IndirectCall =>
              val call = CfgCommandNode(data = i)
              cfg.nodeToBlock += (call -> func.blocks.find(block => block.label == blockName).get)
              cfg.addNode(call)
              cfg.addEdge(lastAdded, call)
              latestAdded = Some(call)
              //            cfg.addEdge(call, functionExitNode)
              Logger.warn(s"Indirect call not supported yet ${i}")


              if (i.returnTarget.isDefined) {
                visitBlock(i.returnTarget.get.label)
              } else {
                cfg.addEdge(lastAdded, functionExitNode)
              }
          }
        }
      }
    }



    latestAdded = Some(functionEntryNode)
    if (blocks.nonEmpty) {
      visitBlocks(blocks)
    }
    cfg.addEdge(latestAdded.get, functionExitNode)
    if (otherLatestAdded.nonEmpty) {
      otherLatestAdded.foreach(otherLatestAddedNode => cfg.addEdge(otherLatestAddedNode, functionExitNode))
      otherLatestAdded.clear()
    }
    latestAdded = Some(functionExitNode)
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
abstract class ProgramCfg(val prog: Program, cfg: Cfg) extends Cfg(Some(cfg)):
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

