package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.break;




/* UUID for a node
 */
type NodeId = Int

/** Node in the control-flow graph.
 */
object CfgNode:

  var id: NodeId = 0

  def nextId(): Int =
    id += 1
    id

// EdgeType
abstract class Edge(from: CfgNode, to: CfgNode):
  def getFrom: CfgNode = from
  def getTo: CfgNode = to

/**
  * Edge between two command nodes in the CFG 
  */
case class RegularEdge(from: CfgNode, to: CfgNode) extends Edge(from, to) {
  override def toString: String = s"RegularEdge(From: $from, To: $to)"
}

/**
  * Procedure call between node and procedure. 
  */
case class InterprocEdge(from: CfgNode, to: CfgNode) extends Edge(from, to) {
  override def toString: String = s"InterprocEdge(From: $from, To: $to)"
}

/**
  * Edge which skips over a procedure call. Used for "jumping over" function 
  * calls, i.e. in an intra-procedural CFG walk. 
  */
case class IntraprocEdge(from: CfgNode, to: CfgNode) extends Edge(from, to) {
  override def toString: String = s"IntraprocEdge(From: $from, To: $to)"
}

/** Node in the control-flow graph.
 */
trait CfgNode:

  /** Predecessor commands of current node
    */
  val predCmds : mutable.Set[CfgNode]
  /** Procedures which call this node
   * Likely empty unless this node is a [[CfgFunctionEntryNode]]
    */
  val predCalls: mutable.Set[CfgNode]
  /** If `intra` True only walk the intraprocedural CFG,
   *    i.e., don't include procedure calls
    */
  def preds(intra: Boolean) = if (intra) predCmds else predCmds.union(predCalls)
  

  /** Successor commands of current node
    */
  val succCmds : mutable.Set[CfgNode]
  /** Procedures which this node calls
    */
  val succCalls: mutable.Set[CfgNode]
  /** If `intra` True only walk the intraprocedural CFG,
   *    i.e., don't include procedure calls
    */
  def succs(intra: Boolean) = if (intra) succCmds else succCmds.union(succCalls)

  /** Unique identifier. */
  val id: NodeId = CfgNode.nextId()

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
case class CfgFunctionEntryNode() extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionEntry] $data"

/** Control-flow graph node for the exit of a function.
 */
case class CfgFunctionExitNode() extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionExit] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockEntryNode() extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockEntry] $data"

/** Control-flow graph node for a block.
 */
case class CfgBlockExitNode() extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockExit] $data"

/** Control-flow graph node for a command (statement or jump).
 */
case class CfgCommandNode() extends CfgNodeWithData[Command]:
  override def toString: String = s"[Stmt] $data"

/** 
 * A control-flow graph. Provides the ability to walk it as both an intra and inter 
 * procedural CFG. Also stores a mapping between CFG nodes and their originating 
 * Basil IR blocks.
 * 
 */
class ProgramCfg: 

  // CFG Block -> IR Block
  var nodeToBlock: mutable.Map[CfgNode, Block] = mutable.Map[CfgNode, Block]()
  var edges: ListBuffer[Edge] = ListBuffer[Edge]()
  var nodes: ListBuffer[CfgNode] = ListBuffer[CfgNode]()

  /** Add an outgoing edge from the current node.
   */
  def addEdge(from: CfgNode, to: CfgNode): Unit

  /** Add a node to the CFG.
    */
  def addNode(node: CfgNode): Unit

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

/** 
 * Control-flow graph for an entire program. We have a more granular approach, 
 *  storing commands as nodes instead of basic blocks.
 *
 * NOTE: this as an object is perhaps redundant, notably if we only wish to keep 
 * a single CFG instance and update it as we go (i.e., resolve indirect calls in place and 
 * update nodes with analysis).
 */
object ProgramCfg:

  /** Generate the cfg for each function of the program. NOTE: is this functionally different to a constructor?
   *    Do we ever expect to generate a CFG from any other data structure? If not then the `class` could probably 
   *    be absorbed into this object. 
   * 
   * @param program
   *  Basil IR of the program
   * @param inlineLimit
   *  How many levels deep to inline function calls. By defualt, don't inline - this is equivalent to an intra-procedural CFG.
   */
  
  val cfg: ProgramCfg = ProgramCfg()

  def fromIR(program: Program, inlineLimit: Int = 0): ProgramCfg = {
    require(inlineLimit >= 0, "Can't inline procedures to negative depth...")
    println("Generating CFG...")

    // Create CFG for individual functions

    // Add 

    cfg
  }

  private def cfgForProcedure(proc: Procedure) = {
    
  }