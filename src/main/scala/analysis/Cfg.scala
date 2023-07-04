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

/**
  * Edge type. 
  * 
  * `cond` : condition if this is a conditional edge. By default, assume True.
  */
trait CfgEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral):
  def getFrom: CfgNode = from
  def getTo: CfgNode = to
  def getCond: Expr = cond

/**
  * Edge between two command nodes in the CFG. Used for `GoTo` branches 
  * as well (with a condition added for the branch).
  */
case class RegularEdge(from: CfgNode, to: CfgNode) extends CfgEdge(from, to) {
  override def toString: String = s"RegularEdge(From: $from, To: $to)"
}

/**
  * Edge which skips over a procedure call. Used for "jumping over" function 
  * calls, i.e. in an intra-procedural CFG walk. 
  */
case class IntraprocEdge(from: CfgNode, to: CfgNode) extends CfgEdge(from, to) {
  override def toString: String = s"IntraprocEdge(From: $from, To: $to)"
}

/**
  * Procedure call between node and procedure. 
  */
case class InterprocEdge(from: CfgNode, to: CfgNode) extends CfgEdge(from, to) {
  override def toString: String = s"InterprocEdge(From: $from, To: $to)"
}

/** Node in the control-flow graph.
 */
trait CfgNode:

  /** Edges to this node from regular statements or ignored procedure calls.
   * 
   * `predCmds` <: Set[RegularEdge | IntraprocEdge]
   */
  val predCmds : mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
  
  /** Edges to this node from procedure calls. Likely empty unless this node is a [[CfgFunctionEntryNode]]
   * 
   * `predCalls` <: Set[InterprocEdge]
   */
  val predCalls: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve predecessor nodes, and the conditions that lead to this node.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return (Node, EdgeCondition)
    */
  def pred(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match 
      case true => predCmds.map(edge => (edge.getFrom, edge.getCond))
      case false => predCmds.union(predCalls).map(edge => (edge.getFrom, edge.getCond))
  }
  
  
  /** Edges to successor nodes, either regular or ignored procedure calls
   * 
   * `succCmds` <: Set[RegularEdge | IntraprocEdge]
   */
  val succCmds : mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Edges to successor procedure calls. Used when walking inter-proc cfg. 
   * 
   * `succCalls` <: Set[InerprocEdge]
   */
  val succCalls: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve succesor nodes and associated conditions, if they exist.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return (Node, EdgeCondition)
    */
  def succ(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match 
      case true => succCmds.map(edge => (edge.getFrom, edge.getCond))
      case false => succCmds.union(succCalls).map(edge => (edge.getFrom, edge.getCond))
  }

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
case class CfgFunctionEntryNode(id: Int = CfgNode.nextId(),
                                data: Procedure
                                ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionEntry] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  def cloneNode(): CfgNode = this.copy(id = CfgNode.nextId())
  

/** Control-flow graph node for the exit of a function.
 */
case class CfgFunctionExitNode( id: Int = CfgNode.nextId(),
                                data: Procedure
                              ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionExit] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  def cloneNode(): CfgNode = this.copy(id = CfgNode.nextId())

/** Control-flow graph node for a block.
 * 
 * NOTE: deprecate?
 */
case class CfgBlockEntryNode( id: Int = CfgNode.nextId(),
                              data: Block
                            ) extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockEntry] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  def cloneNode(): CfgNode = this.copy(id = CfgNode.nextId())

/** Control-flow graph node for a block.
 * 
 * NOTE: deprecate?
 */
case class CfgBlockExitNode(  id: Int = CfgNode.nextId(),
                              data: Block
                            )  extends CfgNodeWithData[Block]:
  override def toString: String = s"[BlockExit] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  def cloneNode(): CfgNode = this.copy(id = CfgNode.nextId())

/** Control-flow graph node for a command (statement or jump).
 */
case class CfgCommandNode(  id: Int = CfgNode.nextId(),
                            data: Command
                            ) extends CfgNodeWithData[Command]:
  override def toString: String = s"[Stmt] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  def cloneNode(): CfgNode = this.copy(id = CfgNode.nextId())

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
  def addNode(node: CfgNode): Unit = 
    nodes += node

  /**
   * Returns a Graphviz dot representation of the CFG.
   * Each node is labeled using the given function labeler.
   */
  /*
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
  }*/

  def toDot(labeler: CfgNode => String, idGen: (CfgNode, Int) => String): String = {
    return "PLACEHOLDER"
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
    program.procedures.foreach(
      proc => cfgForProcedure(proc)
    )



    // Add 

    cfg
  }

  private def cfgForProcedure(proc: Procedure) = {

    val funcEntryNode = CfgFunctionEntryNode(data = proc)
    val funcExitNode  = CfgFunctionExitNode(data = proc)
    cfg.addNode
    
    // Get list of statements
  }

  private def cfgForBlock(block: Block) = {

  }

  /**
    * Recursively add 
    * 
    * @param procs
    * @param inlineAmount
    * @return
    */
  private def inlineProcedures(procs: Set[CfgFunctionEntryNode], inlineAmount: Int): Unit = {
    @assert(inlineAmount >= 0)

    if (inlineAmount == 0) {
      return
    }

    // procs.foreach(proc => )
  }
