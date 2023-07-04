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
trait CfgEdge(from: CfgNode, to: CfgNode, cond: Expr):
  def getFrom: CfgNode = from
  def getTo: CfgNode = to
  def getCond: Expr = cond

/**
  * Edge between two command nodes in the CFG. Used for `GoTo` branches 
  * as well (with a condition added for the branch).
  */
case class RegularEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"RegularEdge(From: $from, To: $to)"
}

/**
  * Edge which skips over a procedure call. Used for "jumping over" function 
  * calls, i.e. in an intra-procedural CFG walk. 
  */
case class IntraprocEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"IntraprocEdge(From: $from, To: $to)"
}

/**
  * Procedure call between node and procedure. 
  */
case class InterprocEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"InterprocEdge(From: $from, To: $to)"
}

/** Node in the control-flow graph.
 */
trait CfgNode:

  /** Edges to this node from regular statements or ignored procedure calls.
   * 
   * `predIntra` <: Set[RegularEdge | IntraprocEdge]
   */
  val predIntra : mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
  
  /** Edges to this node from procedure calls. Likely empty unless this node is a [[CfgFunctionEntryNode]]
   * 
   * `predInter` <: Set[RegularEdge | InterprocEdge]
   */
  val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve predecessor nodes, and the conditions that lead to this node.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return (Node, EdgeCondition)
    */
  def pred(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match 
      case true => predIntra.map(edge => (edge.getFrom, edge.getCond))
      case false => predInter.map(edge => (edge.getFrom, edge.getCond))
  }
  
  
  /** Edges to successor nodes, either regular or ignored procedure calls
   * 
   * `succIntra` <: Set[RegularEdge | IntraprocEdge]
   */
  val succIntra : mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Edges to successor procedure calls. Used when walking inter-proc cfg. 
   * 
   * `succInter` <: Set[RegularEdge | InterprocEdge]
   */
  val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve successor nodes to this node.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return Set of successor nodes
    */
  def succ(intra: Boolean): mutable.Set[CfgNode] = {
    intra match
      case true => succIntra.map(edge => edge.getTo)
      case false => succInter.map(edge => edge.getTo)
  }

  /** Retrieve successor edges from this node.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return Set of successor edges
    */
  def succEdges(intra: Boolean): mutable.Set[CfgEdge] = if (intra) succIntra else succInter

  /** Retrieve succesor nodes and associated conditions, if they exist.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return (Node, EdgeCondition)
    */
  def succConds(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match 
      case true => succIntra.map(edge => (edge.getTo, edge.getCond))
      case false => succInter.map(edge => (edge.getTo, edge.getCond))
  }

  /** Unique identifier. */
  val id: NodeId = CfgNode.nextId()
  def copyNode[T](): T

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
                                override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                data: Procedure
                              ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionEntry] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  override def copyNode(): CfgFunctionEntryNode = this.copy(
    id = CfgNode.nextId(), 
    succInter = mutable.Set[CfgEdge](),
    succIntra = mutable.Set[CfgEdge](),
    predInter = mutable.Set[CfgEdge](),
    predIntra = mutable.Set[CfgEdge]()
    )
  
  

/** Control-flow graph node for the exit of a function.
 */
case class CfgFunctionExitNode(
                                override val id: Int = CfgNode.nextId(),
                                override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                                data: Procedure
                              ) extends CfgNodeWithData[Procedure]:
  override def toString: String = s"[FunctionExit] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  override def copyNode(): CfgFunctionExitNode = this.copy(
    id = CfgNode.nextId(), 
    succInter = mutable.Set[CfgEdge](),
    succIntra = mutable.Set[CfgEdge](),
    predInter = mutable.Set[CfgEdge](),
    predIntra = mutable.Set[CfgEdge]()
    )

/** Control-flow graph node for a command (statement or jump).
 */
case class CfgCommandNode(
                              override val id: Int = CfgNode.nextId(),
                              override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                              override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                              override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                              override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
                              data: Command
                            ) extends CfgNodeWithData[Command]:
  override def toString: String = s"[Stmt] $data"
  /** Copy this node, but give unique ID - do it ourselves because Scala won't */
  override def copyNode(): CfgCommandNode = this.copy(
    id = CfgNode.nextId(), 
    succInter = mutable.Set[CfgEdge](),
    succIntra = mutable.Set[CfgEdge](),
    predInter = mutable.Set[CfgEdge](),
    predIntra = mutable.Set[CfgEdge]()
    )

/** 
 * A control-flow graph. Provides the ability to walk it as both an intra and inter 
 * procedural CFG. Also stores a mapping between CFG nodes and their originating 
 * Basil IR blocks.
 * 
 */
class ProgramCfg:

  // CFG Block -> IR Block
  var nodeToBlock: mutable.Map[CfgNode, Block] = mutable.Map[CfgNode, Block]()
  var edges: ListBuffer[CfgEdge] = ListBuffer[CfgEdge]()
  var nodes: ListBuffer[CfgNode] = ListBuffer[CfgNode]()

  /** Add an outgoing edge from the current node, taking into account any conditionals 
   *  on this jump. Note that we have some duplication of storage here - this is a performance 
   *  consideration. We don't expect >4 edges for any given node, and so the increased storage is 
   *  relatively minimal. This saves having to filter / union sets when trying to retrieve only 
   *  an intra/inter cfg, hopefully improving computation time.
   */
  def addEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): Unit = {

    var newEdge: CfgEdge = _;

    (from, to) match {
      // Calling procedure (follow)
      case (from: CfgCommandNode, to: CfgFunctionEntryNode) =>
        newEdge = InterprocEdge(from, to, cond)
        from.succInter += newEdge
        to.predInter += newEdge
      // Returning from procedure
      case (from: CfgFunctionExitNode, to: CfgNode) =>
        newEdge = InterprocEdge(from, to, cond)
        from.succInter += newEdge
        to.predInter += newEdge
      // First instruction of procedure
      case (from: CfgFunctionEntryNode, to: CfgNode) =>
        newEdge = RegularEdge(from, to, cond)
        from.succIntra += newEdge
        from.succInter += newEdge
        to.predIntra += newEdge
        to.predInter += newEdge
      // Intra-procedural flow of instructions
      case (from: CfgCommandNode, to: CfgCommandNode)  =>
        (from, to) match {
          // Calling procedure (skip)
          case from.data: (DirectCall | IndirectCall) => 
            newEdge = IntraprocEdge(from, to, cond)
            from.succIntra += newEdge
            to.predIntra += newEdge
          // Regular instruction flow
          case _ =>
            newEdge = RegularEdge(from, to, cond)
            from.succIntra += newEdge
            from.succInter += newEdge
            to.predInter += newEdge
            to.predIntra += newEdge
        }

      case _ => println("[!] Unexpected edge combination when adding cfg edge.")
    }

    edges += newEdge
    nodes += from
    nodes += to
  }
    

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
  val cfg: ProgramCfg = ProgramCfg()
  
  // Mapping from procedures to the start of their individual (intra) cfgs
  val procToCfg: mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)] = mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)]()
  // Mapping from procedures to procedure call nodes (all the calls made within this procedure, including inlined functions)
  val procToCalls: mutable.HashMap[Procedure, Set[CfgCommandNode]] = mutable.HashMap[Procedure, Set[CfgCommandNode]]()
  // Mapping from procedures to nodes in the cfg which call that procedure
  val procToCallers: mutable.HashMap[Procedure, Set[CfgCommandNode]] = mutable.HashMap[Procedure, Set[CfgCommandNode]]()

  /** Generate the cfg for each function of the program. NOTE: is this functionally different to a constructor?
   *    Do we ever expect to generate a CFG from any other data structure? If not then the `class` could probably 
   *    be absorbed into this object. 
   * 
   * @param program
   *  Basil IR of the program
   * @param inlineLimit
   *  How many levels deep to inline function calls. By defualt, don't inline - this is equivalent to an intra-procedural CFG.
   */
  def fromIR(program: Program, inlineLimit: Int = 0): ProgramCfg = {
    require(inlineLimit >= 0, "Can't inline procedures to negative depth...")
    println("Generating CFG...")

    // Create CFG for individual functions
    program.procedures.foreach(
      proc => cfgForProcedure(proc)
    )

    // Inline functions up to `nth` level

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

  /** Recursively add 
    * 
    * @param procNodes
    * @param inlineAmount
    */
  private def inlineProcedureCalls(procNodes: Set[CfgCommandNode], inlineAmount: Int): Unit = {
    assert(inlineAmount >= 0)

    if (inlineAmount == 0) {
      return;
    }

    val currProcNodes: Set[CfgCommandNode] = procToCalls.values.flatten.toSet.diff(procNodes)

    // Update `procToCalls` etc
    // Add edges to start and from end of proc call (returned from `cloneProcedureCFG`)

    // Q : will editing these inlined procedures all affect the same IR procedure?
    // TODO: review this section. There's almost certainly a better way to do it. 
    procNodes.foreach(
      procNode => 
        val targetProc: Procedure = procNode.data.asInstanceOf[DirectCall].target
        val (procEntry, procExit) = cloneProcedureCFG(targetProc)

        procToCallers(targetProc) += procNode

        cfg.addEdge(procNode, procEntry)

        procNode.succConds(intra = true).foreach(
          (succ,cond) => cfg.addEdge(procExit, succ, cond)
        )
    )

    val freshProcNodes: Set[CfgCommandNode] = procToCalls.values.flatten.toSet.diff(currProcNodes)
    inlineProcedureCalls(freshProcNodes, inlineAmount - 1)
  }

  /** Clones the intraproc-cfg of the given procedure, with unique CfgNode ids.
    * Adds the new nodes to the cfg, and returns the start/end nodes of the new 
    * procedure cfg.
    *
    * @param proc - the procedure to clone (used to index the pre-computed cfgs)
    * @return (CfgFunctionEntryNode, CfgFunctionExitNode)
    */
  private def cloneProcedureCFG(proc: Procedure): (CfgFunctionEntryNode, CfgFunctionExitNode) = {

    val (entryNode: CfgFunctionEntryNode, exitNode: CfgFunctionExitNode) = procToCfg(proc)
    val newEntry: CfgFunctionEntryNode = CfgFunctionEntryNode(data = entryNode.data)
    val newExit : CfgFunctionExitNode  = exitNode.copyNode()

    // Entry is guaranteed to only have one successor (by our cfg design)
    var currNode: CfgNode = entryNode.succ(intra = true).head
    visitNode(currNode, newEntry, TrueLiteral)
 
    /** Walk this proc's cfg until we reach the exit node on each branch. We 
      *   do this recursively, tracking the previous node, to account for branches
      *   and loops.
      * 
      * We can't represent this as an edge as one node comes from the old cfg, 
      *   and the other from the new cfg.
      *
      * @param node - node in the original procedure's cfg we're up to cloning
      * @param prevNewNode - the originating node in the new clone's cfg
      * @param cond - the condition leading to `node` from `prevNewNode`
      */
    def visitNode(node: CfgNode, prevNewNode: CfgNode, cond: Expr): Unit = {

      if (node == exitNode) {
        cfg.addEdge(prevNewNode, newExit, cond)
        return;
      }

      // Link this node with predecessor in the new cfg
      val newNode = node.copyNode()
      cfg.addEdge(prevNewNode, newNode, cond)

      // Get intra-cfg succesors
      val outEdges: mutable.Set[CfgEdge] = node.succEdges(intra = true)
      outEdges.foreach(
        edge => 
          visitNode(edge.getTo, newNode, edge.getCond) 
      )
    }

    (newEntry, newExit)
  }
