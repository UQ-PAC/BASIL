package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.break;
import analysis.Fresh.next




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
   *  consideration. We don't expect too many edges for any given node, and so the increased storage is 
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

      case _ => println(s"[!] Unexpected edge combination when adding cfg edge between ${from} -> ${to}.")
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
  val procToCalls: mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]] = mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]]()
  // Mapping from procedure entry instances to procedure call nodes within that procedure's instance (`CfgCommandNode.data <: DirectCall`)
  //    Updated on first creation of  
  val callToNodes: mutable.HashMap[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]] = mutable.HashMap[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]]() 
  // Mapping from procedures to nodes in the cfg which call that procedure
  val procToCallers: mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]] = mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]]()

  /** Generate the cfg for each function of the program. NOTE: is this functionally different to a constructor?
   *    Do we ever expect to generate a CFG from any other data structure? If not then the `class` could probably 
   *    be absorbed into this object. 
   * 
   * @param program
   *  Basil IR of the program
   * @param inlineLimit
   *  How many levels deep to inline function calls. By default, don't inline - this is equivalent to an intra-procedural CFG.
   */
  def fromIR(program: Program, inlineLimit: Int = 0): ProgramCfg = {
    require(inlineLimit >= 0, "Can't inline procedures to negative depth...")
    println("Generating CFG...")

    // Create CFG for individual procedures
    program.procedures.foreach(
      proc => cfgForProcedure(proc)
    )

    // Inline functions up to `inlineLimit` level
    val procCallNodes: Set[CfgCommandNode] = procToCalls.values.flatten.toSet
    inlineProcedureCalls(procCallNodes, inlineLimit)

    cfg
  }

  /**
    * TODO
    *
    * @param proc
    */
  private def cfgForProcedure(proc: Procedure): Unit = {
    val funcEntryNode: CfgFunctionEntryNode = CfgFunctionEntryNode(data = proc)
    val funcExitNode: CfgFunctionExitNode   = CfgFunctionExitNode(data = proc)
    cfg.addNode(funcEntryNode)
    cfg.addNode(funcExitNode)

    procToCfg += (proc -> (funcEntryNode, funcExitNode))

    val visitedBlocks: mutable.HashMap[Block, CfgCommandNode] = mutable.HashMap[Block, CfgCommandNode]()
    //val visitedBlocks: mutable.Set[Block] = mutable.Set[Block]()
    
    // Visit each node
    visitBlock(proc.blocks.head, funcEntryNode, TrueLiteral)


    /**
      * TODO
      * 
      * @param block
      * @param prevBlockEnd
      * @param cond
      */
    def visitBlock(block: Block, prevBlockEnd: CfgNode, cond: Expr): Unit = {

      

      // Handle first node - guaranteed to have at least one
      var prevStmtNode: CfgCommandNode = CfgCommandNode(data = block.statements.head)
      cfg.addEdge(prevBlockEnd, prevStmtNode, cond)

      /* OPTIMISATION
      Do this if all the jump "visits" end up being the below. Can just add edges at start and use this as a recursion check.
      if (visitedBlocks.contains(prevBlockEnd)) {
        cfg.addEdge(prevBlockEnd, prevStmtNode, cond)
        return;
      }
      
      if do, replace with this:

      visitBlock(retBlock, callNode, TrueLiteral) // true because can only return to one location (deterministic)
      */
      

      visitedBlocks += (block -> prevStmtNode)  // record entry node for this block

      // Process the rest of the statements
      block.statements.tail.foreach(
        stmt => 
          val stmtNode: CfgCommandNode = CfgCommandNode(data = stmt)
          cfg.addEdge(prevStmtNode, stmtNode)
          prevStmtNode = stmtNode
      )

      // Process jump(s) at end of basic block
      block.jumps.foreach {
        case goto: GoTo =>
          // TODO: negate the condition for cases of two GoTos. Does this happen for any other kind of jumps?
          val targetBlock: Block = goto.target
          val targetCond: Expr = goto.condition match {
            case Some(c) => c
            case None => TrueLiteral
          }

          if (visitedBlocks.contains(targetBlock)) {
            // Already visited the target, add edge and return

            // SEE OPTIMISATION ABOVE
            val targetBlockEntry: CfgCommandNode = visitedBlocks(targetBlock)
            cfg.addEdge(prevStmtNode, targetBlockEntry, targetCond)
          } else {
            visitBlock(targetBlock, prevStmtNode, targetCond)
          }

        case dCall: DirectCall =>
          val callNode: CfgCommandNode = CfgCommandNode(data = dCall)
          val targetProc: Procedure = dCall.target
          val targetCond: Expr = dCall.condition match {
            case Some(c) => c
            case None => TrueLiteral
          }

          // Conditional branch to this call 
          cfg.addEdge(prevStmtNode, callNode, targetCond)

          // Record process association
          procToCalls(proc) += callNode
          procToCallers(targetProc) += callNode
          callToNodes(funcEntryNode) += callNode

          // Handle the return location
          dCall.returnTarget match {
            case Some(retBlock) => 
              if(visitedBlocks.contains(retBlock)) {
                val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                cfg.addEdge(callNode, retBlockEntry)
              } else {
                visitBlock(retBlock, callNode, TrueLiteral)
              }
            case None => 
              // At end of 
              println("[?] Direct - Do we have to handle the \"no return\" case?")
          }

        case iCall: IndirectCall =>
          val callNode: CfgCommandNode = CfgCommandNode(data = iCall)
          val targetCond: Expr = iCall.condition match {
            case Some(c) => c
            case None => TrueLiteral
          }

          // Conditional branch to this call
          cfg.addEdge(prevStmtNode, callNode, targetCond)

          // Record process association
          procToCalls(proc) += callNode
          callToNodes(funcEntryNode) += callNode

          // R30 is the link register - this stores the address to return to 
          //  is this accurate in this sense? Shouldn't we be getting the stored value?
          if (iCall.target.isRegister("R30")) {
            cfg.addEdge(callNode, funcExitNode, targetCond)
          }

          iCall.returnTarget match {
            case Some(retBlock) => 
              if(visitedBlocks.contains(retBlock)) {
                val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                cfg.addEdge(callNode, retBlockEntry)
              } else {
                visitBlock(retBlock, callNode, TrueLiteral)
              }
            case None =>
              println("[?] Indirect - Do we have to handle the \"no return\" case?")
          }
      }
    }
  }

  

  /** Recursively add 
    * 
    * @param procNodes
    * @param inlineAmount
    */
  private def inlineProcedureCalls(procNodes: Set[CfgCommandNode], inlineAmount: Int): Unit = {
    assert(inlineAmount >= 0)

    if (inlineAmount == 0 || procNodes.isEmpty) {
      return;
    }
    
    // Set of procedure calls to be discovered by inlining the ones in `procNodes`
    val nextProcNodes: mutable.Set[CfgCommandNode] = mutable.Set[CfgCommandNode]()

    procNodes.foreach(
      procNode => 
        require(procNode.data.isInstanceOf[DirectCall], s"Trying to inline a non-function call instruction: ${procNode}")

        // Retrieve information about the call to the target procedure
        val targetCall: DirectCall = procNode.data.asInstanceOf[DirectCall]
        val targetProc: Procedure = targetCall.target
        val targetCond: Expr = targetCall.condition match {
          case Some(c) => c
          case None => TrueLiteral
        }
       
        val (procEntry, procExit) = cloneProcedureCFG(targetProc)

        // Add link between call node and the procedure's `Entry`. 
        cfg.addEdge(procNode, procEntry, targetCond)

        // Link the procedure's `Exit` to the return point. There should only be one.
        assert(procNode.succ(intra = true).size == 1, s"More than 1 return node... ${procNode} has ${procNode.succ(intra=true)}")
        val returnNode = procNode.succ(intra = true).head
        cfg.addEdge(procExit, returnNode)

        // Add new (un-inlined) function calls to be inlined
        nextProcNodes ++= callToNodes(procEntry)
    )

    inlineProcedureCalls(nextProcNodes.toSet, inlineAmount - 1)
  }

  /** Clones the intraproc-cfg of the given procedure, with unique CfgNode ids.
    * Adds the new nodes to the cfg, and returns the start/end nodes of the new 
    * procedure cfg.
    *
    * @param proc 
    *   The procedure to clone (used to index the pre-computed cfgs)
    * @return 
        (CfgFunctionEntryNode, CfgFunctionExitNode) of the cloned cfg
    */
  private def cloneProcedureCFG(proc: Procedure): (CfgFunctionEntryNode, CfgFunctionExitNode) = {

    val (entryNode: CfgFunctionEntryNode, exitNode: CfgFunctionExitNode) = procToCfg(proc)
    val (newEntry: CfgFunctionEntryNode, newExit: CfgFunctionExitNode) = (entryNode.copyNode(), exitNode.copyNode())

    // Entry is guaranteed to only have one successor (by our cfg design)
    var currNode: CfgNode = entryNode.succ(intra = true).head
    visitNode(currNode, newEntry, TrueLiteral)
 
    /** Walk this proc's cfg until we reach the exit node on each branch. We 
      *   do this recursively, tracking the previous node, to account for branches
      *   and loops.
      * 
      * We can't represent the parameters as an edge as one node comes from the old cfg, 
      *   and the other from the new cfg.
      *
      * @param node 
      *   Node in the original procedure's cfg we're up to cloning
      * @param prevNewNode 
      *   The originating node in the new clone's cfg
      * @param cond 
      *   The condition leading to `node` from `prevNewNode`
      */
    def visitNode(node: CfgNode, prevNewNode: CfgNode, cond: Expr): Unit = {

      if (node == exitNode) {
        cfg.addEdge(prevNewNode, newExit, cond)
        return;
      }

      // Link this node with predecessor in the new cfg
      val newNode = node.copyNode()
      cfg.addEdge(prevNewNode, newNode, cond)

      // Update cfg information
      node match {
        case n: CfgCommandNode => 
          n.data match {
            case d: DirectCall => 
              procToCalls(proc) += newNode        // This procedure (general) is calling another procedure
              callToNodes(entryNode) += newNode   // This procedure (specfic) is calling another procedure
              procToCallers(d.target) += newNode  // Target of this call has a new caller
            case i: IndirectCall =>
              procToCalls(proc) += newNode
              callToNodes(entryNode) += newNode
            case _ => 
          }
        case _ => 
      }

      // Get intra-cfg succesors
      val outEdges: mutable.Set[CfgEdge] = node.succEdges(intra = true)
      outEdges.foreach(
        edge => 
          visitNode(edge.getTo, newNode, edge.getCond) 
      )
    }

    (newEntry, newExit)
  }
