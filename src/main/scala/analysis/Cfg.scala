package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotRegularArrow, DotInterArrow, DotIntraArrow, DotGraph, DotNode}

import scala.collection.mutable.ListBuffer

import scala.util.control.Breaks.break;
import analysis.Fresh.next




/** Node in the control-flow graph.
 */
object CfgNode:

  var id: Int = 0

  def nextId(): Int =
    id += 1
    id

/** Edge type. 
  * 
  * `cond` : condition if this is a conditional edge. By default, assume True.
  */
trait CfgEdge(from: CfgNode, to: CfgNode, cond: Expr):
  def getFrom: CfgNode = from
  def getTo: CfgNode = to
  def getCond: Expr = cond

/** Edge between two command nodes in the CFG. Used for `GoTo` branches 
  * as well (with a condition added for the branch).
  */
case class RegularEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"RegularEdge(From: $from, To: $to)"
}

/** Edge which skips over a procedure call. Used for "jumping over" function 
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

/** Node in the control-flow graph. Each node has a (simple incremental) unique identifier used to distinguish
  *   it from other nodes in the cfg - this is mainly used for copying procedure cfgs when inlining them.  
  * 
  * Each node will store four separate sets: ingoing/outgoing of both inter-/intra-procedural CFG edges. 
  *   Both intra and inter will also store regular edges in the cfg. This is duplication of storage, however
  *   is done so knowingly.
  *   
  *   By separating sets into inter/intra we are able to return these directly without doing any processing.
  *   Alternative means of achieving this same behaviour would involve some form of set operations, or
  *   a filter operation, both of which can be expensive, especially as the successors/predecessors will 
  *   be accessed frequently by analyses. Additionally, inspecting the space complexity of these sets, we note 
  *   that their sizes should be relatively limited:
  *     a. #(outgoing edges) <= 2
  *     b. #(incoming edges) ~ N
  *   Thus in `succIntra` + `succInter` we have at most 4 elements. For `predIntra` + `predInter` we have a maximum of 2N, resulting 
  *     from the case that this node is a block entry that is jumped to by N other nodes. 
  *   It should be noted that in the majority of cases (statements which are neither the start of blocks nor jumps), both sets will
  *     be of size 1, making the storage complexity negligible.
  * 
  * In the case a node is part of an inlined function (i.e. it is part of a procedure which is the result of a call), all edges 
  *   will be resolved as general edges, making no differentiation between inter/intra edges. The reason for this is that when 
  *   walking an inter-procedural cfg we don't expect to follow intra-procedural edges within those inlined-procedures, until we 
  *   hit the inline-depth. At that depth, we then follow the intra-procedural edge, and have no need for inter-procedural edges.
  *   In essence we treat the calls as if they don't happen, and treat the first statement of the callee as the next instruction in 
  *   the caller, and so is just a regular execution (i.e., directly inlining the callee's body into our procedure) The easiest way to 
  *   do that with the api below (which is preserved from previous iterations of this tool / tip as the interface for analyses to walk 
  *   the cfg) is to have that behaviour encoded in the structure of the cfg. Thus, for an inline depth i:
  *     a. i == 1     : inter- and intra- procedural edges are distinguished and stored
  *     b. 1 < i < n  : inter-procedural edges are stored as regular edges, intra-procedural edges are discarded 
  *     c. i == n     : intra-procedural edges are stored as regular edges, inter-procedural edges are discarded
  * 
  * 
  *   TODO: if we decide to go with a diamond structure for inter-procedural calls we should update this.
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

  /** Retrieve predecessor nodes to this node.
    * 
    * @param intra if true, only walk the intraprocedural cfg.
    * @return Set of predecessor nodes
    */
  def pred(intra: Boolean): mutable.Set[CfgNode] = {
    intra match 
      case true => predIntra.map(edge => edge.getFrom)
      case false => predInter.map(edge => edge.getFrom)
  }

  /** Retrieve predecessor edges to this node.
    *
    * @param intra if true, only walk the intraprocedural cfg
    * @return Set of predecessor edges
    */
  def predEdges(intra: Boolean): mutable.Set[CfgEdge] = if (intra) predIntra else predInter

  /** Retrieve predecessor nodes and associated conditions, if they exist
    *
    * @param intra if true, only walk the intraprocedural cfg
    * @return (Node, EdgeCondition)
    */
  def predConds(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
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
  val id: Int = CfgNode.nextId()
  def copyNode(): CfgNode

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
  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgFunctionEntryNode = CfgFunctionEntryNode(data = this.data)
  
  

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
  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgFunctionExitNode = CfgFunctionExitNode(data = this.data)

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
  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgCommandNode = CfgCommandNode(data = this.data)


/** 
 * A control-flow graph. Provides the ability to walk it as both an intra and inter 
 * procedural CFG. Also stores a mapping between CFG nodes and their originating 
 * Basil IR blocks.
 * 
 */
class ProgramCfg:

  // CFG Block -> IR Block
  var nodeToBlock: mutable.Map[CfgNode, Block] = mutable.Map[CfgNode, Block]()
  var edges: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
  var nodes: mutable.Set[CfgNode] = mutable.Set[CfgNode]()

  /** Add an outgoing edge from the current node, taking into account any conditionals 
   *  on this jump. Note that we have some duplication of storage here - this is a performance 
   *  consideration. We don't expect too many edges for any given node, and so the increased storage is 
   *  relatively minimal. This saves having to filter / union sets when trying to retrieve only 
   *  an intra/inter cfg, hopefully improving computation time.
   */
  def addEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): Unit = {

    var newEdge: CfgEdge = RegularEdge(from, to, cond); // Placeholder 

    (from, to) match {
      // Ignored procedure
      case (from: CfgFunctionEntryNode, to: CfgFunctionExitNode) =>
        newEdge = RegularEdge(from, to, TrueLiteral)
        from.succIntra += newEdge
        from.succInter += newEdge
        to.predIntra += newEdge
        to.predInter += newEdge
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
      case (from: CfgCommandNode, to: (CfgCommandNode | CfgFunctionExitNode))  =>
        from.data match {
          // Calling procedure (skip)
          case call: (DirectCall | IndirectCall) => 
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
  
  def toDot(labeler: CfgNode => String, idGen: (CfgNode, Int) => String): String = {
    val dotNodes = mutable.Map[CfgNode, DotNode]()
    var dotArrows = mutable.ListBuffer[DotArrow]()
    var uniqueId = 0
    nodes.foreach { n =>
      dotNodes += (n -> new DotNode(s"${idGen(n, uniqueId)}", labeler(n)))
      uniqueId += 1
    }
    nodes.foreach { n =>

      // This can just be a match case on the types of edges?

      val regularOut: Set[(CfgNode, Expr)] = n.succConds(true).intersect(n.succConds(false)).toSet
      val intraOut: Set[(CfgNode, Expr)] = n.succConds(true).subtractAll(regularOut).toSet
      val interOut: Set[(CfgNode, Expr)] = n.succConds(false).subtractAll(regularOut).toSet

      regularOut.foreach {
        (dest,cond) =>
          cond match {
            case TrueLiteral =>
              dotArrows += DotRegularArrow(dotNodes(n), dotNodes(dest))
            case _ =>
              dotArrows += DotRegularArrow(dotNodes(n), dotNodes(dest), cond.toString())
          }
          // dotArrows += DotRegularArrow(dotNodes(n), dotNodes(dest))
      }

      intraOut.foreach {
        (dest,cond) =>
          cond match {
            case TrueLiteral =>
              dotArrows += DotIntraArrow(dotNodes(n), dotNodes(dest))
            case _ =>
              dotArrows += DotIntraArrow(dotNodes(n), dotNodes(dest), cond.toString())
          }
          // dotArrows += DotIntraArrow(dotNodes(n), dotNodes(dest))
      }

      interOut.foreach {
        (dest,cond) =>
          cond match {
            case TrueLiteral => 
              dotArrows += DotInterArrow(dotNodes(n), dotNodes(dest))
            case _ =>
              dotArrows += DotInterArrow(dotNodes(n), dotNodes(dest), cond.toString())
          }
          // dotArrows += DotInterArrow(dotNodes(n), dotNodes(dest))
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
  val cfg: ProgramCfg = ProgramCfg()
  
  // Mapping from procedures to the start of their individual (intra) cfgs
  val procToCfg: mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)] = 
    mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)]()
  // Mapping from procedures to procedure call nodes (all the calls made within this procedure, including inlined functions)
  val procToCalls: mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]] = 
    mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]]()
  // Mapping from procedure entry instances to procedure call nodes within that procedure's instance (`CfgCommandNode.data <: DirectCall`)
  //    Updated on first creation of  
  val callToNodes: mutable.Map[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]] = 
    mutable.Map[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]]()
  // Mapping from procedures to nodes in the cfg which call that procedure
  val procToCallers: mutable.Map[Procedure, mutable.Set[CfgCommandNode]] = 
    mutable.Map[Procedure, mutable.Set[CfgCommandNode]]()

  /** Generate the cfg for each function of the program. NOTE: is this functionally different to a constructor?
   *    Do we ever expect to generate a CFG from any other data structure? If not then the `class` could probably 
   *    be absorbed into this object. 
   * 
   * @param program
   *  Basil IR of the program
   * @param inlineLimit
   *  How many levels deep to inline function calls. By default, don't inline - this is equivalent to an intra-procedural CFG.
   */
  def fromIR(program: Program, inlineLimit: Int = 1): ProgramCfg = {
    require(inlineLimit >= 0, "Can't inline procedures to negative depth...")
    println("[!] Generating CFG...")

    // Have to initialise these manually. Scala maps have a `.withDefaulValue`, but this is buggy and doesn't
    //  behave as you would expect. https://github.com/scala/bug/issues/8099
    // We don't initialise `procToCfg` here, because it will never be accessed before `cfgForProcedure`
    //  it's set in `cfgForProcedure`
    program.procedures.foreach(
      proc =>
        procToCalls += (proc -> mutable.Set[CfgCommandNode]())
        procToCallers += (proc -> mutable.Set[CfgCommandNode]())
    )

      // Create CFG for individual procedures
    program.procedures.foreach(
      proc => cfgForProcedure(proc)
    )

    // Inline functions up to `inlineLimit` level
    val procCallNodes: Set[CfgCommandNode] = procToCalls.values.flatten.toSet
    inlineProcedureCalls(procCallNodes, inlineLimit)

    cfg
  }

  /** Create a CFG for the given IR procedure. The start of the CFG for a procedure is identified by 
    *   its `CfgFunctionEntryNode, and its closure is identified by the `CfgFunctionExitNode`. 
    * 
    * 
    *
    * @param proc
    */
  private def cfgForProcedure(proc: Procedure): Unit = {
    val funcEntryNode: CfgFunctionEntryNode = CfgFunctionEntryNode(data = proc)
    val funcExitNode: CfgFunctionExitNode   = CfgFunctionExitNode(data = proc)
    cfg.addNode(funcEntryNode)
    cfg.addNode(funcExitNode)

    procToCfg += (proc -> (funcEntryNode, funcExitNode))
    callToNodes += (funcEntryNode -> mutable.Set[CfgCommandNode]())

    // Procedure has no content (in our case this probably means it's an ignored procedure, e.g., an external function such as @printf)
    if (proc.blocks.size == 0) {
      cfg.addEdge(funcEntryNode, funcExitNode)
      return;
    }

    // Track blocks we've already processed so we don't double up
    val visitedBlocks: mutable.HashMap[Block, CfgCommandNode] = mutable.HashMap[Block, CfgCommandNode]()    
    
    visitBlock(proc.blocks.head, funcEntryNode, TrueLiteral)

      /** Add a block to the CFG. A block in this case is a basic block, so contains a 
        *   list of consecutive statements followed by a jump to another block. We process 
        *   statements in this block (if they exist), and then follow the jump to recurse through
        *   all other blocks. 
        * 
        *   This recursive approach is effectively a "reaches" approach, and will miss cases that
        *   we encounter a jump we can't resolve, or cases where the lifter has not identified a section of 
        *   code. In each case:
        *     a. The only jumps we can't resolve are indirect calls. It's the intent of the tool to attempt to resolve
        *         these through analysis however. The CFG can then be updated as these are resolved to incorporate their
        *         jumps. In construction we do a simple check for register R30 to identify if an indirect call is a return.
        *     b. If the lifter has failed to identify a region of code, then the problem exists at the lifter level. In 
        *         that case we need a way to coerce the lifter into identifying it, or to use a new lifter.
        * 
        *   These visitations will also only produce the intra-procedural CFG - the burden of "creating" the inter-procedural 
        *     CFG is left to the inlining processes later. The benefit of doing this is that we can completely resolve a 
        *     procedure's CFG without jumping to other procedures mid-way through processing, which assures we don't have any issues 
        *     with referencing nodes before they exist. Essentially this is a depth-first approach to CFG 
        *     construction, as opposed to a breadth-first. 
        * 
        * 
        * @param block          the block being added to the CFG.
        * @param prevBlockEnd   preceding block's end node (jump)
        * @param cond           condition on the jump from `prevNode` to the first statement of this block
        */
      def visitBlock(block: Block, prevBlockEnd: CfgNode, cond: Expr): Unit = {
        
        // TODO: this can probably be abstracted to `visitStmts` and `visitJumps`. Perhaps this is more readable though?
        block.statements.size match {
          case i if i > 0 =>
            // Block contains some statements
            val endStmt: CfgCommandNode = visitStmts(block.statements.toList, prevBlockEnd, cond)
            visitJumps(block.jumps.toList, endStmt, TrueLiteral, solitary = false)
          case _ =>
            // Only jumps in this block
            visitJumps(block.jumps.toList, prevBlockEnd, cond, solitary = true)
        }


        /** If a block has statements, we add them to the CFG. Blocks in this case are basic blocks,
          *   so we know consecutive statements will be linked by an unconditional, regular edge.
          *
          * @param stmts      statements in this block
          * @param prevNode   preceding block's end node (jump)
          * @param cond       condition on the jump from `prevNode` to the first statement of this block
          * @return           the last statement's CFG node
          */
        def visitStmts(stmts: List[Statement], prevNode: CfgNode, cond: Expr): CfgCommandNode = {

          val firstNode: CfgCommandNode = CfgCommandNode(data = stmts.head)
          cfg.addEdge(prevNode, firstNode, cond)
          visitedBlocks += (block -> firstNode) // This is guaranteed to be entrance to block

          if (stmts.size == 1) {
            // Statements start and end here for this block
            return firstNode
          }

          var prevStmtNode: CfgCommandNode = firstNode

          // `tail` takes everything after the first element
          stmts.tail.foreach(
            stmt =>
              val stmtNode: CfgCommandNode = CfgCommandNode(data = stmt)
              cfg.addEdge(prevStmtNode, stmtNode)
              prevStmtNode = stmtNode
          )

          prevStmtNode
        }

        /** All blocks end with jump(s). Add these to the CFG and visit their target blocks for processing.
          *
          * @param jmps       jumps in the block being processed
          * @param prevNode   either the previous statement in the block, or the previous block's end node 
          *                     (in the case that this block contains no statements)
          * @param cond       jump from `prevNode` to this. `TrueLiteral` if `prevNode` is a statement,
          *                     and any `Expr` if `prevNode` is a jump.
          * @param solitary   `True` if this block contains no statements, `False` otherwise
          */
        def visitJumps(jmps: List[Jump], prevNode: CfgNode, cond: Expr, solitary: Boolean): Unit = {

          val jmpNode: CfgCommandNode = CfgCommandNode(data = jmps.head)
          var precNode: CfgNode = prevNode

          if (solitary) {
            /* If the block contains only jumps (no statements), then the "start" of the block is a jump. 
                If this is a direct call, then we simply use that call node as the start of the block.
                However, GoTos in the CFG are resolved as edges, and so there doesn't exist a node to use as
                the start. Thus we introduce a "ghost" node to act as that jump point - it has no functionality 
                and will simply be skipped by analyses.

                Currently we display these nodes in the DOT view of the CFG, however these could be hidden if desired.
            */
            jmps.head match {
              case jmp: GoTo =>
                // `GoTo`s are just edges, so introduce a fake `start of block` that can be jmp'd to
                val ghostNode = CfgCommandNode(data = NOP())
                cfg.addEdge(prevNode, ghostNode, cond)
                precNode = ghostNode
                visitedBlocks += (block -> ghostNode)
              case _ =>
                // (In)direct call - use this as entrance to block
                visitedBlocks += (block -> jmpNode)
            }
          }
          
          /* Possible `jmp` combinations:
              1. DirectCall
              2. IndirectCall
              3. GoTo
              4. GoTo + GoTo
          */
          jmps.head match {
            case goto: GoTo =>
              
              // Process first jump
              var targetBlock: Block = goto.target
              var targetCond: Expr = goto.condition match {
                case Some(c) => c
                case None => TrueLiteral
              }

              // Jump to target block
              if (visitedBlocks.contains(targetBlock)) {
                val targetBlockEntry: CfgCommandNode = visitedBlocks(targetBlock)
                cfg.addEdge(precNode, targetBlockEntry, targetCond)
              } else {
                visitBlock(targetBlock, precNode, targetCond)
              }

              /* If the first GoTo's condition is not `True`, then we know there must be a second conditional jump.
                  We could equivalently check if `jmps` has 2 jumps, though this emphasises why we check for 
                  a second jump. We can similarly assume we only have 2 jumps, as that's what the IR (as of writing)
                  intends.
              */
              if (targetCond != TrueLiteral) {
                val secondGoto: GoTo = jmps.tail.head.asInstanceOf[GoTo]
                targetBlock = secondGoto.target
                // IR doesn't store negation of condition, so we must do it manually
                targetCond = negateConditional(targetCond)

                // Jump to target block
                if (visitedBlocks.contains(targetBlock)) {
                  val targetBlockEntry: CfgCommandNode = visitedBlocks(targetBlock)
                  cfg.addEdge(precNode, targetBlockEntry, targetCond)
                } else {
                  visitBlock(targetBlock, precNode, targetCond)
                }
              }


            case dCall: DirectCall =>
              val targetProc: Procedure = dCall.target

              // Branch to this call
              cfg.addEdge(precNode, jmpNode, cond)

              // Record call association
              procToCalls(proc) += jmpNode
              procToCallers(targetProc) += jmpNode
              callToNodes(funcEntryNode) += jmpNode

              // Jump to return location
              dCall.returnTarget match {
                case Some(retBlock) =>
                  if (visitedBlocks.contains(retBlock)) {
                    val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                    cfg.addEdge(jmpNode, retBlockEntry)
                  } else {
                    visitBlock(retBlock, jmpNode, TrueLiteral)
                  }
                case None => 
                  println("[?] Direct - do we have to handle the \"no return\" case?")
                  println("  [!] Currently we just resolve this to be exit of function. This may not be the case in future.")
                  cfg.addEdge(jmpNode, funcExitNode)
              }

            case iCall: IndirectCall =>
              println("Encounted indirect call")
              println(iCall)

              // Branch to this call
              cfg.addEdge(precNode, jmpNode, cond)

              // Record call association
              procToCalls(proc) += jmpNode
              callToNodes(funcEntryNode) += jmpNode

              // R30 is the link register - this stores the address to return to 
              // TODO: introduce a "returnToCaller" node? To decide in meeting.
              if (iCall.target.isRegister("R30")) {
                cfg.addEdge(jmpNode, funcExitNode)
              }

              // Jump to return location
              iCall.returnTarget match {
                case Some(retBlock) =>
                  if (visitedBlocks.contains(retBlock)) {
                    val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                    cfg.addEdge(jmpNode, retBlockEntry)
                  } else {
                    visitBlock(retBlock, jmpNode, TrueLiteral)
                  }
                case None => 
                  println("[?] Indirect - Do we have to handle the \"no return\" case?")
              }
            case _ => assert(false, s"unexpected jump encountered, jumps: ${jmps}")
          } // `jmps.head match`
        } // `visitJumps` function
      } // `visitBlocks` function
  } // `cfgForProcedure` function


  /**
    * This takes an expression used in a conditional (jump), and 
    * negates it in a hopefully nice way. Currently it is quite simple.
    *
    * @param expr the expression to negate
    * @return
    */
  private def negateConditional(expr: Expr): Expr = expr match {
      case binop: BinaryExpr =>
        binop.op match {
          case BVNEQ =>
            BinaryExpr(
              BVEQ, binop.arg1, binop.arg2
            )
          case BVEQ => 
            BinaryExpr(
              BVNEQ, binop.arg1, binop.arg2
            )
          case _ =>
            UnaryExpr(
              BoolNOT, binop
            )
        }
      case _ =>
        UnaryExpr(
          BoolNOT, expr
        )
    }

  

  /** Recursively add 
    * 
    * @param procNodes
    * @param inlineAmount
    */
  private def inlineProcedureCalls(procNodes: Set[CfgCommandNode], inlineAmount: Int): Unit = {
    assert(inlineAmount >= 0)
    println(s"[!] Inlining with ${procNodes.size} at ${inlineAmount}")

    if (inlineAmount == 0 || procNodes.isEmpty) {
      return;
    }
    
    // Set of procedure calls to be discovered by inlining the ones in `procNodes`
    val nextProcNodes: mutable.Set[CfgCommandNode] = mutable.Set[CfgCommandNode]()

    procNodes.foreach{ procNode =>
      procNode.data match { 
        case targetCall: DirectCall => 
          // require(procNode.data.isInstanceOf[DirectCall], s"Trying to inline a non-function call instruction: ${procNode}")

          // Retrieve information about the call to the target procedure
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
        case _ =>
      }
    }

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

    callToNodes += (newEntry -> mutable.Set[CfgCommandNode]())

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
              procToCalls(proc) += newNode.asInstanceOf[CfgCommandNode]        // This procedure (general) is calling another procedure
              callToNodes(newEntry) += newNode.asInstanceOf[CfgCommandNode]   // This procedure (specfic) is calling another procedure
              procToCallers(d.target) += newNode.asInstanceOf[CfgCommandNode]  // Target of this call has a new caller
            case i: IndirectCall =>
              procToCalls(proc) += newNode.asInstanceOf[CfgCommandNode]
              callToNodes(newEntry) += newNode.asInstanceOf[CfgCommandNode]
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
