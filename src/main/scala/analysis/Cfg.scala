package analysis

import scala.collection.mutable
import ir._
import cfg_visualiser.{DotArrow, DotRegularArrow, DotInterArrow, DotIntraArrow, DotGraph, DotNode}

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
  val id: NodeId = CfgNode.nextId()
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
  /** Copy this node, but give unique ID */
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
  /** Copy this node, but give unique ID */
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
  /** Copy this node, but give unique ID */

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
    println("Generating CFG...")

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
    println("Proc to calls:")
    println(procToCalls.get)
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
    callToNodes += (funcEntryNode -> mutable.Set[CfgCommandNode]())

    // Procedure has no content (in our case, this probably means it's an ignored procedure)
    if (proc.blocks.size == 0) {
      cfg.addEdge(funcEntryNode, funcExitNode)
      return;
    }

    val visitedBlocks: mutable.HashMap[Block, CfgCommandNode] = mutable.HashMap[Block, CfgCommandNode]()    
    
    // Visit each node
    // println("=== PROCEDURE ===")
    // println(s"  ${proc}")
    // println(s"blocks:")
    // proc.blocks.foreach(
    //   block =>
    //     println(s"----- BLOCK -----")
    //     println(s"  label: ${block.label}")
    //     println(s" STATEMENTS")
    //     println(block.statements)
    //     println(s" JUMPS")
    //     println(block.jumps)
    // )
    visitBlock(proc.blocks.head, funcEntryNode, TrueLiteral)

    /**
      * TODO
      * 
      * @param block
      * @param prevBlockEnd
      * @param cond
      */
      def visitBlock(block: Block, prevBlockEnd: CfgNode, cond: Expr): Unit = {
        
        block.statements.size match {
          case i if i > 0 =>
            // Block contains some statements
            val endStmt: CfgCommandNode = visitStmts(block.statements.toList, prevBlockEnd, cond)
            visitJumps(block.jumps.toList, endStmt, TrueLiteral, solitary = false)
          case _ =>
            // Only jumps in this block
            visitJumps(block.jumps.toList, prevBlockEnd, cond, solitary = true)
        }


          /**
          * TODO
          *
          * @param stmts
          * @return
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

        /**
          * TODO
          *
          * @param jmps
          * @param prevNode
          * @param cond
          * @param solitary
          * @return
          */
        def visitJumps(jmps: List[Jump], prevNode: CfgNode, cond: Expr, solitary: Boolean): Unit = {

          val jmpNode: CfgCommandNode = CfgCommandNode(data = jmps.head)
          var precNode: CfgNode = prevNode

          if (solitary) {
            // If this block contains only jumps, then the `entrance` to this block is a jump also. 
            //  If this is a direct call, then we simply use that call node as the entrance.
            //  If this is a GoTo, then we introduce a fake "Ghost node" to act as the entrance to
            //    the block. This could equivalently be seen as a `GoTo` jump node, but we leave it 
            //    general for any potential future use cases.
            jmps.head match {
              case jmp: GoTo =>
                // `GoTo`s are just edges, so introduce a fake `start of block` that can be jmp'd to
                println("Double solitary GoTo")
                println(s"  proc: ${proc.name}")
                println(s"  jmps: ${jmps}")
                val ghostNode = CfgCommandNode(data = NOP())
                cfg.addEdge(prevNode, ghostNode, cond)
                precNode = ghostNode
                visitedBlocks += (block -> ghostNode)
              case _ =>
                // (In)direct call - use this as entrance to block
                visitedBlocks += (block -> jmpNode)
            }
          }
          
          // Possible `jmp` combinations:
          //  1. DirectCall
          //  2. IndirectCall
          //  3. GoTo
          //  4. GoTo + GoTo
          jmps.head match {
            case goto: GoTo =>
              
              // Process first jump
              var targetBlock: Block = goto.target
              var targetCond: Expr = goto.condition match {
                case Some(c) => c
                case None => TrueLiteral
              }

              if (visitedBlocks.contains(targetBlock)) {
                val targetBlockEntry: CfgCommandNode = visitedBlocks(targetBlock)
                cfg.addEdge(precNode, targetBlockEntry, targetCond)
              } else {
                visitBlock(targetBlock, precNode, targetCond)
              }

              // If the `GoTo`s condition is not `True`, then there is another conditional jump
              //  Equivalently, there are 2 jumps stored. We could simply check if there are 2
              //  but this is more logical to read. If we have more than 3, something has 
              //  gone wrong, as the IR in its current state shouldn't store more than 2 jumps
              //  in a single basic block.
              if (targetCond != TrueLiteral) {
                val secondGoto: GoTo = jmps.tail.head.asInstanceOf[GoTo]
                targetBlock = secondGoto.target
                // IR doesn't store negation of condition, so we must do it manually
                println(s"  Before negation: ${targetCond}")
                targetCond = negateConditional(targetCond) //UnaryExpr(BoolNOT, targetCond)
                println(s"  After negation : ${targetCond}")

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

              // Branch to this call
              cfg.addEdge(precNode, jmpNode, cond)

              // Record call association
              procToCalls(proc) += jmpNode
              callToNodes(funcEntryNode) += jmpNode

              // R30 is the link register - this stores the address to return to 
              //  is this accurate in this sense? Shouldn't we be getting the stored value?
              if (iCall.target.isRegister("R30")) {
                cfg.addEdge(jmpNode, funcExitNode)
              }

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
          } // `jmps.head match`
        } // `visitJumps` function
      }// `visitBlocks` function
  }


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
          // val targetCall: DirectCall = procNode.data.asInstanceOf[DirectCall]
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
              callToNodes(entryNode) += newNode.asInstanceOf[CfgCommandNode]   // This procedure (specfic) is calling another procedure
              procToCallers(d.target) += newNode.asInstanceOf[CfgCommandNode]  // Target of this call has a new caller
            case i: IndirectCall =>
              procToCalls(proc) += newNode.asInstanceOf[CfgCommandNode]
              callToNodes(entryNode) += newNode.asInstanceOf[CfgCommandNode]
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
