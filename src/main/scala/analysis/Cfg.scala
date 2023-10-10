package analysis

import scala.collection.mutable
import ir.*
import cfg_visualiser.{DotArrow, DotGraph, DotInlineArrow, DotInterArrow, DotIntraArrow, DotNode, DotRegularArrow}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks.break
import analysis.Fresh.next
import util.Logger

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

/** Edge between two command nodes in the CFG. Used for `GoTo` branches as well (with a condition added for the branch).
  */
case class RegularEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"RegularEdge(From: $from, To: $to)"
}

/** Edge which skips over a procedure call. Used for "jumping over" function calls, i.e. in an intra-procedural CFG
  * walk.
  */
case class IntraprocEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"IntraprocEdge(From: $from, To: $to)"
}

/** Procedure call between node and procedure.
  */
class InterprocEdge(from: CfgNode, to: CfgNode, cond: Expr) extends CfgEdge(from, to, cond) {
  override def toString: String = s"InterprocEdge(From: $from, To: $to)"
}

/** Connects an intraprocedural cfg with a copy of a another procedure's intraprocedural cfg.
  */
case class InlineEdge(from: CfgNode, to: CfgNode, cond: Expr) extends InterprocEdge(from, to, cond) {
  override def toString: String = s"InlineEdge(From: $from, To: $to)"
}

/** Connects an intraprocedural cfg with another procedure's intraprocedural cfg, by a direct link. This only applies to
  * leaf-node calls of a cfg, i.e., if the call is not being inlined.
  */
case class InterprocCallEdge(from: CfgNode, to: CfgNode, cond: Expr) extends InterprocEdge(from, to, cond) {
  override def toString: String = s"InterprocCallEdge(From: $from, To: $to)"
}

/** Node in the control-flow graph. Each node has a (simple incremental) unique identifier used to distinguish it from
  * other nodes in the cfg - this is mainly used for copying procedure cfgs when inlining them.
  *
  * Each node will store four separate sets: ingoing/outgoing of both inter-/intra-procedural CFG edges. Both intra and
  * inter will also store regular edges in the cfg. This is duplication of storage, however is done so knowingly.
  *
  * By separating sets into inter/intra we are able to return these directly without doing any processing. Alternative
  * means of achieving this same behaviour would involve some form of set operations, or a filter operation, both of
  * which can be expensive, especially as the successors/predecessors will be accessed frequently by analyses.
  * Additionally, inspecting the space complexity of these sets, we note that their sizes should be relatively limited:
  *   a. #(outgoing edges) <= 2 b. #(incoming edges) ~ N Thus in `succIntra` + `succInter` we have at most 4 elements.
  *      For `predIntra` + `predInter` we have a maximum of 2N, resulting from the case that this node is a block entry
  *      that is jumped to by N other nodes. It should be noted that in the majority of cases (statements which are
  *      neither the start of blocks nor jumps), both sets will be of size 1, making the storage complexity negligible.
  *      This is a point which can be optimised upon however.
  *
  * A node can have three main types of connected edges:
  *   a. A regular edge A regular edge connects two statements which only relate to the current procedure's context. b.
  *      Intra-procedural edge Intra-procedural edges connect a call node with the subsequent cfg node in a way that
  *      bypasses dealing with the semantics of the callee - it is up to analyses to determine how to treat such a call.
  *      c. Inter-procedural edge These are split into two cases:
  *      i. Inline edge These connect call nodes with an inlined copy of the target's procedure body. The exit of the
  *         target procedure's clone is also linked back to the caller via an inline edge. For an inline limit of `n`,
  *         these are the inter-procedural edges for depth 0 <= i < n ii. Call edge These connect leaf call nodes (calls
  *         which are not inlined) to the start of the independent cfg of the call's target. For an inline limit of `n`,
  *         these are the inter-procedural edges at depth i == n.
  */
trait CfgNode:

  /** Edges to this node from regular statements or ignored procedure calls.
    *
    * `predIntra` <: Set[RegularEdge | IntraprocEdge]
    */
  val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Edges to this node from procedure calls. Likely empty unless this node is a [[CfgFunctionEntryNode]]
    *
    * `predInter` <: Set[RegularEdge | InterprocEdge]
    */
  val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve predecessor nodes to this node.
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg.
    * @return
    *   Set of predecessor nodes
    */
  def pred(intra: Boolean): mutable.Set[CfgNode] = {
    intra match
      case true  => predIntra.map(edge => edge.getFrom)
      case false => predInter.map(edge => edge.getFrom)
  }

  /** Retrieve predecessor edges to this node.
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg
    * @return
    *   Set of predecessor edges
    */
  def predEdges(intra: Boolean): mutable.Set[CfgEdge] = if (intra) predIntra else predInter

  /** Retrieve predecessor nodes and associated conditions, if they exist
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg
    * @return
    *   (Node, EdgeCondition)
    */
  def predConds(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match
      case true  => predIntra.map(edge => (edge.getFrom, edge.getCond))
      case false => predInter.map(edge => (edge.getFrom, edge.getCond))
  }

  /** Edges to successor nodes, either regular or ignored procedure calls
    *
    * `succIntra` <: Set[RegularEdge | IntraprocEdge]
    */
  val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Edges to successor procedure calls. Used when walking inter-proc cfg.
    *
    * `succInter` <: Set[RegularEdge | InterprocEdge]
    */
  val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()

  /** Retrieve successor nodes to this node.
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg.
    * @return
    *   Set of successor nodes
    */
  def succ(intra: Boolean): mutable.Set[CfgNode] = {
    intra match
      case true  => succIntra.map(edge => edge.getTo)
      case false => succInter.map(edge => edge.getTo)
  }

  /** Retrieve successor edges from this node.
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg.
    * @return
    *   Set of successor edges
    */
  def succEdges(intra: Boolean): mutable.Set[CfgEdge] = if (intra) succIntra else succInter

  /** Retrieve succesor nodes and associated conditions, if they exist.
    *
    * @param intra
    *   if true, only walk the intraprocedural cfg.
    * @return
    *   (Node, EdgeCondition)
    */
  def succConds(intra: Boolean): mutable.Set[(CfgNode, Expr)] = {
    intra match
      case true  => succIntra.map(edge => (edge.getTo, edge.getCond))
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
  // Block this node originates from
  def block: Block

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
  override def block = data.blocks.head
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
  override def block = data.blocks.head
  override def toString: String = s"[FunctionExit] $data"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgFunctionExitNode = CfgFunctionExitNode(data = this.data)

/** CFG node immediately proceeding a indirect call. This signifies that the call is a return from the current context
  * (i.e., likely an indirect call to R30). Its purpose is to provide a way for analyses to identify whether they should
  * return to the previous function context, if it is a context dependent analyses, and otherwise can be ignored.
  *
  * In the cfg we treat this as a stepping stone to `CfgFunctionExitNode`, as a way to emphasise that the current
  * procedure has no functionality past this point.
  */
case class CfgProcedureReturnNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
) extends CfgNode:
  override def toString: String = s"[ProcedureReturn]"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgProcedureReturnNode = CfgProcedureReturnNode()

/** CFG node immediately proceeding a direct/indirect call, if that call has no specified return block. There are a few
  * reasons this can occur:
  *   a. It is not expected that the program will return from the callee b. The lifter has erroneously labelled a call
  *      as a jump / mislabelled a function name, which was then not associated with a block in some later stage. This
  *      happened with a call to `__gmon_start_`, which was optimised from a call to a jump which was incorrectly
  *      interpreted by the lifter. c. The indirect call is some other form of return-to-caller (which does not use
  *      R30). These are currently unhandled, and could potentially be integrated - e.g., sometimes R17 and R16 can be
  *      used in a similar way to R30.
  *      https://blog.tomzhao.me/wp-content/uploads/2021/08/Procedure_Call_Standard_in_Armv8_54f88cbfe905409aaff956ac2d1ad059.pdf
  *
  * In the cfg this is similarly used as a stepping stone to `CfgFunctionExitNode`.
  */
case class CfgCallNoReturnNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
) extends CfgNode:
  override def toString: String = s"[Call NoReturn]"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgCallNoReturnNode = CfgCallNoReturnNode()

/** CFG node immediately proceeding a direct/indirect call, if that call has a return location specified. This serves as
  * a point for analysis to stop and process update their states after handling a procedure call before continuing
  * within the current contex. For example, a context sensitive analysis will return to this node after reaching a
  * procedure return within the caller. It will then restore and update its context from before the call, before
  * continuing on within the original procedure.
  *
  * Effectively, this just splits a procedure call from a single `Jmp` node into two - the call, and the return point.
  * Incoming edges to the `Jmp` are then incoming edges to the respective `CfgJumpnode`, and outgoing edges from the
  * `Jmp` are then outgoing edges of the `CfgCallReturnNode`. It is functionally in the same spirit as
  * `CfgCallNoReturnNode`, though handles the case that this procedure still has functionality to be explored.
  */
case class CfgCallReturnNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
) extends CfgNode:
  override def toString: String = s"[Call Return]"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgCallReturnNode = CfgCallReturnNode()

/** Control-flow graph node for a command (statement or jump).
  */
trait CfgCommandNode extends CfgNodeWithData[Command] {
  override def copyNode(): CfgCommandNode
}

/** CFG's representation of a single statement.
  */
case class CfgStatementNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    data: Statement,
    block: Block
) extends CfgCommandNode:
  override def toString: String = s"[Stmt] $data"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgStatementNode = CfgStatementNode(data = this.data, block = this.block)

/** CFG's representation of a jump. This is used as a general jump node, for both indirect and direct calls.
  */
case class CfgJumpNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    data: Jump,
    block: Block
) extends CfgCommandNode:
  override def toString: String = s"[Jmp] $data"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgJumpNode = CfgJumpNode(data = this.data, block = this.block)

/** A general purpose node which in terms of the IR has no functionality, but can have purpose in the CFG. As example,
  * this is used as a "block" start node for the case that a block contains no statements, but has a `GoTo` as its jump.
  * In this case we introduce a ghost node as the start of the block for the case that some part of the program jumps
  * back to this conditional jump (e.g. in the case of loops).
  */
case class CfgGhostNode(
    override val id: Int = CfgNode.nextId(),
    override val predIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val predInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succIntra: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    override val succInter: mutable.Set[CfgEdge] = mutable.Set[CfgEdge](),
    block: Block
) extends CfgCommandNode:
  override val data: Statement = NOP
  override def toString: String = s"[NOP]"

  /** Copy this node, but give unique ID and reset edges */
  override def copyNode(): CfgGhostNode = CfgGhostNode(block = this.block)

/** A control-flow graph. Nodes provide the ability to walk it as both an intra and inter procedural CFG.
  */
class ProgramCfg:

  var edges: mutable.Set[CfgEdge] = mutable.Set[CfgEdge]()
  var nodes: mutable.Set[CfgNode] = mutable.Set[CfgNode]()

  /** Inline edges are for connecting an intraprocedural cfg with a copy of another procedure's intraprocedural cfg
    * which is placed inside this one. They are considered interprocedural edges, and will not be followed if the caller
    * requests an intraprocedural cfg.
    *
    * @return
    *   The new edge
    */
  def addInlineEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): CfgEdge = {
    val newEdge: InlineEdge = InlineEdge(from, to, cond)

    from.succInter += newEdge
    to.predInter += newEdge

    newEdge
  }

  /** Interprocedural call edges connect an intraprocedural cfg with another procedure's intraprocedural cfg that it is
    * calling.
    *
    * @return
    *   The new edge
    */
  def addInterprocCallEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): CfgEdge = {
    val newEdge: InterprocCallEdge = InterprocCallEdge(from, to, cond)

    from.succInter += newEdge
    to.predInter += newEdge

    newEdge
  }

  /** Intraprocedural edges are for connecting call nodes to the call's return node, without following the call itself
    * (stepping over the call).
    *
    * @return
    *   The new edge
    */
  def addIntraprocEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): CfgEdge = {
    val newEdge: IntraprocEdge = IntraprocEdge(from, to, cond)

    from.succIntra += newEdge
    to.predIntra += newEdge

    newEdge
  }

  /** Regular edges are normal control flow - used in both inter-/intra-procedural cfgs.
    *
    * @return
    *   The new edge
    */
  def addRegularEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): CfgEdge = {
    val newEdge: RegularEdge = RegularEdge(from, to, cond)

    from.succInter += newEdge
    from.succIntra += newEdge
    to.predInter += newEdge
    to.predIntra += newEdge

    newEdge
  }

  /** Add an outgoing edge from the current node, taking into account any conditionals on this jump. Note that we have
    * some duplication of storage here - this is a performance consideration. We don't expect too many edges for any
    * given node, and so the increased storage is relatively minimal. This saves having to filter / union sets when
    * trying to retrieve only an intra/inter cfg, hopefully improving computation time.
    *
    * NOTE: this function attempts to "smartly" identify how to connect two edges. Perhaps as the CFG changes however
    * different requirements will be made of nodes, and so the conditions on edges below may change. In that case,
    * either update the below, or explicitly specify the edge to be added between two nodes.
    *
    * @param from
    *   The originating node
    * @param to
    *   The destination node
    * @param cond
    *   Condition on this edge, otherwise assume it will always be followed
    */
  def addEdge(from: CfgNode, to: CfgNode, cond: Expr = TrueLiteral): Unit = {

    val newEdge: CfgEdge = (from, to) match {
      // Ignored procedure (e.g. library calls such as @printf)
      case (from: CfgFunctionEntryNode, to: CfgFunctionExitNode) => addRegularEdge(from, to, TrueLiteral)
      // Calling procedure (follow as inline)
      //  This to be used if inlining skips the call node and links the most recent statement to the first statement of the target
      case (from: CfgCommandNode, to: CfgFunctionEntryNode) => addInlineEdge(from, to, cond)
      // Returning from procedure (follow as inline - see above)
      case (from: CfgFunctionExitNode, to: CfgNode) => addInlineEdge(from, to, cond)
      // First instruction of procedure
      case (from: CfgFunctionEntryNode, to: CfgNode) => addRegularEdge(from, to, cond)
      // Function call which returns to the previous context
      case (from: CfgJumpNode, to: CfgProcedureReturnNode) => addRegularEdge(from, to, cond)
      // Edge to intermediary return node (no semantic meaning, a cfg convenience edge)
      case (from: CfgJumpNode, to: (CfgCallReturnNode | CfgCallNoReturnNode)) => addIntraprocEdge(from, to, cond)
      // Pre-exit nodes
      case (from: (CfgProcedureReturnNode | CfgCallNoReturnNode | CfgCallReturnNode), to: CfgFunctionExitNode) =>
        addRegularEdge(from, to, cond)
      // Regular continuation of execution
      case (from: CfgCallReturnNode, to: CfgCommandNode) => addRegularEdge(from, to, cond)
      // Regular flow of instructions
      case (from: CfgCommandNode, to: (CfgCommandNode | CfgFunctionExitNode)) => addRegularEdge(from, to, cond)
      case _ => throw new Exception(s"[!] Unexpected edge combination when adding cfg edge between ${from} -> ${to}.")
    }

    edges += newEdge
    nodes += from
    nodes += to
  }

  /** Add a node to the CFG.
    */
  def addNode(node: CfgNode): Unit =
    nodes += node

  /** Returns a Graphviz dot representation of the CFG. Each node is labeled using the given function labeler.
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

      val allEdgesOut: Set[CfgEdge] = n.succEdges(true).toSet.union(n.succEdges(false))

      allEdgesOut.foreach { edge =>
        val from: CfgNode = edge.getFrom
        val to: CfgNode = edge.getTo
        val cond: Expr = edge.getCond

        edge match {
          case regular: RegularEdge =>
            cond match {
              case TrueLiteral =>
                dotArrows += DotRegularArrow(dotNodes(n), dotNodes(to))
              case _ =>
                dotArrows += DotRegularArrow(dotNodes(n), dotNodes(to), cond.toString)
            }
          case intra: IntraprocEdge =>
            cond match {
              case TrueLiteral =>
                dotArrows += DotIntraArrow(dotNodes(n), dotNodes(to))
              case _ =>
                dotArrows += DotIntraArrow(dotNodes(n), dotNodes(to), cond.toString)
            }
          case inline: InlineEdge =>
            cond match {
              case TrueLiteral =>
                dotArrows += DotInlineArrow(dotNodes(n), dotNodes(to))
              case _ =>
                dotArrows += DotInlineArrow(dotNodes(n), dotNodes(to), cond.toString)
            }
          /* Displaying the below in the CFG is mostly for debugging purposes. With it included the CFG becomes a little unreadable, but
                will emphasise that the leaf-call nodes are linked to the start of the procedures they're calling (as green inter-procedural edges).
                To verify this is still happening, simply uncomment the below and it will add these edges.
           */
          // case interCall: InterprocCallEdge =>
          //   cond match {
          //     case TrueLiteral =>
          //       dotArrows += DotInterArrow(dotNodes(n), dotNodes(to))
          //     case _ =>
          //       dotArrows += DotInterArrow(dotNodes(n), dotNodes(to), cond.toString())
          //   }
          case _ =>
        }
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

/** Control-flow graph for an entire program. We have a more granular approach, storing commands as nodes instead of
  * basic blocks.
  */
class ProgramCfgFactory:
  val cfg: ProgramCfg = ProgramCfg()

  // Mapping from procedures to the start of their individual (intra) cfgs
  val procToCfg: mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)] =
    mutable.HashMap[Procedure, (CfgFunctionEntryNode, CfgFunctionExitNode)]()
  // Mapping from procedures to procedure call nodes (all the calls made within this procedure, including inlined functions)
  val procToCalls: mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]] =
    mutable.HashMap[Procedure, mutable.Set[CfgCommandNode]]()
  // Mapping from procedure entry instances to procedure call nodes within that procedure's instance (`CfgCommandNode.data <: DirectCall`)
  //    Updated on first creation of a new procedure (e.g. in initial creation, or in cloning of a procedure's cfg)
  val callToNodes: mutable.Map[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]] =
    mutable.Map[CfgFunctionEntryNode, mutable.Set[CfgCommandNode]]()
  // Mapping from procedures to nodes in any node in the cfg which has a call to that procedure
  val procToCallers: mutable.Map[Procedure, mutable.Set[CfgCommandNode]] =
    mutable.Map[Procedure, mutable.Set[CfgCommandNode]]()

  /** Generate the cfg for each function of the program. NOTE: is this functionally different to a constructor? Do we
    * ever expect to generate a CFG from any other data structure? If not then the `class` could probably be absorbed
    * into this object.
    *
    * @param program
    *   Basil IR of the program
    * @param inlineLimit
    *   How many levels deep to inline function calls. By default, don't inline - this is equivalent to an
    *   intra-procedural CFG.
    */
  def fromIR(program: Program, unify: Boolean = false, inlineLimit: Int = 0): ProgramCfg = {
    CfgNode.id = 0
    require(inlineLimit >= 0, "Can't inline procedures to negative depth...")
    Logger.info("[+] Generating CFG...")

    // Have to initialise the map entries manually. Scala maps have a `.withDefaulValue`, but this is buggy and doesn't
    //  behave as you would expect: https://github.com/scala/bug/issues/8099 - thus the manual approach.
    // We don't initialise `procToCfg` here, because it will never be accessed before `cfgForProcedure`,
    //  and because it relies on the entry/exit nodes be initialised. It is initialised in `cfgForProcedure`.
    program.procedures.foreach(proc =>
      procToCalls += (proc -> mutable.Set[CfgCommandNode]())
      procToCallers += (proc -> mutable.Set[CfgCommandNode]())
    )

    // Create CFG for individual procedures
    program.procedures.foreach(proc => cfgForProcedure(proc))

    // Inline functions up to `inlineLimit` level
    // EXTENSION; one way to improve this would be to specify inline depths for specific functions / situations.
    //    i.e. we may not want to inline self-recursive functions too much.
    // Of note is whether we want this at all or note. If not, then we can simply remove the below and pass `procCallNodes` to
    //    `addInterprocEdges`.
    val procCallNodes: Set[CfgCommandNode] = procToCalls.values.flatten.toSet
    val leafCallNodes: Set[CfgCommandNode] =
      if !unify then inlineProcedureCalls(procCallNodes, inlineLimit) else unifyProcedureCalls(procCallNodes)

    // Add inter-proc edges to leaf call nodes
    if (leafCallNodes.nonEmpty) {
      addInterprocEdges(leafCallNodes)
    }

    cfg
  }

  /** Create an intraprocedural CFG for the given IR procedure. The start of the CFG for a procedure is identified by
    * its `CfgFunctionEntryNode`, and its closure is identified by the `CfgFunctionExitNode`.
    *
    * @param proc
    *   Procedure for which to generate the intraprocedural cfg
    */
  private def cfgForProcedure(proc: Procedure): Unit = {
    val funcEntryNode: CfgFunctionEntryNode = CfgFunctionEntryNode(data = proc)
    val funcExitNode: CfgFunctionExitNode = CfgFunctionExitNode(data = proc)
    cfg.addNode(funcEntryNode)
    cfg.addNode(funcExitNode)

    procToCfg += (proc -> (funcEntryNode, funcExitNode))
    callToNodes += (funcEntryNode -> mutable.Set[CfgCommandNode]())

    // Procedure has no content (in our case this probably means it's an ignored procedure, e.g., an external function such as @printf)
    if (proc.blocks.isEmpty) {
      cfg.addEdge(funcEntryNode, funcExitNode)
      return;
    }

    // Track blocks we've already processed so we don't double up
    val visitedBlocks: mutable.HashMap[Block, CfgCommandNode] = mutable.HashMap[Block, CfgCommandNode]()

    // Recurse through blocks
    visitBlock(proc.blocks.head, funcEntryNode, TrueLiteral)

    /** Add a block to the CFG. A block in this case is a basic block, so it contains a list of consecutive statements
      * followed by a jump at the end to another block. We process statements in this block (if they exist), and then
      * follow the jump to recurse through all other blocks.
      *
      * This recursive approach is effectively a "reaches" approach, and will miss cases that we encounter a jump we
      * can't resolve, or cases where the lifter has not identified a section of code. In each case:
      *   a. The only jumps we can't resolve are indirect calls. It's the intent of the tool to attempt to resolve these
      *      through analysis however. The CFG can then be updated as these are resolved to incorporate their jumps. In
      *      construction we do a simple check for register R30 to identify if an indirect call is a return, but
      *      otherwise consider it as unresolved. b. If the lifter has failed to identify a region of code, then the
      *      problem exists at the lifter level. In that case we need a way to coerce the lifter into identifying it, or
      *      to use a new lifter.
      *
      * These visitations will also only produce the intra-procedural CFG - the burden of "creating" the
      * inter-procedural CFG is left to processes later during CFG construction. The benefit of doing this is that we
      * can completely resolve a procedure's CFG without jumping to other procedures mid-way through processing, which
      * assures we don't have any issues with referencing nodes before they exist. Essentially this is a depth-first
      * approach to CFG construction, as opposed to a breadth-first.
      *
      * @param block
      *   The block being added to the CFG.
      * @param prevBlockEnd
      *   Preceding block's end node (jump)
      * @param cond
      *   Condition on the jump from `prevNode` to the first statement of this block
      */
    def visitBlock(block: Block, prevBlockEnd: CfgNode, cond: Expr): Unit = {

      // TODO: this can probably be abstracted to single calls to `visitStmts` and `visitJumps` by placing
      //    the burden of checking statement size on the individual functions (effectively what
      //    the `solitary` flag currently checks). Perhaps this approach is more readable though?
      block.statements.size match {
        case i if i > 0 =>
          // Block contains some statements
          val endStmt: CfgCommandNode = visitStmts(block.statements, prevBlockEnd, cond)
          visitJumps(block.jumps, endStmt, TrueLiteral, solitary = false)
        case _ =>
          // Only jumps in this block
          visitJumps(block.jumps, prevBlockEnd, cond, solitary = true)
      }

      /** If a block has statements, we add them to the CFG. Blocks in this case are basic blocks, so we know
        * consecutive statements will be linked by an unconditional, regular edge.
        *
        * @param stmts
        *   Statements in this block
        * @param prevNode
        *   Preceding block's end node (jump)
        * @param cond
        *   Condition on the jump from `prevNode` to the first statement of this block
        * @return
        *   The last statement's CFG node
        */
      def visitStmts(stmts: ArrayBuffer[Statement], prevNode: CfgNode, cond: Expr): CfgCommandNode = {

        val firstNode: CfgStatementNode = CfgStatementNode(data = stmts.head, block = block)
        cfg.addEdge(prevNode, firstNode, cond)
        visitedBlocks += (block -> firstNode) // This is guaranteed to be entrance to block if we are here

        if (stmts.size == 1) {
          // Statements start and end here for this block
          return firstNode
        }

        var prevStmtNode: CfgStatementNode = firstNode

        // `tail` takes everything after the first element of the iterable
        stmts.tail.foreach(stmt =>
          val stmtNode: CfgStatementNode = CfgStatementNode(data = stmt, block = block)
          cfg.addEdge(prevStmtNode, stmtNode)
          prevStmtNode = stmtNode
        )

        prevStmtNode
      }

      /** All blocks end with jump(s), whereas some also start with a jump (in the case of no statements). Add these to
        * the CFG and visit their target blocks for processing.
        *
        * @param jmps
        *   Jumps in the current block being processed
        * @param prevNode
        *   Either the previous statement in the block, or the previous block's end node (in the case that this block
        *   contains no statements)
        * @param cond
        *   Jump from `prevNode` to this. `TrueLiteral` if `prevNode` is a statement, and any `Expr` if `prevNode` is a
        *   jump.
        * @param solitary
        *   `True` if this block contains no statements, `False` otherwise
        */
      def visitJumps(jmps: ArrayBuffer[Jump], prevNode: CfgNode, cond: Expr, solitary: Boolean): Unit = {

        val jmpNode: CfgJumpNode = CfgJumpNode(data = jmps.head, block = block)
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
              val ghostNode = CfgGhostNode(block = block)
              cfg.addEdge(prevNode, ghostNode, cond)
              precNode = ghostNode
              visitedBlocks += (block -> ghostNode)
            case _ =>
              // (In)direct call - use this as entrance to block
              visitedBlocks += (block -> jmpNode)
          }
        }

        // TODO this is not a robust approach

        jmps.head match {
          case goto: GoTo =>
            // Process first jump
            var targetBlock: Block = goto.target
            var targetCond: Expr = goto.condition match {
              case Some(c) => c
              case None    => TrueLiteral
            }

            // Jump to target block
            if (visitedBlocks.contains(targetBlock)) {
              val targetBlockEntry: CfgCommandNode = visitedBlocks(targetBlock)
              cfg.addEdge(precNode, targetBlockEntry, targetCond)
            } else {
              visitBlock(targetBlock, precNode, targetCond)
            }

            /* TODO it is not a safe assumption that there are a maximum of two jumps, or that a GoTo will follow a GoTo
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
            val calls = jmps.filter(_.isInstanceOf[DirectCall]).map(x => CfgJumpNode(data = x, block = block))

            calls.foreach(node => {
              cfg.addEdge(precNode, node, node.data.asInstanceOf[DirectCall].condition.getOrElse(TrueLiteral))

              procToCalls(proc) += node
              procToCallers(targetProc) += node
              callToNodes(funcEntryNode) += node
            })

            // Record call association

            // Jump to return location
            dCall.returnTarget match {
              case Some(retBlock) =>
                // Add intermediary return node (split call into call and return)
                val callRet = CfgCallReturnNode()

                calls.foreach(node => {
                  cfg.addEdge(node, callRet)
                })
                if (visitedBlocks.contains(retBlock)) {
                  val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                  cfg.addEdge(callRet, retBlockEntry)
                } else {
                  visitBlock(retBlock, callRet, TrueLiteral)
                }
              case None =>
                val noReturn = CfgCallNoReturnNode()
                calls.foreach(node => {
                  cfg.addEdge(node, noReturn)
                })
                cfg.addEdge(noReturn, funcExitNode)
            }
          case iCall: IndirectCall => {
            Logger.info(s"Indirect call found: $iCall in ${proc.name}")

            // Branch to this call
            cfg.addEdge(precNode, jmpNode, cond)

            // Record call association
            procToCalls(proc) += jmpNode
            callToNodes(funcEntryNode) += jmpNode

            // R30 is the link register - this stores the address to return to.
            //  For now just add a node expressing that we are to return to the previous context.
            if (iCall.target == Register("R30", BitVecType(64))) {
              val returnNode = CfgProcedureReturnNode()
              cfg.addEdge(jmpNode, returnNode)
              cfg.addEdge(returnNode, funcExitNode)
              return;
            }

            // Jump to return location
            iCall.returnTarget match {
              case Some(retBlock) => // Add intermediary return node (split call into call and return)
                val callRet = CfgCallReturnNode()
                cfg.addEdge(jmpNode, callRet)

                if (visitedBlocks.contains(retBlock)) {
                  val retBlockEntry: CfgCommandNode = visitedBlocks(retBlock)
                  cfg.addEdge(callRet, retBlockEntry)
                } else {
                  visitBlock(retBlock, callRet, TrueLiteral)
                }
              case None =>
                val noReturn = CfgCallNoReturnNode()
                cfg.addEdge(jmpNode, noReturn)
                cfg.addEdge(noReturn, funcExitNode)
            }
          }
          case _ => assert(false, s"unexpected jump encountered, jumps: ${jmps}")
        } // `jmps.head` match
      } // `visitJumps` function
    } // `visitBlocks` function
  } // `cfgForProcedure` function

  /** This takes an expression used in a conditional (jump) and tries to negate it in a (hopefully) nice way. Most
    * conditional jumps are just bitvector comparisons.
    *
    * @param expr
    *   The expression to negate
    * @return
    *   The negated expression
    */
  private def negateConditional(expr: Expr): Expr = expr match {
    case binop: BinaryExpr =>
      binop.op match {
        case BVNEQ =>
          BinaryExpr(
            BVEQ,
            binop.arg1,
            binop.arg2
          )
        case BVEQ =>
          BinaryExpr(
            BVNEQ,
            binop.arg1,
            binop.arg2
          )
        case _ =>
          // Worst case scenario we just take the logical not of everything
          UnaryExpr(
            BoolNOT,
            binop
          )
      }
    case unop: UnaryExpr =>
      unop.op match {
        case BVNOT | BoolNOT =>
          unop.arg
        case _ =>
          UnaryExpr(
            BoolNOT,
            unop
          )
      }
    case _ =>
      UnaryExpr(
        BoolNOT,
        expr
      )
  }

  /** Recursively inline procedures. This has a dumb/flat approach - we simply continue inlining each all direct calls
    * until we either run out of direct calls, or we are at our max inline depth.
    *
    * For each direct call to be inlined we make a copy of the target's intraprocedural cfg, which is then linked to the
    * calling procedure's cfg. We keep track of newly found direct calls that come from inlined functions, which is what
    * we pass to the next recursive call. At the end of recursion this set stores the leaf nodes of the cfg - this is
    * then used later to link interprocedural calls.
    *
    * @param procNodes
    *   The call nodes to inline
    * @param inlineAmount
    *   Maximum amount of inlining from this depth allowed
    * @return
    *   Tthe next leaf call nodes
    */
  private def inlineProcedureCalls(procNodes: Set[CfgCommandNode], inlineAmount: Int): Set[CfgCommandNode] = {
    assert(inlineAmount >= 0)
    Logger.info(s"[+] Inlining ${procNodes.size} leaf call nodes with ${inlineAmount} level(s) left")

    if (inlineAmount == 0 || procNodes.isEmpty) {
      return procNodes;
    }

    // Set of procedure calls to be discovered by inlining the ones in `procNodes`
    val nextProcNodes: mutable.Set[CfgCommandNode] = mutable.Set[CfgCommandNode]()

    procNodes.foreach { procNode =>
      procNode.data match {
        case targetCall: DirectCall =>
          // Retrieve information about the call to the target procedure
          val targetProc: Procedure = targetCall.target
          val targetCond: Expr = targetCall.condition match {
            case Some(c) => c
            case None    => TrueLiteral
          }

          val (procEntry, procExit) = cloneProcedureCFG(targetProc)

          // Add link between call node and the procedure's `Entry`.
          cfg.addInlineEdge(procNode, procEntry, targetCond)

          // Link the procedure's `Exit` to the return point. There should only be one.
          assert(
            procNode.succ(intra = true).size == 1,
            s"More than 1 return node... ${procNode} has ${procNode.succ(intra = true)}"
          )
          val returnNode = procNode.succ(intra = true).head
          cfg.addInlineEdge(procExit, returnNode)

          // Add new (un-inlined) function calls to be inlined
          nextProcNodes ++= callToNodes(procEntry)
        case _ =>
      }
    }

    inlineProcedureCalls(nextProcNodes.toSet, inlineAmount - 1)
  }

  private def unifyProcedureCalls(procNodes: Set[CfgCommandNode]): Set[CfgCommandNode] = {
    Logger.info(s"[+] Unifyig ${procNodes.size} leaf call nodest")

    if (procNodes.isEmpty) {
      return procNodes;
    }

    // Set of procedure calls to be discovered by unifying the ones in `procNodes`
    val nextProcNodes: mutable.Set[CfgCommandNode] = mutable.Set[CfgCommandNode]()

    procNodes.foreach { procNode =>
      procNode.data match {
        case targetCall: DirectCall => // Retrieve information about the call to the target procedure
          val targetProc: Procedure = targetCall.target
          val targetCond: Expr = targetCall.condition match {
            case Some(c) => c
            case None    => TrueLiteral
          }

          val (procEntry, procExit) = procToCfg(targetProc)

          // Add link between call node and the procedure's `Entry`.
          cfg.addInlineEdge(procNode, procEntry, targetCond)

          // Link the procedure's `Exit` to the return point. There should only be one.
          assert(
            procNode.succ(intra = true).size == 1,
            s"More than 1 return node... ${procNode} has ${procNode.succ(intra = true)}"
          )
          val returnNode = procNode.succ(intra = true).head
          cfg.addInlineEdge(procExit, returnNode)

          // Add new (un-inlined) function calls to be inlined
          nextProcNodes ++= callToNodes(procEntry)
        case _ =>
      }
    }

    unifyProcedureCalls(nextProcNodes.toSet)
  }

  /** Clones the intraproc-cfg of the given procedure, with unique CfgNode ids. Adds the new nodes to the cfg, and
    * returns the start/end nodes of the new procedure cfg.
    *
    * @param proc
    *   The procedure to clone (used to index the pre-computed cfgs)
    * @return
    *   (CfgFunctionEntryNode, CfgFunctionExitNode) of the cloned cfg
    */
  private def cloneProcedureCFG(proc: Procedure): (CfgFunctionEntryNode, CfgFunctionExitNode) = {

    val (entryNode: CfgFunctionEntryNode, exitNode: CfgFunctionExitNode) = procToCfg(proc)
    val (newEntry: CfgFunctionEntryNode, newExit: CfgFunctionExitNode) = (entryNode.copyNode(), exitNode.copyNode())

    callToNodes += (newEntry -> mutable.Set[CfgCommandNode]())

    // Entry is guaranteed to only have one successor (by our cfg design)
    var currNode: CfgNode = entryNode.succ(intra = true).head
    visitNode(currNode, newEntry, TrueLiteral)

    /** Walk this proc's cfg until we reach the exit node on each branch. We do this recursively, tracking the previous
      * node, to account for branches and loops.
      *
      * We can't represent the parameters as an edge as one node comes from the old cfg, and the other from the new cfg.
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
        return
      }

      node match {
        case n: CfgCommandNode =>
          val newNode: CfgCommandNode = n.copyNode()

          // Link this node with predecessor in the new cfg
          cfg.addEdge(prevNewNode, newNode, cond)

          n.data match {
            case d: DirectCall =>
              procToCalls(proc) += newNode
              callToNodes(newEntry) += newNode
              procToCallers(d.target) += newNode
            case i: IndirectCall =>
              procToCalls(proc) += newNode
              callToNodes(newEntry) += newNode
            case _ =>
          }

          // Get intra-cfg successors
          val outEdges: mutable.Set[CfgEdge] = node.succEdges(intra = true)
          outEdges.foreach(edge => visitNode(edge.getTo, newNode, edge.getCond))

        // For other node types, link with predecessor and continue traversal
        case _ =>
          val newNode = node.copyNode()
          cfg.addEdge(prevNewNode, newNode, cond)

          val outEdges: mutable.Set[CfgEdge] = node.succEdges(intra = true)
          outEdges.foreach(edge => visitNode(edge.getTo, newNode, edge.getCond))
      }
    }

    (newEntry, newExit)
  }

  /** After inlining has been done, we link all residual direct calls (leaf nodes) to the start of the intraprocedural
    * that are the target of the call. These links are a special kind of inter-procedural edge, where we don't store a
    * return edge.
    *
    * @param leaves
    *   The call nodes at edge of intraprocedural cfgs to be linked to their targets
    */
  private def addInterprocEdges(leaves: Set[CfgCommandNode]): Unit = {

    leaves.foreach { callNode =>
      callNode.data match {
        case targetCall: DirectCall =>
          val targetProc: Procedure = targetCall.target
          val targetCond: Expr = targetCall.condition match {
            case Some(c) => c
            case None    => TrueLiteral
          }

          // We don't care about returns, as this is context dependent. It is up to the caller
          //  (in our case, the analyses) to keep track of context so that it knows where to return to
          //  at the exit of the target procedure
          val (targetEntry: CfgFunctionEntryNode, _) = procToCfg(targetProc)

          cfg.addInterprocCallEdge(callNode, targetEntry, targetCond)
        case _ =>
      }
    }
  }
