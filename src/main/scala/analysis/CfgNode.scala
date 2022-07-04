package analysis

import scala.collection.mutable

import astnodes._

object CfgNode {

  var lastUid: Int = 0

  def uid: Int = {
    lastUid += 1
    lastUid
  }
}

/** Node in a control-flow graph.
  */
trait CfgNode {

  /** Predecessors of the node.
    */
  def pred: mutable.Set[CfgNode]

  /** Successors of the node.
    */
  def succ: mutable.Set[CfgNode]

  /** Unique node ID.
    */
  def id: Int

  /** The AST node contained by this node.
    */
  def data: Block | FunctionNode

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case o: CfgNode => o.id == this.id
      case _          => false
    }

  override def hashCode(): Int = id
}

/** Node in a CFG representing a block
  */
case class CfgBlockNode(
    override val id: Int = CfgNode.uid,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Block
) extends CfgNode

/** Node in a CFG representing the entry of a function.
  */
case class CfgFunEntryNode(
    override val id: Int = CfgNode.uid,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNode

/** Node in a CFG representing the exit of a function.
  */
case class CfgFunExitNode(
    override val id: Int = CfgNode.uid,
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: FunctionNode
) extends CfgNode

/* object FragmentCfg {

  /** Generates a CFG for each function in the given program.
 */
  def generateFromProgram(prog: Program, nodeBuilder: CfgNode => FragmentCfg)(implicit
      declData: DeclarationData
  ): Map[AFunDeclaration, FragmentCfg] =
    prog.funs.map { f =>
      f -> FragmentCfg.generateFromFunction(f, nodeBuilder)
    }.toMap

  /** Constructs an empty CFG.
 */
  private def seqUnit(): FragmentCfg =
    new FragmentCfg(Set(), Set())

  /** Converts a CFG node to a one-node CFG.
 */
  def nodeToGraph(node: CfgNode): FragmentCfg =
    new FragmentCfg(Set(node), Set(node))

  /** Generates a CFG from the body of a function.
 */
  def generateFromFunction(fun: FunctionNode, nodeBuilder: CfgNode => FragmentCfg): FragmentCfg = {

    def recGen(node: AstNode): FragmentCfg =
      node match {
        case fun: AFunDeclaration =>
          val blk = recGen(fun.stmts)
          val entry = nodeBuilder(CfgFunEntryNode(data = fun))
          val exit = nodeBuilder(CfgFunExitNode(data = fun))
          entry ~ blk ~ exit
        case _: AAssignStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case block: ABlock =>
          block.body.foldLeft(seqUnit()) { (g, stmt) =>
            g ~ recGen(stmt)
          }
        case iff: AIfStmt =>
          val ifGuard = nodeBuilder(CfgStmtNode(data = node))
          val trueBranch = recGen(iff.ifBranch)
          val falseBranch = iff.elseBranch.map {
            recGen(_)
          }
          val guardedTrue = ifGuard ~ trueBranch
          val guardedFalse = falseBranch.map(fb => ifGuard ~ fb)
          guardedFalse.fold(guardedTrue | ifGuard)(guardedTrue | _)
        case _: AOutputStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AReturnStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AVarStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case whl: AWhileStmt =>
          val whileG = nodeBuilder(CfgStmtNode(data = node))
          val bodyG = recGen(whl.innerBlock)
          val loopingBody = whileG ~ bodyG ~ whileG
          loopingBody | whileG
        case _: ADeviceDisconnect =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: ADeviceWrite =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AAssert =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AErrorStmt =>
          nodeBuilder(CfgStmtNode(data = node))
        case _: AExpr | _: AIdentifierDeclaration | _: AProgram => ???
      }

    recGen(fun)
  }
}

class FragmentCfg(private[cfg] val graphEntries: Set[CfgNode], private[cfg] val graphExits: Set[CfgNode]) {

  /** Returns true if this is the unit CFG w.r.t. to concatenation.
 */
  def isUnit: Boolean = graphEntries.isEmpty && graphExits.isEmpty

  /** Returns the concatenation of this CFG with `after`.
 */
  def ~(after: FragmentCfg): FragmentCfg =
    if (isUnit) after
    else if (after.isUnit) this
    else {
      graphExits.foreach(_.succ ++= after.graphEntries)
      after.graphEntries.foreach(_.pred ++= graphExits)
      new FragmentCfg(graphEntries, after.graphExits)
    }

  /** Returns the union of this CFG with `other`.
 */
  def |(other: FragmentCfg): FragmentCfg =
    new FragmentCfg(other.graphEntries.union(graphEntries), other.graphExits.union(graphExits))

  /** Returns the set of nodes in the CFG.
 */
  def nodes: Set[CfgNode] =
    graphEntries.flatMap { entry =>
      nodesRec(entry).toSet
    }

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set()): mutable.Set[CfgNode] = {
    if (!visited.contains(n)) {
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    }
    visited
  }

  /** Returns a map associating each node with its rank. The rank is defined such that rank(x) < rank(y) iff y is
 * visited after x in a depth-first visit of the control-flow graph
 */
  lazy val rank: Map[CfgNode, Int] = {
    def rankRec(elems: List[CfgNode], visited: List[List[CfgNode]], level: Int): Map[CfgNode, Int] = {
      val curLevel = elems.map { x =>
        x -> level
      }.toMap
      val newNeighbors = elems.flatMap(_.succ).filterNot(visited.flatten.contains).distinct
      if (newNeighbors.isEmpty)
        Map() ++ curLevel
      else
        rankRec(newNeighbors, newNeighbors :: visited, level + 1) ++ curLevel
    }
    rankRec(graphEntries.toList, List(graphEntries.toList), 0)
  }
}
 */
