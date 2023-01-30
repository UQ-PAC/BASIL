package analysis
import scala.collection.mutable
import astnodes._
import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}

object CfgNode:

  var id: Int = 0

  def nextId(): Int =
    id += 1
    id

/** Node in the control-flow graph.
  */
trait CfgNode:

  /** Predecessors of the current node. */
  val pred: mutable.Set[CfgNode]

  /** Successors of the current node. */
  val succ: mutable.Set[CfgNode]

  /** Unique identifier. */
  val id: Int

  /** Add an outgoing edge from the current node.
    */
  def addEdge(other: CfgNode): Unit =
    succ += other
    other.pred += this

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
    data: Subroutine
) extends CfgNodeWithData[Subroutine]:
  override def toString: String = s"[FunctionEntry] $data"

/** Control-flow graph node for the exit of a function.
  */
case class CfgFunctionExitNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Subroutine
) extends CfgNodeWithData[Subroutine]:
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

/** Control-flow graph node for a statement.
  */
case class CfgStatementNode(
    override val id: Int = CfgNode.nextId(),
    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
    data: Statement
) extends CfgNodeWithData[Statement]:
  override def toString: String = s"[Stmt] $data"

/** A control-flow graph.
  * @param entry
  *   the entry to the graph (source)
  * @param exits
  *   the exit to he graph (sink)
  */
class Cfg(val entries: Set[CfgNode], val exits: Set[CfgNode]):

  /** Returns whether or not the graph is empty.
    */
  def isUnit: Boolean = entries.isEmpty && exits.isEmpty

  /** Returns the concatenation of this CFG with `after`.
    */
  def concat(after: Cfg): Cfg =
    if isUnit then after
    else if after.isUnit then this
    else
      exits.foreach(_.succ ++= after.entries)
      after.entries.foreach(_.pred ++= exits)
      new Cfg(entries, after.exits)

  /** Returns the union of this CFG with `other`.
    */
  def union(other: Cfg): Cfg =
    new Cfg(other.entries.union(entries), other.exits.union(exits))

  /** Returns the set of nodes in the CFG.
    */
  def nodes: Set[CfgNode] =
    //val visited = mutable.Set[CfgNode]()
    entries.flatMap {
      entry =>
      nodesRec(entry).toSet
    }

  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set[CfgNode]()): mutable.Set[CfgNode] =
    if !visited.contains(n) then
      visited += n
      n.succ.foreach { n =>
        nodesRec(n, visited)
      }
    visited

object Cfg:

  /** Creates an empty cfg.
    */
  def unit(): Cfg = Cfg(Set(), Set())

  /** Creates a cfg consisting of a single node.
    */
  def singletonGraph(node: CfgNode): Cfg = Cfg(Set(node), Set(node))

  /** Generate the cfg for each function of the program.
    */
  def generateCfgProgram(program: Program): Map[Subroutine, Cfg] =
    program.functions.map(f => f -> generateCfgFunc(f)).toMap

  /** Generate the cfg for a function.
    */

  // what does it mean when the block is not found? do we create that block (but what statements would be inside that block?) or do we throw an error or we pass?   ans: make a node for each unknown block
  // should we assume that the target is always a new block (cause normally it could be pointing to a set of instruction inside that block)?
  // Design choice: what do we do with indirect calls when we don't know the target and also we don't know if the LocalVars would point to a block target or not
  // if not do we point that indirect call to a default block that is a place holder (unknown location block) so that when we do the analysis we resolve those locations

  def generateCfgFunc(func: Subroutine): Cfg = {
    val blocks = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap
    //val blocks = collection.mutable.Map(blockss.toSeq: _*)
    val entryNode = CfgFunctionEntryNode(data = func)
    val exitNode = CfgFunctionExitNode(data = func)



    val cfgs = mutable.Map[String, Cfg]()

    // generate cfg for a statement
    def generateCfgStatement(stmt: Statement): Cfg =
      val node = CfgStatementNode(data = stmt)

      // TODO: commented out the GoTo case because it is not supported yet and it was causing an error when the ast was
      // made mutable (ie. case class -> class)
//      stmt match {
//                case GoTo(target, condition, _, _) =>
//                  blocks.get(target) match
//                    case Some(blockNode) => node.addEdge(blockNode)
//                    case _               =>
//
//
//        case _ =>
//      }

      stmt match {
        case goTo: GoTo =>
          blocks.get(goTo.target) match
            case Some(blockNode) => node.addEdge(blockNode)
            case _ =>

        case directCall: DirectCall =>
          blocks.get(directCall.target) match
            case Some(blockNode) => node.addEdge(blockNode)
            case _ =>

//            {
//              // make a new block node and add edge
//              val newBlockNode = CfgBlockEntryNode(data = Block(label = directCall.target, statements = List()))
//              blocks += (directCall.target -> newBlockNode)
//              node.addEdge(newBlockNode)
//            }
            directCall.returnTarget match
              case Some(returnTarget) =>
                blocks.get(returnTarget) match
                  case Some(returnBlockNode) => returnBlockNode.addEdge(node)
                  case _ =>
//                  {
//                    // make a new block node and add edge
//                    val newBlockNode = CfgBlockEntryNode(data = Block(label = returnTarget, statements = List()))
//                    blocks += (returnTarget -> newBlockNode)
//                    node.addEdge(newBlockNode)
//                  }
              case _ =>

        case indirectCall: IndirectCall =>
            node.addEdge(CfgBlockEntryNode(data = Block(label = indirectCall.locals.toString(), address = null, statements = List())))
            indirectCall.returnTarget match
              case Some(returnTarget) =>
                blocks.get(returnTarget) match
                  case Some(returnBlockNode) => returnBlockNode.addEdge(node)
                  case _ =>
//                  {
//                    // make a new block node and add edge
//                    val newBlockNode = CfgBlockEntryNode(data = Block(label = returnTarget, statements = List()))
//                    blocks += (returnTarget -> newBlockNode)
//                    node.addEdge(newBlockNode)
//                  }
              case _ =>
        case _ =>
      }

      singletonGraph(node)

    for (_, entryNode) <- blocks do
      val exitNode = CfgBlockExitNode(data = entryNode.data)
      val stmts = entryNode.data.statements
      val body = stmts.foldLeft(unit())((acc, stmt) => acc.concat(generateCfgStatement(stmt)))

      val cfg = singletonGraph(entryNode).concat(body).concat(singletonGraph(exitNode))
      cfgs += (entryNode.data.label -> cfg)

    // this is not good
    if (func.blocks.nonEmpty) {
      cfgs.get(func.blocks.head.label) match
        case Some(cfg) => singletonGraph(entryNode).concat(cfg).concat(singletonGraph(exitNode))
        case _ => throw new RuntimeException("error generating function cfg")
    } else {
      singletonGraph(entryNode).concat(singletonGraph(exitNode))
    }
  }


//  def generateForNode(node: Any): Cfg = {
//    node match {
//      case func: Subroutine => {
//        val blocks = generateForNode(func.blocks.toSet)
//        val entryNode = CfgFunctionEntryNode(data = func)
//        val exitNode = CfgFunctionExitNode(data = func)
//
//        singletonGraph(entryNode).concat(blocks).concat(singletonGraph(exitNode))
//      }
//      case blkList: List[Block] => {
//        for (blk <- blkList) {
//          val stmts = generateForNode(blk.statements.toSet)
//          val entryNode = CfgBlockEntryNode(data = blk)
//          val exitNode = CfgFunctionExitNode(data = entryNode.data)
//
//          singletonGraph(entryNode).concat(stmts).concat(singletonGraph(exitNode))
//        }
//      }
//      case stmtList: List[Statement] => {
//        for (stmt <- stmtList) {
//          val node = CfgStatementNode(data = stmt)
//          singletonGraph(node)
//        }
//      }
//  }
//
//    // for each statement type generate a cfg
//    def generateCfgForStatement(stmt: Statement): Cfg = {
//      val node = CfgStatementNode(data = stmt)
//      stmt match {
//        case DirectCall(target, condition, returnTarget, line, instruction) => {
//
//        }
//      }
//        case _ => singletonGraph(node)
//      }
//  }







/** Control-flow graph for an entire program.
  *
  * @param prog
  *   AST of the program
  * @param funEntries
  *   map from AST function declarations to CFG function entry nodes
  * @param funExits
  *   map from AST function declarations to CFG function exit nodes
  */
abstract class ProgramCfg(
                           val prog: Program,
                           val funEntries: Map[Subroutine, CfgFunctionEntryNode],
                           val funExits: Map[Subroutine, CfgFunctionExitNode]
) extends Cfg(funEntries.values.toSet, funExits.values.toSet):
  /**
   * Returns a Graphviz dot representation of the CFG.
   * Each node is labeled using the given function labeler.
   */
  def toDot(labeler: CfgNode => String, idGen: CfgNode => String): String = {
    val dotNodes = mutable.Map[CfgNode, DotNode]()
    var dotArrows = mutable.ListBuffer[DotArrow]()
    nodes.foreach { n =>

      dotNodes += (n -> new DotNode(s"${idGen(n)}", labeler(n), Map()))
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
    val funGraphs = Cfg.generateCfgProgram(prog)
    val allEntries = funGraphs.view.mapValues(cfg => cfg.entries.head.asInstanceOf[CfgFunctionEntryNode]).toMap
    val allExits = funGraphs.view.mapValues(cfg => cfg.exits.head.asInstanceOf[CfgFunctionExitNode]).toMap

    new IntraproceduralProgramCfg(prog, allEntries, allExits)

/** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
  * nodes.
  */
class IntraproceduralProgramCfg(
                                 prog: Program,
                                 funEntries: Map[Subroutine, CfgFunctionEntryNode],
                                 funExits: Map[Subroutine, CfgFunctionExitNode]
) extends ProgramCfg(prog, funEntries, funExits):
  override def toString: String = funEntries.keys.toString()
