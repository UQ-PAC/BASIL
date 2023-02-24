//package analysis
//import scala.collection.mutable
//import astnodes.*
//import cfg_visualiser.{DotArrow, DotDirArrow, DotGraph, DotNode}
//
//import scala.collection.mutable.ListBuffer
//
//object CfgNode:
//
//  var id: Int = 0
//
//  def nextId(): Int =
//    id += 1
//    id
//
//// EdgeType
//abstract class Edge(from: CfgNode, to: CfgNode)
//case class conditionalEdge(cond: Expr, from: CfgNode, to: CfgNode) extends Edge(from, to) {
//  override def toString: String = s"conditionalEdge(cond: $cond, From: $from, To: $to)"
//}
//
//case class unconditionalEdge(from: CfgNode, to: CfgNode) extends Edge(from = from, to = to) {
//  override def toString: String = s"unconditionalEdge(From: $from, To: $to)"
//}
//
//val edges: ListBuffer[Edge] = ListBuffer[Edge]()
//
///** Node in the control-flow graph.
//  */
//trait CfgNode:
//
//  /** Predecessors of the current node. */
//  val pred: mutable.Set[CfgNode]
//
//  /** Successors of the current node. */
//  val succ: mutable.Set[CfgNode]
//
//  /** Unique identifier. */
//  val id: Int
//
//  /** Add an outgoing edge from the current node.
//    */
//  def addEdge(other: CfgNode, cond: Expr): Unit =
//    succ += other
//    other.pred += this
//    if cond != null then
//      edges += conditionalEdge(cond, this, other)
//    else
//      edges += unconditionalEdge(this, other)
//
//  override def equals(obj: scala.Any): Boolean =
//    obj match
//      case o: CfgNode => o.id == this.id
//      case _          => false
//
//  override def hashCode(): Int = id
//
///** Control-flow graph node that additionally stores an AST node.
//  */
//trait CfgNodeWithData[T] extends CfgNode:
//
//  def data: T
//
///** Control-flow graph node for the entry of a function.
//  */
//case class CfgFunctionEntryNode(
//    override val id: Int = CfgNode.nextId(),
//    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    data: Subroutine
//) extends CfgNodeWithData[Subroutine]:
//  override def toString: String = s"[FunctionEntry] $data"
//
///** Control-flow graph node for the exit of a function.
//  */
//case class CfgFunctionExitNode(
//    override val id: Int = CfgNode.nextId(),
//    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    data: Subroutine
//) extends CfgNodeWithData[Subroutine]:
//  override def toString: String = s"[FunctionExit] $data"
//
///** Control-flow graph node for a block.
//  */
//case class CfgBlockEntryNode(
//    override val id: Int = CfgNode.nextId(),
//    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    data: Block
//) extends CfgNodeWithData[Block]:
//  override def toString: String = s"[BlockEntry] $data"
//
///** Control-flow graph node for a block.
//  */
//case class CfgBlockExitNode(
//    override val id: Int = CfgNode.nextId(),
//    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    data: Block
//) extends CfgNodeWithData[Block]:
//  override def toString: String = s"[BlockExit] $data"
//
///** Control-flow graph node for a statement.
//  */
//case class CfgStatementNode(
//    override val id: Int = CfgNode.nextId(),
//    override val pred: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    override val succ: mutable.Set[CfgNode] = mutable.Set[CfgNode](),
//    data: Statement
//) extends CfgNodeWithData[Statement]:
//  override def toString: String = s"[Stmt] $data"
//
///** A control-flow graph.
//  * @param entry
//  *   the entry to the graph (source)
//  * @param exits
//  *   the exit to he graph (sink)
//  */
//class Cfg(val entries: Set[CfgNode], val exits: Set[CfgNode]):
//
//  def getEdges: ListBuffer[Edge] = {
//    edges
//  }
//
//  /** Returns whether or not the graph is empty.
//    */
//  def isUnit: Boolean = entries.isEmpty && exits.isEmpty
//
//  /** Returns the concatenation of this CFG with `after`.
//    */
//  def concat(after: Cfg): Cfg =
//    if isUnit then after
//    else if after.isUnit then this
//    else
//      exits.foreach(_.succ ++= after.entries)
//      after.entries.foreach(_.pred ++= exits)
//      new Cfg(entries, after.exits)
//
//  /** Returns the union of this CFG with `other`.
//    */
//  def union(other: Cfg): Cfg =
//    new Cfg(other.entries.union(entries), other.exits.union(exits))
//
//  /** Returns the set of nodes in the CFG.
//    */
//  def nodes: Set[CfgNode] =
//    //val visited = mutable.Set[CfgNode]()
//    entries.flatMap {
//      entry =>
//      nodesRec(entry).toSet
//    }
//
//  protected def nodesRec(n: CfgNode, visited: mutable.Set[CfgNode] = mutable.Set[CfgNode]()): mutable.Set[CfgNode] =
//    if !visited.contains(n) then
//      visited += n
//      n.succ.foreach { n =>
//        nodesRec(n, visited)
//      }
//    visited
//
//case class NodePool() {
//  val pool = mutable.Map[(Statement, CfgBlockEntryNode), CfgNode]()
//  var latestAdded: (Statement, CfgBlockEntryNode) = (null: Statement, null: CfgBlockEntryNode)
//
//  def get(statement: Statement, context: CfgBlockEntryNode): CfgNode = {
//    if (!statement.isInstanceOf[GoTo]) {
//      latestAdded = (statement, context)
//    }
//    if (pool.contains((statement, context))) {
//      pool((statement, context))
//    }
//    else {
//      val node = CfgStatementNode(data = statement)
//      pool((statement, context)) = node
//      node
//    }
//  }
//
//  def getLatestAdded(): CfgNode = {
//    pool(latestAdded)
//  }
//}
//
//object Cfg:
//
//  /** Creates an empty cfg.
//    */
//  def unit(): Cfg = Cfg(Set(), Set())
//
//  /** Creates a cfg consisting of a single node.
//    */
//  def singletonGraph(node: CfgNode): Cfg = Cfg(Set(node), Set(node))
//
//  /** Generate the cfg for each function of the program.
//    */
//  def generateCfgProgram(program: Program): Map[Subroutine, Cfg] =
//    program.functions.map(f => f -> generateCfgFunc(f)).toMap
//
//  /** Generate the cfg for a function.
//    */
//
//  // what does it mean when the block is not found? do we create that block (but what statements would be inside that block?) or do we throw an error or we pass?   ans: make a node for each unknown block
//  // should we assume that the target is always a new block (cause normally it could be pointing to a set of instruction inside that block)?
//  // Design choice: what do we do with indirect calls when we don't know the target and also we don't know if the LocalVars would point to a block target or not
//  // if not do we point that indirect call to a default block that is a place holder (unknown location block) so that when we do the analysis we resolve those locations
//
//  //val branchGoToChecker: BranchGoToChecker = BranchGoToChecker()
//  val nodePool = NodePool()
//  def generateCfgFunc(func: Subroutine): Cfg = {
//    val blocks = func.blocks.map(block => block.label -> CfgBlockEntryNode(data = block)).toMap
//    //val blocks = collection.mutable.Map(blockss.toSeq: _*)
//    val functionEntryNode = CfgFunctionEntryNode(data = func)
//    val functionExitNode = CfgFunctionExitNode(data = func)
//
//
//
//    val cfgs = mutable.Map[String, Cfg]()
//
//    // generate cfg for a statement
//    def generateCfgStatement(stmt: Statement, context: CfgBlockEntryNode): Cfg =
//
//      val node: CfgNode = nodePool.get(stmt, context)
//      stmt match {
//        case goTo: GoTo =>
////          val parentNode = branchGoToChecker.getNode()
////          if (parentNode != null) {
////            parentNode.addEdge(node)
////          }
//          // edge between current -> target
//          blocks.get(goTo.target) match
//
//            case Some(blockNode) => nodePool.getLatestAdded().addEdge(nodePool.get(blockNode.data.statements.head, blockNode), goTo.condition)
//            case _ => print(s"ERROR: goto target in '${goTo}' not found\n")
//
//        case directCall: DirectCall =>
//          // edge between current -> target
//          blocks.get(directCall.target) match
//            case Some(blockNode) => node.addEdge(blockNode, null)
//            case _ => print(s"ERROR: direct call target in '${directCall}' not found\n")
//            directCall.returnTarget match
//              case Some(returnTarget) =>
//                blocks.get(returnTarget) match
//                  case Some(returnBlockNode) =>
//                    blocks.get(directCall.target) match
//                      // edge between target -> return target
//                      case Some(blockNode) => blockNode.addEdge(returnBlockNode, null)
//                      case _ =>
//                  case _ =>
//                    print(s"ERROR: direct call return target in '${directCall}' not found\n")
//              case _ =>
//                // edge between target -> current (if no return target)
//                blocks.get(directCall.target) match
//                  case Some(blockNode) => blockNode.addEdge(node, null)
//                  case _ =>
//
//        case indirectCall: IndirectCall =>
//            // edge between current -> unknown block
//            val unknownBlockNode = CfgBlockEntryNode(data = Block(label = s"Unknown: ${indirectCall.locals.toString()}", address = null, statements = List()))
//            //node.addEdge(unknownBlockNode)
//            if (indirectCall.target.name == "R30") {
//              node.addEdge(functionExitNode, null)
//            }
//            print(s"ERROR: indirect call target in '${indirectCall}' not found\n")
//            indirectCall.returnTarget match
//              case Some(returnTarget) =>
//                // edge between unknown block -> return target
//                blocks.get(returnTarget) match
//                  case Some(returnBlockNode) => //unknownBlockNode.addEdge(returnBlockNode)
//                  case _ => print(s"ERROR: indirect call return target in '${indirectCall}' not found\n")
//              // edge between unknown block -> current (if no return target)
//              case _ => //unknownBlockNode.addEdge(node)
//        case _ =>
//      }
//
//      singletonGraph(node)
//
//    for (_, entryNode) <- blocks do
//      val exitNode = CfgBlockExitNode(data = entryNode.data)
//      val stmts = entryNode.data.statements
////      val body = stmts.foldLeft(unit())((acc, stmt) => acc.union(generateCfgStatement(stmt, entryNode)))
//
//      var cfg: Cfg = unit()
//      stmts.foreach(stmt => {
//        if (stmt.isInstanceOf[GoTo]) {
//          generateCfgStatement(stmt, entryNode)
//        } else {
//          cfg = cfg.concat(generateCfgStatement(stmt, entryNode))
//        }
//      })
//
//
//
//
//      //val cfg = singletonGraph(entryNode).concat(body).concat(singletonGraph(exitNode))
//      cfgs += (entryNode.data.label -> cfg)
//
//    nodePool.pool.foreach(p => print(s"\n Pool(${p})\n"))
//    // this is not good
//    if (func.blocks.nonEmpty) {
//      cfgs.get(func.blocks.head.label) match
//        case Some(cfg) => singletonGraph(functionEntryNode).concat(cfg).concat(singletonGraph(functionExitNode))
//        case _ => throw new RuntimeException("error generating function cfg")
//    } else {
//      singletonGraph(functionEntryNode).concat(singletonGraph(functionExitNode))
//    }
//  }
//
///** Control-flow graph for an entire program.
//  *
//  * @param prog
//  *   AST of the program
//  * @param funEntries
//  *   map from AST function declarations to CFG function entry nodes
//  * @param funExits
//  *   map from AST function declarations to CFG function exit nodes
//  */
//abstract class ProgramCfg(
//                           val prog: Program,
//                           val funEntries: Map[Subroutine, CfgFunctionEntryNode],
//                           val funExits: Map[Subroutine, CfgFunctionExitNode]
//) extends Cfg(funEntries.values.toSet, funExits.values.toSet):
//  /**
//   * Returns a Graphviz dot representation of the CFG.
//   * Each node is labeled using the given function labeler.
//   */
//  def toDot(labeler: CfgNode => String, idGen: (CfgNode, Int) => String): String = {
//    val dotNodes = mutable.Map[CfgNode, DotNode]()
//    var dotArrows = mutable.ListBuffer[DotArrow]()
//    var uniqueId = 0
//    nodes.foreach { n =>
//
//      dotNodes += (n -> new DotNode(s"${idGen(n, uniqueId)}", labeler(n)))
//      uniqueId += 1
//    }
//    nodes.foreach { n =>
//      n.succ.foreach { dest =>
//        dotArrows += new DotDirArrow(dotNodes(n), dotNodes(dest))
//      }
//    }
//    dotArrows = dotArrows.sortBy(arr => arr.fromNode.id + "-" + arr.toNode.id)
//    val allNodes = dotNodes.values.seq.toList.sortBy(n => n.id)
//    new DotGraph("CFG", allNodes, dotArrows).toDotString
//  }
//
//object IntraproceduralProgramCfg:
//
//  /** Generates an [[IntraproceduralProgramCfg]] from a program.
//    */
//  def generateFromProgram(prog: Program): IntraproceduralProgramCfg =
//    val funGraphs = Cfg.generateCfgProgram(prog)
//    val allEntries = funGraphs.view.mapValues(cfg => cfg.entries.head.asInstanceOf[CfgFunctionEntryNode]).toMap
//    val allExits = funGraphs.view.mapValues(cfg => cfg.exits.head.asInstanceOf[CfgFunctionExitNode]).toMap
//
//    new IntraproceduralProgramCfg(prog, allEntries, allExits)
//
///** Control-flow graph for a program, where function calls are represented as expressions, without using call/after-call
//  * nodes.
//  */
//class IntraproceduralProgramCfg(
//                                 prog: Program,
//                                 funEntries: Map[Subroutine, CfgFunctionEntryNode],
//                                 funExits: Map[Subroutine, CfgFunctionExitNode]
//) extends ProgramCfg(prog, funEntries, funExits):
//  override def toString: String = funEntries.keys.toString()
