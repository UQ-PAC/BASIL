package translating

import astnodes.exp.`var`.Register
import astnodes.stmt.*
import astnodes.stmt.assign.{Assign, RegisterAssign}
import util.AssumptionViolationException

import java.util
import java.util.*
import java.util.stream.Collectors
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

// TODO neaten scala code
// TODO convert to scala collections

/** A flow graph is a graphical representation of a bil/boogie program. Nodes in the graph represent basic blocks, which
  * are defined by jumps, conditional jumps, function headers, function returns and lines which are jumped to. Edges in
  * the graph represent 'links' (i.e. conditional jumps, jumps or simply the following line) between blocks. In this
  * file, a block 'cluster' refers to a complete subgraph of linked blocks, which is disjoint from all other blocks in
  * the flow graph. An example of a cluster is a function cluster, which represents all blocks within a self-contained
  * function (or 'procedure' in boogie). The 'head' of a cluster refers to the root block of that cluster. For example,
  * the first line of a function cluster's root block will always be an EnterSub. The globalBlock is the head of a
  * cluster which represents all global code and is the presumed starting point for the boogie program. Each
  * functionBlock is the head of a function cluster.
  *
  * A flow graph self-maintains particular guarantees for user programs:
  *   1. All lines (i.e. statements) in the flow graph are unique; !line1.equals(line2) for all line1, line2. 2.
  *      Clusters are disjoint; no block reachable by a head block is reachable by any other head block.
  */
object FlowGraph {

  /** Creates a FlowGraph from the given list of statements. Assumes no line is reachable from more than one function
    * header (i.e. EnterSub).
    */
  def fromStmts(stmts: util.List[Stmt], types: immutable.Map[String, Int]): FlowGraph = {
    val flowGraph = FlowGraphFactory.fromStmts(stmts, types)
    flowGraph.enforceDisjointFunctions()
    flowGraph
  }

  class Function(val header: EnterSub, val blocks: util.List[FlowGraph.Block]) {
    private val initStmts = new util.LinkedList[InitStmt]
    // TODO should automatically generate these
    initStmts.add(InitStmt(Register("ZF", -1), "ZF", "bv1"))
    initStmts.add(InitStmt(Register("CF", -1), "CF", "bv1"))
    initStmts.add(InitStmt(Register("NF", -1), "NF", "bv1"))
    initStmts.add(InitStmt(Register("VF", -1), "VF", "bv1"))

    // variable initialisations to be at the top of this function
    def getHeader: EnterSub = header
    def getBlocks: util.List[Block] = blocks
    def getLines: util.ArrayList[Stmt] = {
      val lines = new util.ArrayList[Stmt]
      blocks.forEach((block: FlowGraph.Block) => lines.addAll(block.getLines))
      lines
    }
    def addInitStmt(initStmt: InitStmt): Boolean = initStmts.add(initStmt)
    def getInitStmts = new util.ArrayList(initStmts)
    override def toString: String = {
      val builder = new StringBuilder
      builder.append(header).append("\n")
      initStmts.forEach((stmt: InitStmt) => builder.append(stmt).append("\n"))
      blocks.forEach(builder.append)
      builder.append("}")
      builder.toString.replaceAll("\n", "\n  ") + "\n"
    }
    
    
  }

  /** Factory for a flow graph. It works, at a high level, as follows: Take a list of statements. Partition the list
    * into chunks of statements called blocks. These are the nodes in the flow graph. Go through all the blocks and link
    * them to all the other blocks that they might transition to. Identify all the function blocks (i.e. blocks with
    * function headers as first lines). Return a flow graph of these function blocks.
    */
  private object FlowGraphFactory {

    /** Creates a FlowGraph from the given list of statements. Does not enforce any constraints.
      *
      * @requires
      *   statements are ordered and there are no duplicates
      * @param stmts
      *   an ordered list of statements from which to create the flow graph
      * @return
      *   a new flow graph with an empty global block and no constraints
      */
    def fromStmts(stmts: util.List[Stmt], types: immutable.Map[String, Int]): FlowGraph = {
      val stmts1 = mergeCjmp(stmts.asScala.toList)
      // val stmts2 = setFunctionsWithReturns(stmts1).asJava;
      val stmts2 = stmts1.asJava
      // an ordered list of indexes of the given statements list, indicating where the list should be split into blocks
      // val splits = getSplits(stmts2)
      val splits = getSplits(stmts2)
      // an ordered list of blocks, defined by the given given list of splits
      // essentially creates the nodes for this flow graph
      var blocks = createBlocksFromSplits(splits.asJava, stmts2)
      blocks = new util.ArrayList(stripBlocks(blocks.asScala.toList).asJava)
      // links each block in the list to each other block that may follow this one (e.g. via a jump)
      // essentially creates the edges for this flow graph
      setChildren(blocks, stmts2)
      // create a flow graph consisting of the functions formed from these blocks
      val flowGraph = new FlowGraph(convertBlocksToFunctions(blocks), types)
      // ensure the created flow graph maintains the required properties
      flowGraph.enforceConstraints()

      flowGraph
    }

    private def setFunctionsWithReturns(stmts: immutable.List[Stmt]): immutable.List[Stmt] = {
      var i = 0 // TODO work out a way to not use this

      // TODO ideally could collect (returning a partial funciton instead of some/none)
      (stmts.sliding(3, 1).flatMap{
        case (call: CallStmt) :: _ :: (assign: RegisterAssign) :: Nil =>
          call.setLHS(assign.lhs)
          i = 2
          Some(call)
        case x :: rest if i == 2 =>
          i -= 1
          Some(x)
        case _ if i == 1 =>
          i -= 1
          None
        case stmt :: rest => Some(stmt)
        case Nil => None
      } ++ stmts.takeRight(2)).toList  // Make sure to get the last 3 lines
        // TODO check 2 or 3
    }


    private def findFunction(blocks: util.List[FlowGraph.Block], functionName: String) = blocks.stream
      .filter((b: FlowGraph.Block) => b.firstLine.isInstanceOf[EnterSub] && b.firstLine.asInstanceOf[EnterSub].funcName == functionName)
      .findFirst.get

    private def stripBlocks(blocks: immutable.List[FlowGraph.Block]): immutable.List[FlowGraph.Block] = {
      val reachableBlocks = new ArrayBuffer[FlowGraph.Block]
      val queue = new util.LinkedList[FlowGraph.Block]
      val mainBlock = blocks.find(_.getLines.get(0) match {
        case enterSub: EnterSub => enterSub.funcName == "main"
        case _ => false
      }).get

      queue.add(mainBlock)
      while ({ !queue.isEmpty }) {
        val block = queue.poll
        if (!reachableBlocks.contains(block)) {
          reachableBlocks += block

          // TODO can look just at the last line
          block.getLines.forEach {
            case jmp: JmpStmt => queue.add(findBlockStartingWith(jmp.target, blocks.asJava))
            case cjmp: CJmpStmt =>
              queue.add(findBlockStartingWith(cjmp.trueTarget, blocks.asJava))
              queue.add(findBlockStartingWith(cjmp.falseTarget, blocks.asJava))
            case call: CallStmt =>
              if (!call.libraryFunction) queue.add(findFunction(blocks.asJava, call.funcName))
              if (call.returnTarget.isDefined) queue.add(findBlockStartingWith(call.returnTarget.get, blocks.asJava))
            case _ =>
          }
        }
      }

      reachableBlocks.toList
    }

    /** Returns an ordered list of all statement indexes ('splits') which represent block boundaries. Splits are defined
      * such that the splits 3 and 6 should define a block consisting of lines stmts.get(3), stmts.get(4) and
      * stmts.get(5). Any given statements list will also be provided a split at the beginning and end of the program.
      *
      * @requires
      *   statements are ordered and there are no duplicates
      * @ensures
      *   the returned list of splits are ordered, and there is a split at index 0 and at index stmts.size()
      * @param stmts
      *   an ordered list of statements from which to create the splits
      * @return
      *   a list of splits indicating where blocks should be defined in the given statements list
      */
    private def getSplits(stmts: util.List[Stmt]) = { // we use a set to avoid double-ups, as some lines may be jumped to twice
      val splits = new util.HashSet[Integer]
      for (i <- 0 until stmts.size) {
        stmts.get(i) match {
          case jmpStmt: JmpStmt =>
            splits.add(i + 1)
            val targetIndex = findInstWithPc(jmpStmt.target, stmts)
            if (targetIndex != -1) splits.add(targetIndex)
          case jmpStmt: CJmpStmt =>
            splits.add(i + 1) // TODO check if we need this
            val trueTargetIndex = findInstWithPc(jmpStmt.trueTarget, stmts)
            if (trueTargetIndex != -1) splits.add(trueTargetIndex)
            val falseTargetIndex = findInstWithPc(jmpStmt.falseTarget, stmts)
            if (falseTargetIndex != -1) splits.add(falseTargetIndex)
          case _: CallStmt => splits.add(i + 1)
          case _: EnterSub => splits.add(i)
          case _: ExitSub => splits.add(i + 1)
          case _ =>
        }
      }

      // ensure there is a split at the start and end of the program
      splits.add(0)
      splits.add(stmts.size)
      val splitsList = splits.asScala.toList.sorted
      splitsList
    }

    /** Locates the statement in the given list that has the given PC.
      *
      * @param pc
      *   of the statement to find
      * @param stmts
      *   the list of statements to search
      * @return
      *   the statement in the given list that has the given PC.
      */
    private def findInstWithPc(pc: String, stmts: util.List[Stmt]): Int = {
      if (pc.substring(0, 2) == "__") return -1 // TODO when jumping to a function e.g. goto @__gmon_start__
      for (i <- 0 until stmts.size) { if (stmts.get(i).label.pc == pc) return i }
      throw new AssumptionViolationException(s"Error in constructing flow graph: No inst found with pc $pc.\n")
    }

    /** Creates a list of blocks from each pair of consecutive splits. For example, for splits = [0, 3, 4, 8]
      * and lines = [a, b, c, d, e, f, g, h], we will get blocks of: [a, b, c], [d], [e, f, g, h]
      *
      * @requires
      *   splits and lines are sorted and splits.contains(0) and splits.contains(lines.size())
      * @param splits
      *   a list of indexes to define the boundaries of blocks
      * @param lines
      *   a list of statements from which to extract blocks at the indexes given by splits
      * @return
      *   a list of blocks consisting of sublists of the given statements list, as defined by the given splits list
      */
    private def createBlocksFromSplits(splits: util.List[Integer], lines: util.List[Stmt]) = {
      val blocks = new util.ArrayList[FlowGraph.Block]
      for (i <- 0 until splits.size - 1) { // need to convert the sublist view to a real arraylist to avoid ConcurrentModificationException
        val blockLines = new util.ArrayList[Stmt](lines.subList(splits.get(i), splits.get(i + 1)))
// blocks are initially created with no children
        val block =
          new FlowGraph.Block(blockLines.get(0).label.pc, blockLines, new util.ArrayList[FlowGraph.Block])
        blocks.add(block)
      }
      blocks
    }

    /** Sets the children of all blocks in the given list. Think of this like "wiring up" all the blocks in the list
      * with each other.
      *
      * @requires
      *   all possible children of blocks within the given list are also contained in the list, and the given list of
      *   statements is ordered
      * @param blocks
      *   the list of blocks to create children for
      * @param stmts
      *   the ordered list of statements the given blocks were created from; necessary for finding statements which
      *   directly follow the last statement in a block, as it may represent a child of this block if the last line is,
      *   for instance, not a jump
      */
    private def setChildren(blocks: util.List[FlowGraph.Block], stmts: util.List[Stmt]): Unit = {
      for (block <- blocks.asScala) { // the PCs of all statements this block jumps to (for instance, the targets of jumps)
        val childrenPcs = getChildrenPcs(block, stmts)
        for (childPc <- childrenPcs) { // tries to find a block in the list with a first line that has the child's PC
          val child = findBlockStartingWith(childPc, blocks)
          // no such block was found, which means there is no block defined for this jump/cjump etc.
          if (child == null)
            throw new AssumptionViolationException(s"Error creating flow graph. Could not find a block starting with target pc '$childPc'.")
          block.children.add(child)
        }
      }
    }

    /** Finds all PCs of lines that the given block may transition to. Presuming the given block was properly created,
      * this will solely depend on the last line in the block.
      *
      * @requires
      *   the list of lines contains all PCs which may be jumped to, and the given block was properly created (e.g. does
      *   not contain any jumps halfway through), and the list of lines is ordered
      * @param block
      *   to find the children PCs of
      * @param lines
      *   see {@link #setChildren}
      * @return
      *   a list of PCs representing the children of the given block
      */
    private def getChildrenPcs(block: FlowGraph.Block, lines: util.List[Stmt]): immutable.List[String] = {
      block.lastLine match {
        case jmp: JmpStmt => immutable.List(jmp.target)
        case cjmp: CJmpStmt => immutable.List(cjmp.trueTarget, cjmp.falseTarget)
        // TODO case exitSub: ExitSub => immutable.List(lines.get(lines.indexOf(exitSub) + 1).getLabel.pc)
        case callStmt: CallStmt => callStmt.returnTarget.toList
        case _: ExitSub => immutable.List()
      }
    }

    // TODO this and the stripping could happen at the same time I think
    private def convertBlocksToFunctions(blocks: util.List[FlowGraph.Block]) = {
      val functions = new util.ArrayList[FlowGraph.Function]
      for (block <- blocks.asScala) {
        block.firstLine match {
          case firstLine: EnterSub =>
            val blocksInFunction = new util.ArrayList[FlowGraph.Block]
            dfsOnBlock(blocksInFunction, block)
            val function = new FlowGraph.Function(firstLine, blocksInFunction)
            functions.add(function)
          case _ =>
        }
      }
      functions.forEach((function: FlowGraph.Function) => function.getBlocks.get(0).lines.remove(0))
      functions
    }
    private def dfsOnBlock(blocks: util.List[FlowGraph.Block], next: FlowGraph.Block): Unit = {
      blocks.add(next)
      for (child <- next.getChildren.asScala) { if (!blocks.contains(child)) dfsOnBlock(blocks, child) }
    }

    /** Searches for a block in the given list that contains a first line with the given PC. Returns null if none found.
      *
      * @param pc
      *   the PC to search for
      * @param blocks
      *   the list of blocks to search through
      * @return
      *   the first block with a first line that contains the given pc, or null if none found
      */
    private def findBlockStartingWith(pc: String, blocks: util.List[FlowGraph.Block]): FlowGraph.Block = {
      for (block <- blocks.asScala) { if (block.firstLine.label.pc == pc) return block }
      null
    }

    private def mergeCjmp (stmts: immutable.List[Stmt]): immutable.List[Stmt] = stmts match {
      case (cjmp: CJmpStmt) :: (jmp: JmpStmt) :: rest => cjmp.copy(falseTarget = jmp.target) +: mergeCjmp(rest)
      case (cjmp: CJmpStmt) :: _ => throw new AssumptionViolationException("Unexpected conditional jump")
      case stmt :: rest => stmt +: mergeCjmp(rest)
      case Nil => Nil
    }

  }

  /** A block is an ordered list of instruction statements (i.e. lines). They represent nodes in the flow graph. Blocks
    * have a list of children blocks, which represent directed edges in the flow graph (from block, to child). They are
    * designed to be highly malleable, and therefore do not provide any guarantees about duplicates or other
    * constraints.
    */
  class Block private[translating] ( // the label of this block
                                     val label: String, // the lines of code this block contains
                                     var lines: util.List[Stmt], // a list of other blocks this block may transition to
                                     // TODO im not sure if we actually need to store the children
                                     var children: util.List[FlowGraph.Block]
  )
  /** Constructor for a block.
    */ {
    def getLabel: String = label

    /** @return
      *   the list object containing lines of code this block contains
      */
    def getLines: util.List[Stmt] = lines

    /** Sets the lines contained in this block.
      * @param lines
      *   to set
      */
    def setLines(lines: util.List[Stmt]): Unit = this.lines = lines

    def replaceStmt(newStmt: Stmt, index: Int): Stmt = {
      lines.set(index, newStmt)
    }

    /** @return
      *   the list object containing the children of this block
      */
    def getChildren: util.List[Block] = children
    def removeLine(line: Stmt): Boolean = lines.remove(line)

    /** A convenience method that returns the first line of this block.
      * @return
      *   the first line of this block
      */
    def firstLine: Stmt = lines.get(0)

    /** A convenience method that returns the last line of this block.
      * @return
      *   the last line of this block
      */
    def lastLine: Stmt = lines.get(lines.size - 1)
// TODO blocks cant start with a number
    override def toString: String = {
      val builder = new StringBuilder
      builder.append("label").append(label).append(":\n")
      lines.forEach((line: Stmt) => builder.append("  ").append(line).append("\n"))
      builder.toString
    }
  }
}

class FlowGraph (var functions: util.List[FlowGraph.Function], val types: immutable.Map[String, Int]) {
  // TODO this isnt great
  private var globalInits: util.List[InitStmt] = new util.LinkedList[InitStmt].asInstanceOf[util.List[InitStmt]]
  globalInits.add(InitStmt(Register("heap", -1), "heap", "[bv64] bv8")) // TODO label.none
  globalInits.add(InitStmt(Register("heap_free", -1), "heap_free", "[bv64] bool"))
  globalInits.add(InitStmt(Register("heap_sizes", -1), "heap_size", "[bv64] bv64"))
  globalInits.add(InitStmt(Register("stack", -1), "stack", "[bv64] bv8"))
  globalInits.add(InitStmt(Register("SP", -1), "SP", "bv64"))
  globalInits.add(InitStmt(Register("R31", -1), "R31", "bv64"))

  def getGlobalInits: util.List[InitStmt] = globalInits
  def setGlobalInits(inits: util.List[InitStmt]): Unit = this.globalInits = inits
  def setFunctions(functions: util.List[FlowGraph.Function]): Unit = this.functions = functions

  /** @return
    *   the list of functions of this flow graph
    */
  def getFunctions: util.List[FlowGraph.Function] = functions

  /** @return
    *   all lines of all blocks within this flow graph
    */
  def getLines: util.ArrayList[Stmt] = {
    val lines = new util.ArrayList[Stmt]
    getBlocks.forEach((block: FlowGraph.Block) => lines.addAll(block.getLines))
    lines
  }

  /** @return
    *   all blocks within this flow graph
    */
  def getBlocks: util.ArrayList[FlowGraph.Block] = {
    val blocks = new util.ArrayList[FlowGraph.Block]
    functions.forEach((function: FlowGraph.Function) => blocks.addAll(function.getBlocks))
    blocks
  }
  def removeLine(line: Stmt): Unit = getBlocks.forEach((block: FlowGraph.Block) => block.getLines.remove(line))

  def replaceLine(newStmt: Stmt, oldStmt: Stmt): Unit = {
    getBlocks.forEach(block => {
      if (block.getLines.contains(oldStmt)) {
        val index = block.getLines.indexOf(oldStmt)
        block.replaceStmt(newStmt, index)
      }
    })
  }

  /** Enforce guaranteed properties of this flow graph. Exceptions are thrown when these constraints are not met.
    */
  def enforceConstraints(): Unit = {
    enforceDisjointFunctions()
    enforceUniqueLines()
  }

  /** No block should be accessible (directly or indirectly) by two different function blocks.
    */
  private def enforceDisjointFunctions(): Unit = {
    val usedBlocks = new util.HashSet[FlowGraph.Block]
    for (function <- functions.asScala) {
      val cluster = function.getBlocks
      for (block <- cluster.asScala) {
        if (usedBlocks.contains(block))
          throw new AssumptionViolationException(
            String.format(
              "Flow graph error. The following block can be accessed by two different functions:\n%s",
              block.toString
            )
          )
      }
      usedBlocks.addAll(cluster)
    }
  }

  /** A complete traversal of a flow graph should encounter no line twice, or no line with the same PC twice.
    */
  private def enforceUniqueLines(): Unit = {
    val linesList = getLines
    val pcList = new util.ArrayList[String]
    linesList.forEach((line: Stmt) => pcList.add(line.label.pc))
    val linesSet = new util.HashSet[Stmt](linesList)
    val pcSet = new util.HashSet[String](pcList)
    if (linesSet.size != linesList.size) {
      linesSet.forEach(linesList.remove)
      val stringBuilder = new StringBuilder
      linesList.forEach((line: Stmt) => stringBuilder.append(line.toString))
      throw new AssumptionViolationException(s"Flow graph error. The following lines were found twice throughout the program:\n$stringBuilder")
    }
    if (pcSet.size != pcList.size) {
      pcSet.forEach(pcList.remove)
      val stringBuilder = new StringBuilder
      pcList.forEach(stringBuilder.append)
      throw new AssumptionViolationException(s"Flow graph error. The following lines were found twice throughout the program:\n$stringBuilder")
    }
  }
  override def toString: String = {
    val builder = new StringBuilder
    globalInits.forEach((init: InitStmt) => builder.append(init).append("\n"))
    functions.forEach((function: FlowGraph.Function) => builder.append(function.toString))
    builder.toString
  }
}
