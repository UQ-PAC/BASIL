/*
package translating

import astnodes._
import vcgen.Block

import scala.collection.mutable.StringBuilder
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue
import util.AssumptionViolationException

import scala.collection.mutable
*/

// TODO neaten scala code
/*
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
  def fromStmts(stmts: List[Statement], types: Map[String, Int]): FlowGraph = {
    val flowGraph = FlowGraphFactory.fromStmts(stmts, types)
    flowGraph.enforceDisjointFunctions()
    flowGraph
  }

  case class Function(header: EnterSub, blocks: List[Block]) {
    // TODO should automatically generate these
    def initStmts: List[InitStmt] = List(InitStmt(LocalVar("ZF", -1), "ZF", "bv1"),
      InitStmt(LocalVar("CF", -1), "CF", "bv1"),
      InitStmt(LocalVar("NF", -1), "NF", "bv1"),
      InitStmt(LocalVar("VF", -1), "VF", "bv1")
    )

    def lines: List[Statement] = blocks flatMap { block => block.lines }

    override def toString: String = {
      val builder = mutable.StringBuilder()
      builder.append(header).append("\n")
      initStmts.foreach((stmt: InitStmt) => builder.append(stmt).append("\n"))
      for (block <- blocks) builder.append(block)
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
    def fromStmts(stmts: List[Statement], types: Map[String, Int]): FlowGraph = {
      val stmts2 = mergeCjmp(stmts)
      // val stmts2 = setFunctionsWithReturns(stmts1);
      //val stmts2 = stmts1
      // an ordered list of indexes of the given statements list, indicating where the list should be split into blocks
      val splits = getSplits(stmts2)
      // an ordered list of blocks, defined by the given given list of splits
      // essentially creates the nodes for this flow graph
      val blocks = createBlocksFromSplits(splits, stmts2)
      val blocks2 = stripBlocks(blocks)
      // links each block in the list to each other block that may follow this one (e.g. via a jump)
      // essentially creates the edges for this flow graph
      val blocks3 = setChildren(blocks2, stmts2)
      // create a flow graph consisting of the functions formed from these blocks
      val flowGraph = new FlowGraph(convertBlocksToFunctions(blocks3), types)
      // ensure the created flow graph maintains the required properties
      flowGraph.enforceConstraints()

      flowGraph
    }

    private def setFunctionsWithReturns(stmts: List[Statement]): List[Statement] = {
      var i = 0 // TODO work out a way to not use this

      // TODO ideally could collect (returning a partial function instead of some/none)
      (stmts.sliding(3, 1).flatMap{
        case (call: CallStmt) :: _ :: (assign: LocalAssign) :: Nil =>
          call.copy(lhs = Some(assign.lhs))
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

    private def findFunction(blocks: List[Block], functionName: String): Block = {
      blocks.find {
        _.lines.head match {
          case e: EnterSub => e.funcName == functionName
        }
      }
    }.get

    private def stripBlocks(blocks: List[Block]): List[Block] = {
      val mainBlock = blocks.find(_.lines.head match {
        case enterSub: EnterSub => enterSub.funcName == "main"
        case _ => false
      }).get

      val reachableBlocks: ArrayBuffer[Block] = ArrayBuffer()
      val queue = mutable.Queue(mainBlock)

      while (queue.nonEmpty) {
        val block = queue.dequeue()
        if (!reachableBlocks.contains(block)) {
          reachableBlocks += block

          // TODO can look just at the last line
          block.lines.foreach {
            case jmp: JmpStmt =>
              queue += findBlockStartingWith(jmp.target, blocks).get
            case cjmp: CJmpStmt =>
              queue += findBlockStartingWith(cjmp.trueTarget, blocks).get
              queue += findBlockStartingWith(cjmp.falseTarget, blocks).get
            case call: CallStmt =>
              if (!call.libraryFunction) queue += findFunction(blocks, call.funcName)
              if (call.returnTarget.isDefined) queue += findBlockStartingWith(call.returnTarget.get, blocks).get
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
    private def getSplits(stmts: List[Statement]): Vector[Int] = { // we use a set to avoid double-ups, as some lines may be jumped to twice
      val splits: mutable.HashSet[Int] = mutable.HashSet()
      for (i <- stmts.indices) {
        stmts(i) match {
          case jmpStmt: JmpStmt =>
            splits += (i + 1)
            findInstWithPc(jmpStmt.target, stmts) match {
              case Some(targetIndex) => splits += targetIndex
              case None =>
            }
          case jmpStmt: CJmpStmt =>
            splits += (i + 1) // TODO check if we need this
            findInstWithPc(jmpStmt.trueTarget, stmts) match {
              case Some(trueTargetIndex) => splits += trueTargetIndex
              case None =>
            }
            findInstWithPc(jmpStmt.falseTarget, stmts) match {
              case Some(falseTargetIndex) => splits += falseTargetIndex
              case None =>
            }
          case _: CallStmt => splits += (i + 1)
          case _: EnterSub => splits += i
          case _: ExitSub => splits += (i + 1)
          case _ =>
        }
      }
      // ensure there is a split at the start and end of the program
      splits += 0
      splits += stmts.size
      splits.toVector.sorted
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
    private def findInstWithPc(pc: String, stmts: List[Statement]): Option[Int] = {
      if (pc.substring(0, 2) == "__") return None // TODO when jumping to a function e.g. goto @__gmon_start__
      for (i <- stmts.indices) {
        if (stmts(i).pc == pc) return Some(i)
      }
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
    private def createBlocksFromSplits(splits: Vector[Int], lines: List[Statement]): List[Block] = {
      for (i <- 0 until splits.size - 1) yield {
        val blockLines = lines.slice(splits(i), splits(i + 1))
        // blocks are initially created with no children
        Block(blockLines.head.pc, blockLines, List())
      }
    }.toList

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
    private def setChildren(blocks: List[Block], stmts: List[Statement]): List[Block] = {
      for (block <- blocks) yield {
        // the PCs of all statements this block jumps to (for instance, the targets of jumps)
        val childrenPcs = getChildrenPcs(block, stmts)
        // tries to find a block in the list with a first line that has the child's PC
        val children = for (childPc <- childrenPcs) yield {
          findBlockStartingWith(childPc, blocks) match {
            case Some(child) => child
            // no such block was found, which means there is no block defined for this jump/cjump etc.
            case None => throw new AssumptionViolationException(
              s"Error creating flow graph. Could not find a block starting with target pc '$childPc'."
            )
          }
        }
        block.copy(children = children)
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
    private def getChildrenPcs(block: Block, lines: List[Statement]): List[String] = {
      block.lines.last match {
        case jmp: JmpStmt => List(jmp.target)
        case cjmp: CJmpStmt => List(cjmp.trueTarget, cjmp.falseTarget)
        // TODO case exitSub: ExitSub => List(lines.get(lines.indexOf(exitSub) + 1).getLabel.pc)
        case callStmt: CallStmt => callStmt.returnTarget.toList
        case _: ExitSub => List()
      }
    }

    // TODO this and the stripping could happen at the same time I think
    private def convertBlocksToFunctions(blocks: List[Block]): List[FlowGraph.Function] = {
      val functions = blocks flatMap {
        block => block.lines.head match {
          case firstLine: EnterSub =>
            val blockLineStrip = block.copy(lines = block.lines.tail)
            val blocksInFunction = dfsOnBlock(mutable.HashSet(), blockLineStrip)
            Some(FlowGraph.Function(firstLine, blocksInFunction.toList))
          case _ => None
        }
      }
      functions
    }

    private def dfsOnBlock(blocks: mutable.HashSet[Block], next: Block): mutable.HashSet[Block] = {
      blocks.add(next)
      for (child <- next.children) {
        if (!blocks.contains(child)) {
          dfsOnBlock(blocks, child)
        }
      }
      blocks
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
    private def findBlockStartingWith(pc: String, blocks: List[Block]): Option[Block] = {
      for (block <- blocks) {
        if (block.lines.head.pc == pc) {
          return Some(block)
        }
      }
      None
    }

    private def mergeCjmp (stmts: List[Statement]): List[Statement] = stmts match {
      case (cjmp: CJmpStmt) :: (jmp: JmpStmt) :: rest => cjmp.copy(falseTarget = jmp.target) +: mergeCjmp(rest)
      case (cjmp: CJmpStmt) :: _ => throw new AssumptionViolationException("Unexpected conditional jump")
      case stmt :: rest => stmt +: mergeCjmp(rest)
      case Nil => Nil
    }

  }
}

case class FlowGraph (functions: List[FlowGraph.Function], types: Map[String, Int]) {
  def globalInits: List[InitStmt] = List(InitStmt(LocalVar("heap", -1), "heap", "[bv64] bv8"), // TODO label.none
    InitStmt(LocalVar("heap_free", -1), "heap_free", "[bv64] bool"),
    InitStmt(LocalVar("heap_sizes", -1), "heap_size", "[bv64] bv64"),
    InitStmt(LocalVar("stack", -1), "stack", "[bv64] bv8"),
    InitStmt(LocalVar("SP", -1), "SP", "bv64"),
    InitStmt(LocalVar("R31", -1), "R31", "bv64")
  )

  /** @return
    *   all lines of all blocks within this flow graph
    */
  def lines: List[Statement] = blocks flatMap { block => block.lines}

  /** @return
    *   all blocks within this flow graph
    */
  def blocks: List[Block] = functions flatMap { function => function.blocks }

  //def removeLine(line: Stmt): Unit = getBlocks.forEach((block: FlowGraph.Block) => block.lines.remove(line))

  /*
  def replaceLine(newStmt: Stmt, oldStmt: Stmt): Unit = {
    getBlocks.forEach(block => {
      if (block.lines.contains(oldStmt)) {
        val index = block.getLines.indexOf(oldStmt)
        block.replaceStmt(newStmt, index)
      }
    })
  }
  */

  /** Enforce guaranteed properties of this flow graph. Exceptions are thrown when these constraints are not met.
    */
  def enforceConstraints(): Unit = {
    enforceDisjointFunctions()
    enforceUniqueLines()
  }

  /** No block should be accessible (directly or indirectly) by two different function blocks.
    */
  private def enforceDisjointFunctions(): Unit = {
    val usedBlocks = functions flatMap { _.blocks }
    val duplicates = usedBlocks.groupBy(identity).collect { case (x, ys) if ys.lengthCompare(1) > 0 => x }
    if (duplicates.nonEmpty) {
      val stringBuilder = mutable.StringBuilder()
      duplicates.foreach(stringBuilder.append)
      throw new AssumptionViolationException(
        s"Flow graph error. The following blocks can be accessed by two different functions:\n$stringBuilder")
    }
  }

  /** A complete traversal of a flow graph should encounter no line twice, or no line with the same PC twice.
    */
  private def enforceUniqueLines(): Unit = {
    val pcList = lines map {line => line.pc}
    if (lines.distinct.size != lines.size) {
      val duplicateLines = lines.groupBy(identity).collect { case (x, ys) if ys.lengthCompare(1) > 0 => x }
      val stringBuilder = mutable.StringBuilder()
      duplicateLines.foreach(stringBuilder.append)
      throw new AssumptionViolationException(
        s"Flow graph error. The following lines were found twice throughout the program:\n$stringBuilder")
    }
    if (pcList.distinct.size != pcList.size) {
      val duplicates = pcList.groupBy(identity).collect { case (x, ys) if ys.lengthCompare(1) > 0 => x }
      val stringBuilder = mutable.StringBuilder()
      duplicates.foreach(stringBuilder.append)
      throw new AssumptionViolationException(
        s"Flow graph error. The following lines were found twice throughout the program:\n$stringBuilder")
    }
  }

  override def toString: String = {
    val builder = new mutable.StringBuilder
    globalInits.foreach(init => builder.append(init).append("\n"))
    functions.foreach(builder.append)
    builder.toString
  }
}
*/