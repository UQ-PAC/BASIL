package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.{IterableOnceExtensionMethods, View, immutable, mutable}
import boogie.*
import analysis.BitVectorEval
import util.intrusive_list.*

class Program(var procedures: ArrayBuffer[Procedure], var mainProcedure: Procedure,
              var initialMemory: ArrayBuffer[MemorySection],
              var readOnlyMemory: ArrayBuffer[MemorySection]) extends Iterable[CFGPosition] {

  // This shouldn't be run before indirect calls are resolved
  def stripUnreachableFunctions(depth: Int = Int.MaxValue): Unit = {
    val procedureCalleeNames = procedures.map(f => f.name -> f.calls.map(_.name)).toMap

    val toVisit: mutable.LinkedHashSet[(Int, String)] = mutable.LinkedHashSet((0, mainProcedure.name))
    var reachableFound = true
    val reachableNames = mutable.HashMap[String, Int]()
    while (toVisit.nonEmpty) {
      val next = toVisit.head
      toVisit.remove(next)

      if (next._1 <= depth) {

        def addName(depth: Int, name: String): Unit = {
          val oldDepth = reachableNames.getOrElse(name, Integer.MAX_VALUE)
          reachableNames.put(next._2, if depth < oldDepth then depth else oldDepth)
        }
        addName(next._1, next._2)

        val callees = procedureCalleeNames(next._2)

        toVisit.addAll(callees.diff(reachableNames.keySet).map(c => (next._1 + 1, c)))
        callees.foreach(c => addName(next._1 + 1, c))
      }
    }
    procedures = procedures.filter(f => reachableNames.keySet.contains(f.name))

    for (elem <- procedures.filter(c => c.calls.exists(s => !procedures.contains(s)))) {
      // last layer is analysed only as specifications so we remove the body for anything that calls
      // a function we have removed

      elem.clearBlocks()
    }
  }

  def setModifies(specModifies: Map[String, List[String]]): Unit = {
    val procToCalls: mutable.Map[Procedure, Set[Procedure]] = mutable.Map()
    for (p <- procedures) {
      p.modifies.addAll(p.blocks.flatMap(_.modifies))
      procToCalls(p) = p.calls
    }

    for (p <- procedures) {
      if (specModifies.contains(p.name)) {
        p.modifies.addAll(specModifies(p.name).map(nameToGlobal))
      }
    }

    // very naive implementation but will work for now
    var hasChanged: Boolean = true
    while (hasChanged) {
      hasChanged = false
      for (p <- procedures) {
        val children = procToCalls(p)
        val childrenModifies: mutable.Set[Global] = mutable.Set()
        for (c <- children) {
          childrenModifies.addAll(c.modifies)
        }
        if (!childrenModifies.subsetOf(p.modifies)) {
          hasChanged = true
          p.modifies.addAll(childrenModifies)
        }
      }
    }
  }

  // this is very crude but the simplest thing for now until we have a more sophisticated specification system that can relate to the IR instead of the Boogie
  def nameToGlobal(name: String): Global = {
    if ((name.startsWith("R") || name.startsWith("V")) && (name.length == 2 || name.length == 3)
      && name.substring(1).forall(_.isDigit)) {
      if (name.startsWith("R")) {
        Register(name, 64)
      } else {
        Register(name, 128)
      }
    } else if (name == "stack") {
      StackMemory(name, 64, 8)
    } else {
      SharedMemory(name, 64, 8)
    }
  }

  /**
    * Takes all the memory sections we get from the ADT (previously in initialMemory) and restricts initialMemory to
    * just the .data section (which contains things such as global variables which are mutable) and puts the .rodata
    * section in readOnlyMemory. It also takes the .rela.dyn entries taken from the readelf output and adds them to the
    * .rodata section, as they are the global offset table entries that we can assume are constant.
    */
  def determineRelevantMemory(rela_dyn: Map[BigInt, BigInt]): Unit = {
    val initialMemoryNew = ArrayBuffer[MemorySection]()

    val rodata = initialMemory.collect { case s if s.name == ".rodata" => s }
    readOnlyMemory.addAll(rodata)

    val data = initialMemory.collect { case s if s.name == ".data" => s }
    initialMemoryNew.addAll(data)

    // assuming little endian, adding the rela_dyn offset/address pairs like this is crude but is simplest for now
    for ((offset, address) <- rela_dyn) {
      val addressBV = BitVecLiteral(address, 64)
      val bytes = for (i <- 0 to 7) yield {
        val low = i * 8
        val high = low + 8
        BitVectorEval.boogie_extract(high, low, addressBV)
      }
      readOnlyMemory.append(MemorySection(s".got_$offset", offset.intValue, 8, bytes))
    }

    initialMemory = initialMemoryNew
  }

  /**
   * Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are 
   * not guaranteed to be in any defined order. 
   */
  private class ILUnorderedIterator(private val begin: Program) extends Iterator[CFGPosition] {
    private val stack = mutable.Stack[CFGPosition]()
    stack.addAll(begin.procedures)

    override def hasNext: Boolean = {
      stack.nonEmpty
    }

    override def next(): CFGPosition = {
      val n: CFGPosition = stack.pop()

      stack.pushAll(n match {
        case p: Procedure => p.blocks
        case b: Block => Seq() ++ b.statements ++ Seq(b.jump) ++ b.fallthrough.toSet
        case s: Command => Seq()
      })
      n
    }

  }

  /**
   * Get an Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are 
   * not guaranteed to be in any defined order. 
   */
  def iterator: Iterator[CFGPosition] = {
    ILUnorderedIterator(this)
  }

}

class Procedure private (
                  var name: String,
                  var address: Option[Int],
                  private var _entryBlock: Option[Block],
                  private var _returnBlock: Option[Block],
                  private val _blocks: mutable.LinkedHashSet[Block],
                  var in: ArrayBuffer[Parameter],
                  var out: ArrayBuffer[Parameter],
                ) {
  private val _callers = mutable.HashSet[DirectCall]()
  _blocks.foreach(_.parent = this)
  // class invariant
  require(_returnBlock.forall(b => _blocks.contains(b)) && _entryBlock.forall(b => _blocks.contains(b)))
  require(_blocks.isEmpty == _entryBlock.isEmpty) // blocks.nonEmpty <==> entryBlock.isDefined

  def this(name: String, address: Option[Int] = None , entryBlock: Option[Block] = None, returnBlock: Option[Block] = None, blocks: Iterable[Block] = ArrayBuffer(), in: IterableOnce[Parameter] = ArrayBuffer(), out: IterableOnce[Parameter] = ArrayBuffer()) = {
    this(name, address, entryBlock, returnBlock, mutable.LinkedHashSet.from(blocks), ArrayBuffer.from(in), ArrayBuffer.from(out))
  }

  override def toString: String = {
    s"Procedure $name at ${address.getOrElse("None")} with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }

  def calls: Set[Procedure] = blocks.iterator.flatMap(_.calls).toSet

  /**
   * Block iteration order is defined such that that the entryBlock is first, and no order is defined beyond that.
   * Both entry block and return block are elements of _blocks.
   */
  def blocks: Iterator[Block] = _blocks.iterator

  def addCaller(c: DirectCall): Unit = {
    _callers.add(c)
  }

  def removeCaller(c: DirectCall): Unit = {
    _callers.remove(c)
  }

  def returnBlock: Option[Block] = _returnBlock

  def returnBlock_=(value: Block): Unit = {
    if (!returnBlock.contains(value)) {
      removeBlocks(_returnBlock)
      _returnBlock = Some(addBlocks(value))
    }
  }

  def entryBlock: Option[Block] = _entryBlock

  def entryBlock_=(value: Block): Unit = {
    if (!entryBlock.contains(value)) {
      removeBlocks(_entryBlock)
      _entryBlock = Some(addBlocks(value))
    }
  }

  def addBlocks(block: Block): Block = {
    block.parent = this
    if (!_blocks.contains(block)) {
      block.parent = this
      _blocks.add(block)
      if (entryBlock.isEmpty) {
        entryBlock = block
      }
    }
    block
  }

  def addBlocks(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      addBlocks(elem)
    }
  }

  def replaceBlock(oldBlock: Block, block: Block): Block = {
    require(_blocks.contains(oldBlock))
    if (oldBlock ne block) {
      val isEntry: Boolean = entryBlock.contains(oldBlock)
      val isReturn: Boolean = returnBlock.contains(oldBlock)
      val incoming = oldBlock.incomingJumps
      removeBlocksDisconnect(oldBlock)
      addBlocks(block)
      for (elem <- incoming) {
        elem.addTarget(block)
      }
      if isEntry then entryBlock = block
      if isReturn then returnBlock = block
    }
    block
  }

  /**
   * Removes all blocks and replaces them with the provided iterator.
   *
   * @param newBlocks the new set of blocks
   * @return an iterator to the new block set
   */
  def replaceBlocks(newBlocks: Iterable[Block]): Unit = {
    clearBlocks()
    addBlocks(newBlocks)
  }

  /**
   * Removes a block assuming no existing blocks jump to it.
   * @param block the block to remove
   * @return the removed block
   */
  def removeBlocks(block: Block): Block = {
    require(block.incomingJumps.isEmpty) // don't leave jumps dangling
    if (_blocks.contains(block)) {
      block.deParent()
      _blocks.remove(block)
    }
    if (_entryBlock.contains(block)) {
      _entryBlock = None
    }
    if (_returnBlock.contains(block)) {
      _returnBlock = None
    }
    block
  }

  /**
   * Remove blocks with the semantics of replacing them with a noop. The incoming jumps to this are replaced
   * with a jump(s) to this blocks jump target(s). If this block ends in a call then only its statements are removed.
   * @param blocks the block/blocks to remove
   */
  def removeBlocksInline(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      elem.jump match {
        case g: GoTo =>
          // rewrite all the jumps to include our jump targets
          elem.incomingJumps.foreach(_.removeTarget(elem))
          elem.incomingJumps.foreach(_.addAllTargets(g.targets))
          removeBlocks(elem)
        case c: Call =>
          // just remove statements, keep call
          elem.statements.clear()
      }
    }
  }


  def removeBlocksInline(blocks: Block*): Unit = {
    removeBlocksInline(blocks.toSeq)
  }

  /**
   * Remove block(s) and all jumps that target it
   * @param blocks the blocks to remove
   */
  def removeBlocksDisconnect(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      for (j <- elem.incomingJumps) {
        j.removeTarget(elem)
      }
      removeBlocks(elem)
    }
  }

  def removeBlocksDisconnect(blocks: Block*): Unit = {
    removeBlocksDisconnect(blocks.toSeq)
  }
  

  def removeBlocks(blocks: IterableOnce[Block]): Unit = {
    for (elem <- blocks.iterator) {
      removeBlocks(elem)
    }
  }

  def clearBlocks() : Unit = {
    // O(n) because we are careful to unlink the parents etc.
    removeBlocks(_blocks)
  }

  def callers(): Iterable[Procedure] = _callers.map(_.parent.parent).toSet[Procedure]
  def incomingCalls(): Iterator[DirectCall] = _callers.iterator

  var modifies: mutable.Set[Global] = mutable.Set()
}

class Parameter(var name: String, var size: Int, var value: Register) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}



class Block private (
 val label: String,
 val address: Option[Int],
 val statements: IntrusiveList[Statement],
 private var _jump: Jump,
 private val _incomingJumps: mutable.HashSet[GoTo],
 var _fallthrough: Option[GoTo],
) extends HasParent[Procedure] {
  _jump.setParent(this)
  statements.foreach(_.setParent(this))

  statements.onInsert = x => x.setParent(this)
  statements.onRemove = x => x.deParent()


  def this(label: String, address: Option[Int] = None, statements: IterableOnce[Statement] = Set.empty, jump: Jump = GoTo(Set.empty)) = {
    this(label, address, IntrusiveList.from(statements), jump,  mutable.HashSet.empty, None)
  }

  def jump: Jump = _jump

  def fallthrough: Option[GoTo] = _fallthrough

  def fallthrough_=(g: Option[GoTo]): Unit = {
    /*
     * Fallthrough is only set if Jump is a call, this is maintained maintained at the 
     * linkParent implementation on FallThrough of Call.
     */
    _fallthrough.foreach(_.deParent())
    g.foreach(x => x.parent = this)
    _fallthrough = g
  }

  def jump_=(j: Jump): Unit = {
    if (j ne _jump) {
      _jump.deParent()
      _jump = j
      _jump.parent = this
    }
  }

  def replaceJump(j: Jump): Block = {
    jump = j
    this
  }

  def incomingJumps: immutable.Set[GoTo] = _incomingJumps.toSet

  def addIncomingJump(g: GoTo): Boolean = _incomingJumps.add(g)
  
  def removeIncomingJump(g: GoTo): Unit = {
    _incomingJumps.remove(g)
    assert(!incomingJumps.contains(g))
  }

  def calls: Set[Procedure] = _jump.calls

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  def calledBy: Set[Block] = {
    Set.empty
  }

  override def toString: String = {
    val statementsString = statements.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jump"
  }

  /**
   * @return The intra-procedural set of successor blocks. If the block ends in a call then the empty set is returned.
   */
  def nextBlocks: Iterable[Block] = {
    jump match {
      case c: GoTo => c.targets
      case c: Call => fallthrough match {
        case Some(x) => x.targets
        case _ => Seq()
      }
    }
  }

  /**
   * @return The intra-procedural set of predecessor blocks.
   */
  def prevBlocks: Iterable[Block] = {
    incomingJumps.map(_.parent)
  }

  /**
   * If the block has a single block successor then this returns that block, otherwise None.
   *
   * @return The successor block if there is exactly one
   */
  def singleSuccessor: Option[Block] = {
    jump match {
      case c: GoTo if c.targets.size == 1 => c.targets.headOption
      case _ => None
    }
  }

  /**
   * If the block has a single block predecessor then this returns that block, otherwise None.
   *
   * @return The predecessor block if there is exactly one
   */
  def singlePredecessor: Option[Block] = {
    if incomingJumps.size == 1 then {
      incomingJumps.headOption.map(_.parent)
    } else None
  }

  override def linkParent(p: Procedure): Unit = {
    // to connect call() links that reference jump.parent.parent
    jump.setParent(this)
  }

  override def unlinkParent(): Unit = {
    // to disconnect call() links that reference jump.parent.parent
    jump.deParent()
  }
}

object Block {
  def procedureReturn(from: Procedure): Block = {
    Block(from.name + "_basil_return", None, List(), IndirectCall(Register("R30", BitVecType(64))))
  }
}

/**
  * @param name name
  * @param address initial offset of memory section
  * @param size number of bytes
  * @param bytes sequence of bytes represented by BitVecLiterals of size 8
  */
case class MemorySection(name: String, address: Int, size: Int, bytes: Seq[BitVecLiteral])
