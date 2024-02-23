package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.{IterableOnceExtensionMethods, View, immutable, mutable}
import boogie.*
import analysis.BitVectorEval
import intrusivelist.{IntrusiveList, IntrusiveListElement}

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
  class ILUnorderedIterator(private val begin: Program) extends Iterator[CFGPosition] {
    private val stack = mutable.Stack[CFGPosition]()
    stack.addAll(begin.procedures)

    override def hasNext: Boolean = {
      stack.nonEmpty
    }

    override def next(): CFGPosition = {
      val n: CFGPosition  = stack.pop()

      stack.pushAll(n match {
        case p: Procedure => p.blocks
        case b: Block => Seq() ++ b.statements ++ Seq(b.jump)
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
                  var entryBlock: Option[Block],
                  var returnBlock: Option[Block],
                  private val _blocks: mutable.LinkedHashSet[Block],
                  var in: ArrayBuffer[Parameter],
                  var out: ArrayBuffer[Parameter],
                ) {
  private val _callers = mutable.HashSet[DirectCall]()
  _blocks.foreach(_.setParent(this))
  // class invariant
  require(returnBlock.forall(b => _blocks.contains(b)) && entryBlock.forall(b => _blocks.contains(b)))
  require(_blocks.isEmpty == entryBlock.isEmpty) // blocks.nonEmpty <==> entryBlock.isDefined

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
    _callers.remove(c)
  }

  def removeCaller(c: DirectCall): Unit = {
    _callers.remove(c)
  }

  def addBlocks(block: Block): Block = {
    if (!_blocks.contains(block)) {
      block.parent = this
      _blocks.add(block)
      if (entryBlock.isEmpty) {
        entryBlock = Some(block)
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
      removeBlocks(oldBlock)
      addBlocks(block)
      if isEntry then entryBlock = Some(block)
      if isReturn then returnBlock = Some(block)
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

  def removeBlocks(block: Block): Block = {
    if (_blocks.contains(block)) {
      block.deParent()
      _blocks.remove(block)
    }
    if (_blocks.isEmpty) {
      entryBlock = None
    }
    block
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


class Block private (var label: String,
 var address: Option[Int],
 val statements: IntrusiveList[Statement],
 private var _jump: Jump,
 private val _incomingJumps: mutable.HashSet[GoTo],
) extends IntrusiveListElement, HasParent[Procedure] {
  statements.foreach(_.setParent(this))
  _jump.setParent(this)

  statements.onInsert = x => x.setParent(this)
  statements.onRemove = x => x.deParent()

  def this(label: String, address: Option[Int], statements: IterableOnce[Statement], jump: Jump) = {
    this(label, address, IntrusiveList.from(statements), jump, mutable.HashSet.empty)
  }

  def this(label: String, address: Option[Int], statements: IterableOnce[Statement]) = {
    this(label, address, IntrusiveList.from(statements), GoTo(Seq(), Some(label + "_unknown")), mutable.HashSet.empty)
  }

  def this(label: String, address: Option[Int], statements: IntrusiveList[Statement]) = {
    this(label, address, statements, GoTo(Seq(), Some(label + "_unknown")), mutable.HashSet.empty)
  }

  def this(label: String, address: Option[Int] = None) = {
    this(label, address, IntrusiveList(), GoTo(Seq(), Some(label + "_unknown")), mutable.HashSet.empty)
  }

  def jump: Jump = _jump

  def incomingJumps: immutable.Set[GoTo] = _incomingJumps.toSet

  def addIncomingJump(g: GoTo): Boolean = _incomingJumps.add(g)
  def removeIncomingJump(g: GoTo): Boolean = _incomingJumps.remove(g)

  def replaceJump(j: Jump): this.type = {
    _jump.deParent()
    j.setParent(this)
    _jump = j
    this
  }

  def isEntry: Boolean = parent.entryBlock.contains(this)
  def isReturn: Boolean = parent.returnBlock.contains(this)

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
      case c: DirectCall => c.returnTarget
      case c: IndirectCall => c.returnTarget
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
    // The first block added to the procedure is the entry block
    if parent.blocks.isEmpty then parent.entryBlock = Some(this)
    // to connect call() links that reference jump.parent.parent
    jump.setParent(this)
  }
  override def unlinkParent(): Unit = {
    if parent.entryBlock.contains(this) then parent.entryBlock = None
    if parent.returnBlock.contains(this) then parent.returnBlock = None
    // to disconnect call() links that reference jump.parent.parent
    jump.deParent()
  }
}

/**
  * @param name name
  * @param address initial offset of memory section
  * @param size number of bytes
  * @param bytes sequence of bytes represented by BitVecLiterals of size 8
  */
case class MemorySection(name: String, address: Int, size: Int, bytes: Seq[BitVecLiteral])
