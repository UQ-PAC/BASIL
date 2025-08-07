package ir

import analysis.{Loop, MergedRegion}
import boogie.*
import translating.PrettyPrinter.*
import util.assertion.*
import util.functional.Snoc
import util.intrusive_list.*

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

import eval.BitVectorEval

/** Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are not
  * guaranteed to be in any defined order.
  */
private class ILForwardIterator(
  private val begin: IterableOnce[CFGPosition],
  val walk: IRWalk[CFGPosition, CFGPosition]
) extends Iterator[CFGPosition] {
  val seen = mutable.Set[CFGPosition]()
  private val stack = mutable.Stack[CFGPosition]()
  stack.pushAll(begin)
  seen.addAll(begin)

  override def hasNext: Boolean = {
    stack.nonEmpty
  }

  override def next(): CFGPosition = {
    val n: CFGPosition = stack.pop()
    seen.add(n)

    val next = walk.succ(n).filterNot(seen.contains(_))
    seen.addAll(next)
    stack.pushAll(next)
    n
  }
}

/** Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are not
  * guaranteed to be in any defined order.
  */
private class ILLexicalIterator(private val begin: Iterable[CFGPosition]) extends Iterator[CFGPosition] {
  private val stack = mutable.Stack[CFGPosition]()
  stack.pushAll(begin)

  override def hasNext: Boolean = {
    stack.nonEmpty
  }

  override def next(): CFGPosition = {
    val n: CFGPosition = stack.pop()

    stack.pushAll(n match {
      case p: Procedure => p.blocks
      case b: Block => b.statements ++ Iterator(b.jump)
      case s: Command => Seq()
    })
    n
  }
}

case class Metadata(originalLabel: Option[String] = None, address: Option[BigInt] = None)

class Program(
  var procedures: ArrayBuffer[Procedure],
  var mainProcedure: Procedure,
  val initialMemory: mutable.TreeMap[BigInt, MemorySection],
  val declarations: mutable.ArrayBuffer[Decl] = mutable.ArrayBuffer()
) extends Iterable[CFGPosition]
    with DeepEquality {

  val threads: ArrayBuffer[ProgramThread] = ArrayBuffer()
  val usedMemory: mutable.Map[BigInt, MemorySection] = mutable.TreeMap()

  override def deepEquals(o: Object): Boolean = o match {
    case p: Program => deepEqualsProg(p)
    case _ => false
  }
  private def deepEqualsProg(p: Program): Boolean = {
    def toMap(p: Program) = {
      p.procedures.view.map(p => p.name -> p).toMap
    }
    val t = toMap(this)
    val o = toMap(p)
    (mainProcedure.name == p.mainProcedure.name) && (t.keys == o.keys) && t.keys.forall { case k =>
      t(k).deepEquals(o(k))
    }
  }

  def removeProcedure(i: Int): Unit = {
    val p = procedures(i)
    for (b <- p.blocks) {
      b.deParent()
    }
    procedures.remove(i)
  }

  def removeProcedure(p: Procedure): Unit = {
    removeProcedure(procedures.indexOf(p))
  }

  def addProcedure(p: Procedure) = {
    for (b <- p.blocks) {
      b.setParent(p)
    }
    procedures += p
  }

  def sortProceduresRPO() = {

    var count = 0
    val seen = mutable.HashSet[Procedure]()
    val ordering = mutable.HashMap[Procedure, Int]()

    def walk(p: Procedure): Unit = {
      seen += p
      for (n <- p.calls) {
        if (!seen.contains(n)) {
          walk(n)
        }
      }
      ordering(p) = count
      count += 1
    }

    walk(mainProcedure)

    val wl = mutable.TreeSet.from(procedures)(Ordering.by(x => (x.procName, x.address)))
    wl --= seen

    while (wl.nonEmpty) {
      // add the rest of the procedures
      val n = wl.find(p => p.incomingCalls().isEmpty).getOrElse(wl.head)
      walk(n)
      wl --= seen
    }

    procedures.sortInPlaceBy(ordering)
  }

  override def toString(): String = {
    translating.PrettyPrinter.pp_prog(this)
  }

  def setModifies(specModifies: Map[String, List[String]]): Unit = {
    val procToCalls: mutable.Map[Procedure, Set[Procedure]] = mutable.Map()
    for (p <- procedures) {
      p.modifies.addAll(p.blocks.flatMap(_.modifies))
      procToCalls(p) = p.calls
    }

    for (p <- procedures) {
      if (specModifies.contains(p.procName)) {
        p.modifies.addAll(specModifies(p.procName).map(nameToGlobal))
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
    if (
      (name.startsWith("R") || name.startsWith("V")) && (name.length == 2 || name.length == 3)
      && name.substring(1).forall(_.isDigit)
    ) {
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

  /** Takes all the memory sections we get from the ADT (previously in initialMemory) and restricts initialMemory to
    * just the .data section (which contains things such as global variables which are mutable) and puts the .rodata
    * section in readOnlyMemory. It also takes the .rela.dyn entries taken from the readelf output and adds them to the
    * .rodata section, as they are the global offset table entries that we can assume are constant.
    */

  def determineRelevantMemory(rela_dyn: Map[BigInt, BigInt]): Unit = {
    val rodata = initialMemory.values.collect { case s if s.name == ".rodata" => s }
    rodata.foreach { r => usedMemory.addOne(r.address, r) }
    val data = initialMemory.values.collect { case s if s.name == ".data" => s }
    data.foreach { d => usedMemory.addOne(d.address, d) }

    // assuming little endian, adding the rela_dyn offset/address pairs like this is crude but is simplest for now
    for ((offset, address) <- rela_dyn) {
      val addressBV = BitVecLiteral(address, 64)
      val bytes = for (i <- 0 to 7) yield {
        val low = i * 8
        val high = low + 8
        BitVectorEval.boogie_extract(high, low, addressBV)
      }
      usedMemory.addOne(offset, MemorySection(s".got_$offset", offset, 8, bytes, true, None))
    }

  }

  /** Get an Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are
    * not guaranteed to be in any defined order.
    */
  def iterator: Iterator[CFGPosition] = {
    ILLexicalIterator(this.procedures)
  }

  def preOrderIterator: Iterator[CFGPosition] = {
    ILForwardIterator(this.procedures, IntraProcIRCursor)
  }

  def memoryLookup(memory: mutable.TreeMap[BigInt, MemorySection], address: BigInt) = {
    memory.maxBefore(address + 1) match {
      case Some(_, section) =>
        if (section.address + section.size > address) {
          Some(section)
        } else {
          None
        }
      case _ => None
    }
  }

  def initialMemoryLookup(address: BigInt): Option[MemorySection] = memoryLookup(initialMemory, address)

  def nameToProcedure: Map[String, Procedure] = {
    procedures.view.map(p => p.procName -> p).toMap
  }

  def labelToBlock: Map[String, Block] = {
    procedures.view.flatMap(_.blocks.map((b: Block) => b.label -> b)).toMap
  }

  def loadedLabelToBlock: Map[String, Block] = {
    val x = procedures.view.flatMap(_.loadedLabelToBlock).toMap
    x
    // blocks.filter(_.meta.originalLabel.isDefined).map(p => p.meta.originalLabel.get -> p).toMap
  }
}

// if creationSite == None then it is the initial thread
class ProgramThread(
  val entry: Procedure,
  val procedures: mutable.LinkedHashSet[Procedure],
  val creationSite: Option[DirectCall]
)

/*
 * R0 := call procname(R0, R1, R2)
 *
 * procname (R0a, R1a, R2a):
 *  ...
 *  return (R0)
 *
 */

class Procedure private (
  var procName: String,
  var address: Option[BigInt],
  private var _entryBlock: Option[Block],
  private var _returnBlock: Option[Block],
  private val _blocks: mutable.ArrayBuffer[Block],
  var formalInParam: mutable.SortedSet[LocalVar],
  var formalOutParam: mutable.SortedSet[LocalVar],
  var inParamDefaultBinding: immutable.SortedMap[LocalVar, Expr],
  var outParamDefaultBinding: immutable.SortedMap[LocalVar, Variable],
  var requires: List[BExpr],
  var ensures: List[BExpr],
  var requiresExpr: List[Expr],
  var ensuresExpr: List[Expr]
) extends Iterable[CFGPosition]
    with DeepEquality {

  def name = procName + address.map("_" + _).getOrElse("")

  private val _callers = mutable.HashSet[DirectCall]()
  _blocks.foreach(_.parent = this)
  // class invariant
  debugAssert(_returnBlock.forall(b => _blocks.contains(b)) && _entryBlock.forall(b => _blocks.contains(b)))
  debugAssert(_blocks.isEmpty == _entryBlock.isEmpty) // blocks.nonEmpty <==> entryBlock.isDefined

  def this(
    name: String,
    address: Option[BigInt] = None,
    entryBlock: Option[Block] = None,
    returnBlock: Option[Block] = None,
    blocks: Iterable[Block] = ArrayBuffer(),
    formalInParam: IterableOnce[LocalVar] = ArrayBuffer(),
    formalOutParam: IterableOnce[LocalVar] = ArrayBuffer(),
    inParamDefaultBinding: Map[LocalVar, Expr] = Map(),
    outParamDefaultBinding: Map[LocalVar, Variable] = Map(),
    requires: IterableOnce[BExpr] = ArrayBuffer(),
    ensures: IterableOnce[BExpr] = ArrayBuffer()
  ) = {
    this(
      name,
      address,
      entryBlock,
      returnBlock,
      mutable.ArrayBuffer.from(blocks),
      mutable.SortedSet.from(formalInParam),
      mutable.SortedSet.from(formalOutParam),
      immutable.SortedMap.from(inParamDefaultBinding),
      immutable.SortedMap.from(outParamDefaultBinding),
      List.from(requires),
      List.from(ensures),
      List(),
      List()
    )
  }

  // None is scc only containing this procedure
  // Some(Set(this)) is a scc only containing this procedure which has call edge to itself (self recursion)
  var scc: Option[Set[Procedure]] = None

  override def deepEquals(o: Object): Boolean = o match {
    case p: Procedure => deepEqualsProc(p)
    case _ => false
  }
  private def deepEqualsProc(p: Procedure) = {
    name == p.name && (p.blocks.size == blocks.size) && {
      p.blocksBookended.zip(blocksBookended).forall { case ((l: Block), (r: Block)) =>
        l.deepEqualsDbg(r)
      }
    }
    // `corresponds` tests whether its iterable arguments are equal, using the given predicate
    // to compare elements pairwise.
    && (returnBlock corresponds p.returnBlock)(_ deepEqualsDbg _)
  }

  var blockCounter = 0

  def freshBlockId(prefix: String) = {
    blockCounter += 1
    prefix + "_" + blockCounter
  }

  def updateBlockSuffix() = {
    blocks.foreach(_.label.split("_").toList match {
      case Snoc(r, ind) =>
        try
          val n = ind.toInt
          blockCounter = Integer.max(blockCounter, n + 1)
        catch
          _ => ()
      case _ => ()
    })
  }

  def normaliseBlockNames() = {
    var counter = 0
    var loopCounter = 0
    sort()
    val prefix = procName
    for (b <- _blocks) {
      counter += 1
      val label = b match {
        case b if b.isEntry => prefix + "_entry"
        case b if b.isReturn => prefix + "_return"
        case b => {
          counter += 1
          if (b.isLoopHeader()) {
            loopCounter += 1
          }
          val loop = if (b.isLoopParticipant()) {
            s"_loop${loopCounter}"
          } else ""
          s"${prefix}${loop}_$counter"
        }
      }

      b.label = label

    }
  }

  def makeCall(label: Option[String] = None) = DirectCall(this, label, outParamDefaultBinding, inParamDefaultBinding)

  var isExternal: Option[Boolean] = None

  /** Get an Iterator in approximate syntactic pre-order of procedures, blocks, and commands. Blocks and procedures are
    * not guaranteed to be in any defined order.
    */
  def iterator: Iterator[CFGPosition] = {
    ILLexicalIterator(Seq(this))
  }

  /** Iterate in cfg pre order.
    */
  def preOrderIterator: Iterator[CFGPosition] = {
    ILForwardIterator(Seq(this), IntraProcIRCursor)
  }

  override def toString: String = Sigil.BASIR.proc + name

  def calls: Set[Procedure] = blocks.iterator.flatMap(_.calls).toSet

  /** Block iteration order is defined such that that the entryBlock is first, and no order is defined beyond that. Both
    * entry block and return block are elements of _blocks.
    */
  def blocks: Iterator[Block] = _blocks.iterator
  def blocksBookended: Iterable[Block] =
    (entryBlock.iterator
      ++ _blocks.iterator
        .filterNot(entryBlock.contains)
        .filterNot(returnBlock.contains)
      ++ returnBlock.iterator).toSeq

  def addCaller(c: DirectCall): Unit = {
    _callers.add(c)
  }

  def removeCaller(c: DirectCall): Unit = {
    _callers.remove(c)
  }

  def returnBlock: Option[Block] = _returnBlock

  def returnBlock_=(value: Block): Unit = {
    if (!returnBlock.contains(value)) {
      _returnBlock = Some(addBlock(value))
    }
  }

  def returnBlock_=(value: Option[Block]): Unit = value match {
    case Some(newblock) => returnBlock = newblock
    case None => _returnBlock = None
  }

  def entryBlock: Option[Block] = _entryBlock

  def entryBlock_=(value: Block): Unit = {
    if (!entryBlock.contains(value)) {
      _entryBlock = Some(addBlock(value))
    }
  }

  def addBlock(block: Block): Block = {
    if (!_blocks.contains(block)) {
      block.parent = this
      _blocks.append(block)
    }
    block
  }

  def addBlocks(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      addBlock(elem)
    }
  }

  def replaceBlock(oldBlock: Block, block: Block): Block = {
    debugAssert(_blocks.contains(oldBlock))
    if (oldBlock ne block) {
      val isEntry: Boolean = entryBlock.contains(oldBlock)
      val isReturn: Boolean = returnBlock.contains(oldBlock)
      val incoming = oldBlock.incomingJumps
      removeBlocksDisconnect(oldBlock)
      addBlock(block)
      for (elem <- incoming) {
        elem.addTarget(block)
      }
      if isEntry then entryBlock = block
      if isReturn then returnBlock = block
    }
    block
  }

  /** Removes all blocks and replaces them with the provided iterator.
    *
    * @param newBlocks
    *   the new set of blocks
    * @return
    *   an iterator to the new block set
    */
  def replaceBlocks(newBlocks: Iterable[Block]): Unit = {
    clearBlocks()
    addBlocks(newBlocks)
  }

  /** Removes a block assuming no existing blocks jump to it.
    * @param block
    *   the block to remove
    * @return
    *   the removed block
    */
  def removeBlocks(block: Block): Block = {
    debugAssert(_blocks.contains(block))
    debugAssert(block.incomingJumps.isEmpty) // don't leave jumps dangling
    block.deParent()
    val index = _blocks.indexOf(block)
    _blocks.remove(index)
    if (_entryBlock.contains(block)) {
      _entryBlock = None
    }
    if (_returnBlock.contains(block)) {
      _returnBlock = None
    }
    block
  }

  /** Remove block(s) and all jumps that target it
    * @param blocks
    *   the blocks to remove
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

  def clearBlocks(): Unit = {
    // O(n) because we are careful to unlink the parents etc.
    // .toList to avoid modifying our own iterator
    removeBlocksDisconnect(_blocks.toList)
  }

  def labelToBlock: Map[String, Block] = {
    blocks.map(p => p.label -> p).toMap
  }
  def loadedLabelToBlock: Map[String, Block] = {
    blocks.filter(_.meta.originalLabel.isDefined).map(p => p.meta.originalLabel.get -> p).toMap
  }

  def callers(): Iterable[Procedure] = _callers.map(_.parent.parent).toSet[Procedure]
  def incomingCalls(): Iterator[DirectCall] = _callers.iterator

  var modifies: mutable.Set[Global] = mutable.Set()

  def reachableFrom: Set[Procedure] = {
    val reachable = mutable.Set[Procedure](this)
    val toVisit = mutable.Queue[Procedure]()
    toVisit.enqueue(this)

    while (toVisit.nonEmpty) {
      val p = toVisit.dequeue()
      val calledBy = p.calls
      for (c <- p.calls) {
        if (!reachable.contains(c)) {
          reachable.add(c)
          toVisit.enqueue(c)
        }
      }
    }
    reachable.toSet
  }

  /**
   *  SSA Form
   */

  var ssaCount = 0
  def getFreshSSAVar(name: String, ty: IRType) = {
    ssaCount += 1
    LocalVar(name, ty, ssaCount)
  }

  def sort() = {
    ir.transforms.reversePostOrder(this)
    _blocks.sortInPlaceBy(_.rpoOrder)
    for (e <- entryBlock) {
      val i = _blocks.indexOf(e)
      if (i != 0) {
        _blocks.remove(i)
        _blocks.prepend(e)
      }
    }
  }

}

class Block private (
  var label: String,
  val statements: IntrusiveList[Statement],
  private var _jump: Jump,
  private val _incomingJumps: mutable.HashSet[GoTo],
  var meta: Metadata
) extends HasParent[Procedure]
    with DeepEquality {
  var atomicSection: Option[AtomicSection] = None
  _jump.setParent(this)
  statements.foreach(_.setParent(this))

  statements.onInsert = x => x.setParent(this)
  statements.onRemove = x => x.deParent()

  def this(
    label: String,
    address: Option[BigInt] = None,
    statements: IterableOnce[Statement] = Set.empty,
    jump: Jump = Unreachable()
  ) = {
    this(label, IntrusiveList().addAll(statements), jump, mutable.HashSet.empty, Metadata(None, address))
  }

  def address = meta.address

  override def deepEquals(b: Object): Boolean = b match {
    case b: Block => deepEqualsBlock(b)
    case o => false
  }
  private def deepEqualsBlock(b: Block): Boolean = {
    (label == b.label) && statements.zip(b.statements).forall { case (l, r) =>
      l.deepEqualsDbg(r)
    }
  }

  def isReturn: Boolean = parent.returnBlock.contains(this)
  def isEntry: Boolean = parent.entryBlock.contains(this)

  var inLoop: Set[Loop] = Set()
  def isLoopHeader() = inLoop.exists(x => x.header == this)
  def isLoopParticipant() = inLoop.nonEmpty

  def jump: Jump = _jump

  var rpoOrder: Long = -1

  private def jump_=(j: Jump): Unit = {
    debugAssert(!j.hasParent)
    if (j ne _jump) {
      _jump.deParent()
      _jump = j
      _jump.parent = this
    }
  }

  def replaceJump(j: Jump): Block = {
    if (j eq jump)
      return this
    if (j.hasParent) {
      val parent = j.parent
      j.deParent()
      parent.jump = Unreachable()
    }
    jump = j
    this
  }

  def incomingJumps: immutable.Set[GoTo] = _incomingJumps.toSet

  def addIncomingJump(g: GoTo): Boolean = _incomingJumps.add(g)

  def removeIncomingJump(g: GoTo): Unit = {
    _incomingJumps.remove(g)
    debugAssert(!incomingJumps.contains(g))
  }

  def calls: Set[Procedure] = statements.toSet.collect { case d: DirectCall =>
    d.target
  }

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  // def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  def calledBy: Set[Block] = {
    Set.empty
  }

  override def toString: String = Sigil.BASIR.block + label

  /** @return
    *   The intra-procedural set of successor blocks. If the block ends in a call then the empty set is returned.
    */
  def nextBlocks: Iterable[Block] = {
    jump match {
      case c: GoTo => c.targets
      case _ => Seq()
    }
  }

  /** @return
    *   The intra-procedural set of predecessor blocks.
    */
  def prevBlocks: Iterable[Block] = {
    _incomingJumps.toSeq.map(_.parent)
  }

  /** If the block has a single block successor then this returns that block, otherwise None.
    *
    * @return
    *   The successor block if there is exactly one
    */
  def singleSuccessor: Option[Block] = {
    jump match {
      case c: GoTo if c.targets.size == 1 => c.targets.headOption
      case _ => None
    }
  }

  /** If the block has a single block predecessor then this returns that block, otherwise None.
    *
    * @return
    *   The predecessor block if there is exactly one
    */
  def singlePredecessor: Option[Block] = {
    if incomingJumps.size == 1 then {
      incomingJumps.headOption.map(_.parent)
    } else None
  }

  override def linkParent(p: Procedure): Unit = {
    // to connect call() links that reference jump.parent.parent
    for (s <- statements) {
      s.setParent(this)
    }
    jump.setParent(this)
  }

  override def unlinkParent(): Unit = {
    // to disconnect call() links that reference jump.parent.parent
    for (s <- statements) {
      s.deParent()
    }
    jump.deParent()
  }

  def createBlockAfter(suffix: String): Block = {
    val label = parent.freshBlockId(suffix)
    val nb = Block(label)
    nb.meta = meta.copy(address = None)

    parent.addBlock(nb)
    val ojump = jump
    replaceJump(GoTo(nb))
    nb.replaceJump(ojump)
  }

  def splitAfterStatement(s: Statement, suffix: String): Block = {
    val rest = statements.splitOn(s)
    val thisJump = jump
    val succ = Block(label + suffix, None, rest, Unreachable())
    parent.addBlock(succ)
    replaceJump(GoTo(succ))
    succ.replaceJump(thisJump)
    succ
  }

  def createBlockBetween(b2: Block, suffix: String = "goto"): Block = {
    debugAssert(nextBlocks.toSet.contains(b2))
    val b1 = this
    val label = parent.freshBlockId(suffix)
    val origs = (meta.originalLabel.toList ++ b2.meta.originalLabel).flatMap(_.split(",")).toSet.toList.sorted match {
      case Nil => None
      case xs => Some(xs.mkString(", "))
    }

    val nb = Block(label)
    nb.meta = Metadata(originalLabel = origs)

    b1.parent.addBlock(nb)
    b1.jump match {
      case g: GoTo => {
        g.addTarget(nb)
        g.removeTarget(b2)
      }
      case _ => ???
    }
    nb.replaceJump(GoTo(b2))
    nb
  }

  def createBlockOnEdgeWith(b2: Block, suffix: String = "_goto"): Block = {
    debugAssert((nextBlocks ++ prevBlocks).exists(_ == b2))
    if (nextBlocks.exists(_ == b2)) {
      createBlockBetween(b2, suffix)
    } else if (prevBlocks.exists(_ == b2)) {
      b2.createBlockBetween(this, suffix)
    } else {
      throw IllegalArgumentException(s"This block does not have edge with ${b2.label}")
    }
  }

}

object Block {
  def procedureReturn(from: Procedure): Block = {
    Block(from.name + "_basil_return", None, List(), Return())
  }
}

/** @param name
  *   name
  * @param address
  *   initial offset of memory section
  * @param size
  *   number of bytes
  * @param bytes
  *   sequence of bytes represented by BitVecLiterals of size 8
  */
case class MemorySection(
  name: String,
  address: BigInt,
  size: Int,
  bytes: Seq[BitVecLiteral],
  readOnly: Boolean,
  region: Option[MergedRegion] = None
) {

  def canGetBytes(addr: BigInt, num: Int): Boolean = {
    (addr >= address) && (addr + num < (address + size))
  }

  def getBytes(addr: BigInt, num: Int): Seq[BitVecLiteral] = {
    val startIndex = (addr - address).toInt
    for (i <- 0 until num) yield {
      val index = startIndex + i
      if (index >= bytes.size || index < 0) {
        throw Exception(
          s"can't get $num bytes from section $name with size $size starting at index $startIndex (access address $addr)"
        )
      }
      bytes(index)
    }
  }

}

class AtomicSection(start: Block, end: Block, blocks: mutable.Set[Block]) {
  def isStart(b: Block): Boolean = {
    if (start == b) {
      true
    } else {
      false
    }
  }

  def isEnd(b: Block): Boolean = {
    if (end == b) {
      true
    } else {
      false
    }
  }

  def getBlocks: mutable.Set[Block] = blocks
}
