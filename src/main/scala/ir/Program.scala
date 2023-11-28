package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import boogie.*
import analysis.BitVectorEval
import intrusiveList.{IntrusiveList, IntrusiveListElement}

trait HasParent[T]:
  /*
      If a node is reachable from the IL then it *must* have a parent defined. This will only be null until
      the object is fully initialised.

      All IL structures must set the parent of the child to itself, when a child is added to itself.
   */
  private var _parent: Option[T] = None
  def parent: T = _parent.get

  protected final def setParentValue(p: T): Unit = _parent = Some(p)

  def deParent(): Unit = {}

  def setParent(p: T): Unit = setParentValue(p)

class Program(var procedures: ArrayBuffer[Procedure], var mainProcedure: Procedure,
              var initialMemory: ArrayBuffer[MemorySection],
              var readOnlyMemory: ArrayBuffer[MemorySection]) {

  // This shouldn't be run before indirect calls are resolved
  def stripUnreachableFunctions(): Unit = {
    val functionToChildren = procedures.map(f => f.name -> f.calls.map(_.name)).toMap

    var next = mainProcedure.name
    var reachableNames: Set[String] = Set(next)
    var toVisit: List[String] = List()
    var reachableFound = true
    while (reachableFound) {
      val children = functionToChildren(next) -- reachableNames -- toVisit - next
      reachableNames = reachableNames ++ children
      toVisit = toVisit ++ children
      if (toVisit.isEmpty) {
        reachableFound = false
      } else {
        next = toVisit.head
        toVisit = toVisit.tail
      }
    }
    procedures = procedures.filter(f => reachableNames.contains(f.name))
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
    if ((name.startsWith("R") || name.startsWith("V")) && (name.length == 2 || name.length == 3) && name.substring(1).forall(_.isDigit)) {
      if (name.startsWith("R")) {
        Register(name, BitVecType(64))
      } else {
        Register(name, BitVecType(128))
      }
    } else {
      Memory(name, 64, 8)
    }
  }

  def stackIdentification(): Unit = {
    for (p <- procedures) {
      p.stackIdentification()
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


}

class Procedure private (
                  var name: String,
                  var address: Option[Int],
                  private val _blocks: mutable.HashSet[Block],
                  var in: ArrayBuffer[Parameter],
                  var out: ArrayBuffer[Parameter],
                ) {

  def this(name: String, address: Option[Int] = None , blocks: IterableOnce[Block] = ArrayBuffer(), in: IterableOnce[Parameter] = ArrayBuffer(), out: IterableOnce[Parameter] = ArrayBuffer()) = {
    this(name, address, mutable.HashSet.from(blocks), ArrayBuffer.from(in), ArrayBuffer.from(out))
  }

  private var _entryBlock: Option[Block] = None
  val returnBlock: Block = new Block(name + "_return", None, List(), new IndirectCall(Register("R30", BitVecType(64)), None, Some(name)))
  addBlocks(returnBlock)

  private var _callers = new mutable.HashMap[Procedure, mutable.Set[Call]] with mutable.MultiMap[Procedure, Call]

  def entryBlock: Option[Block] = {
    _entryBlock match {
      case None =>
        _entryBlock = blocks.find(p => p.address == address)
        _entryBlock
      case Some(b) => Some(b)
    }
  }

  def calls: Set[Procedure] = blocks.flatMap(_.calls).toSet
  override def toString: String = {
    s"Procedure $name at ${address.getOrElse("None")} with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }

  def blocks: Iterator[Block] = _blocks.iterator

  def removeCaller(c: Call): Unit = {
    _callers.removeBinding(c.parent.parent, c)
  }

  def addBlocks(block: Block): Block = {
    block.deParent()
    block.setParent(this)
    _blocks.add(block)
    block
  }

  def addBlocks(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      addBlocks(elem)
    }
  }

  def replaceBlock(oldBlock: Block, block: Block): Block = {
    require(_blocks.contains(oldBlock) || block == returnBlock)
    if (oldBlock ne block) {
      removeBlocks(oldBlock)
      addBlocks(block)
    }
    block
  }

  /**
   * Removes all blocks and replaces them with the provided iterator.
   *
   * @param newBlocks the new set of blocks
   * @return an iterator to the new block set
   */
  def replaceBlocks(newBlocks: Iterable[Block]): Iterator[Block] = {
    _blocks.clear
    addBlocks(newBlocks)
    blocks
  }


  def removeBlocks(block: Block): Block = {
    block.deParent()
    _blocks.remove(block)
    block
  }
  def removeBlocks(blocks: Iterable[Block]): Unit = {
    for (elem <- blocks) {
      removeBlocks(elem)
    }
  }

  def addCaller(c: Call): Unit = {
    _callers.addBinding(c.parent.parent, c)
  }

  def callers(): Iterable[Procedure] = _callers.keySet

  var modifies: mutable.Set[Global] = mutable.Set()


  def stackIdentification(): Unit = {
    val stackPointer = Register("R31", BitVecType(64))
    val stackRefs: mutable.Set[Variable] = mutable.Set(stackPointer)
    val visitedBlocks: mutable.Set[Block] = mutable.Set()
    val stackMemory = Memory("stack", 64, 8)
    val firstBlock = entryBlock
    firstBlock.foreach(visitBlock)

    // does not handle loops but we do not currently support loops in block CFG so this should do for now anyway
    def visitBlock(b: Block): Unit = {
      if (visitedBlocks.contains(b)) {
        return
      }
      for (s <- b.statements) {
        s match {
          case l: LocalAssign =>
            // replace mem with stack in loads if index contains stack references
            val loads = l.rhs.loads
            for (load <- loads) {
              val loadStackRefs = load.index.variables.intersect(stackRefs)
              if (loadStackRefs.nonEmpty) {
                load.mem = stackMemory
              }
            }

            // update stack references
            val variableVisitor = VariablesWithoutStoresLoads()
            variableVisitor.visitExpr(l.rhs)

            val rhsStackRefs = variableVisitor.variables.toSet.intersect(stackRefs)
            if (rhsStackRefs.nonEmpty) {
              stackRefs.add(l.lhs)
            } else if (stackRefs.contains(l.lhs) && l.lhs != stackPointer) {
              stackRefs.remove(l.lhs)
            }
          case m: MemoryAssign =>
            // replace mem with stack if index contains stack reference
            val indexStackRefs = m.rhs.index.variables.intersect(stackRefs)
            if (indexStackRefs.nonEmpty) {
              m.lhs = stackMemory
              m.rhs.mem = stackMemory
            }
          case _ =>
        }
      }
      visitedBlocks.add(b)
      b.jump match {
        case g: GoTo => g.targets.foreach(visitBlock)
        case d: DirectCall => d.returnTarget.foreach(visitBlock)
        case i: IndirectCall => i.returnTarget.foreach(visitBlock)
      }
    }
  }

}
class Parameter(var name: String, var size: Int, var value: Register) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}


class Block private (var label: String,
 var address: Option[Int],
 val statements: IntrusiveList[Statement],
 // invariant: all Goto targets are disjoint
 private var _jump: Option[Jump],
 //private val _calls: IntrusiveList[Call],
 val incomingJumps: mutable.HashSet[Block],
) extends IntrusiveListElement, HasParent[Procedure] {
  statements.foreach(_.setParent(this))
  _jump.foreach(_.setParent(this))
  statements.onInsert = x => x.setParent(this)
  statements.onRemove = x => x.deParent()

  statements.onInsert = parenter

  def parenter(c: Statement): Statement = {
    c.setParent(this)
    c
  }
  def this(label: String, address: Option[Int], statements: IterableOnce[Statement], jump: Jump) = {
    this(label, address, IntrusiveList.from(statements), Some(jump), mutable.HashSet.empty)
    _jump.foreach(_.setParent(this))
  }

  def this(label: String, address: Option[Int], statements: IterableOnce[Statement]) = {
    this(label, address, IntrusiveList.from(statements), None, mutable.HashSet.empty)
  }

  def this(label: String, address: Option[Int] = None) = {
    this(label, address, IntrusiveList(), None, mutable.HashSet.empty)
  }

  def jump: Jump = _jump.get
  def jumpSet: Set[Jump] = _jump.toSet

  def addGoToTargets(targets: mutable.Set[Block]): this.type = {
    require(_jump.isDefined && _jump.get.isInstanceOf[GoTo])
    _jump.foreach(_.asInstanceOf[GoTo].addAllTargets(targets))
    this
  }

  def replaceJump(j: Jump): this.type = {
    _jump.foreach(_.deParent())
    j.setParent(this)
    _jump = Some(j)
    this
  }

  def isReturn: Boolean = this == parent.returnBlock

//  def replaceGoTo(targets: Iterable[Block], label: Option[String] = None): this.type = {
//    _jump.foreach(_.deParent)
//    _jump = Some(GoTo(targets, label).setParent(this))
//    this
//  }
//
//  def replaceDirectCall(target: Procedure, returnTarget: Option[Block] = None, label: Option[String] = None): this.type = {
//    _jump.foreach(_.deParent)
//    _jump = Some(DirectCall(target, returnTarget, label).setParent(this))
//    this
//  }
//
//  def addIndirectCall(target: Variable, returnTarget: Option[Block] = None, label: Option[String] = None): this.type = {
//    _jump.foreach(_.deParent)
//    _jump = Some(IndirectCall(target, returnTarget, label).setParent(this))
//    this
//  }

  def removeJump(): this.type = {
    _jump.foreach(_.deParent())
    _jump = None
    this
  }


  def predecessors: immutable.Set[Block] = incomingJumps to immutable.Set

  def calls: Set[Procedure] = _jump.toSet.flatMap(_.calls)

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  def calledBy: Set[Block] = {
    Set.empty
  }

  override def toString: String = {
    // display all statements and jumps
    val statementsString = statements.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jump"
  }

  override def equals(obj: scala.Any): Boolean =
    obj match
      case b: Block => b.label == this.label
      case _ => false

  override def hashCode(): Int = label.hashCode()
}


/**
  * @param name name
  * @param address initial offset of memory section
  * @param size number of bytes
  * @param bytes sequence of bytes represented by BitVecLiterals of size 8
  */
case class MemorySection(name: String, address: Int, size: Int, bytes: Seq[BitVecLiteral])
