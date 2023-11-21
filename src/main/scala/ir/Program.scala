package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import boogie.*
import analysis.BitVectorEval
import intrusiveList.{IntrusiveList, IntrusiveListElement}

class Program(var procedures: ArrayBuffer[Procedure], var mainProcedure: Procedure,
              var initialMemory: ArrayBuffer[MemorySection],
              var readOnlyMemory: ArrayBuffer[MemorySection]) {

  // This shouldn't be run before indirect calls are resolved?
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

class Procedure (
                  var name: String,
                  var address: Option[Int],
                  var blocks: IntrusiveList[Block],
                  var in: ArrayBuffer[Parameter],
                  var out: ArrayBuffer[Parameter]
                ) {


  private var _callers = new mutable.HashMap[Procedure, mutable.Set[Call]] with mutable.MultiMap[Procedure, Call]

  def calls: Set[Procedure] = blocks.flatMap(_.calls).toSet
  override def toString: String = {
    s"Procedure $name at ${address.getOrElse("None")} with ${blocks.size} blocks and ${in.size} in and ${out.size} out parameters"
  }

  def removeCaller(c: Call): Unit = {
    _callers.removeBinding(c.parent.parent, c)
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
    val firstBlock = blocks.headOption
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
      for (j <- b.jumps) {
        j match {
          case g: DetGoTo => visitBlock(g.target)
          case d: DirectCall => d.returnTarget.foreach(visitBlock)
          case i: IndirectCall => i.returnTarget.foreach(visitBlock)
          case n: NonDetGoTo => n.targets.foreach(visitBlock)
        }
      }
    }
  }

}
class Parameter(var name: String, var size: Int, var value: Register) {
  def toBoogie: BVariable = BParam(name, BitVecBType(size))
  def toGamma: BVariable = BParam(s"Gamma_$name", BoolBType)
}


class Block private
(var label: String,
 var address: Option[Int],
 val statements: IntrusiveList[Statement],
 // invariant: all Goto targets are disjoint
 private val _jumps: IntrusiveList[Jump],
 //private val _calls: IntrusiveList[Call],
 val incomingJumps: mutable.HashSet[Block],
 val parent: Procedure
) extends IntrusiveListElement {
  def this(label: String, address: Option[Int], statements: IterableOnce[Statement], jumps: IterableOnce[Jump], parent: Procedure) = {
    this(label, address, IntrusiveList.from(statements), IntrusiveList.from(jumps), mutable.HashSet.empty, parent)
  }

  def jumps: immutable.List[Jump] = _jumps to immutable.List

  def addJump(j: Jump): Unit = {
    j.parent = this
    _jumps.append(j)
  }

  def predecessors: immutable.Set[Block] = incomingJumps to immutable.Set

  def removeJump(j: Jump) : Unit = {
    assert(j.parent == this)
    j.deParent()
    _jumps.remove(j)
  }

  def replaceJump(j: Jump, newJ: Jump) : Unit = {
    assert((j.parent == this))
    if (j eq newJ) {
      // GoTo/Call is responsible for maintaining the CFG if it is modified in-place
      return
    }
    (j, newJ) match {
      case (g: GoTo, f: GoTo) => {
        removeJump(j)
        addJump(newJ)
      }
      case (g: Call, f: Call) => {
        removeJump(g)
        addJump(f)
      }
      case (_, _) => throw Exception("Programmer error: can not replace jump with call or vice versa")
    }
  }

  //def addStatementAfter(statement: Statement, newStatement: Statement): Statement = {
  //  val i = statements.indexOf(statement)
  //  statements.insert(i, newStatement)
  //}


  def calls: Set[Procedure] = _jumps.flatMap(_.calls).toSet

  def modifies: Set[Global] = statements.flatMap(_.modifies).toSet
  //def locals: Set[Variable] = statements.flatMap(_.locals).toSet ++ jumps.flatMap(_.locals).toSet

  def calledBy: Set[Block] = {
    Set.empty
  }

  override def toString: String = {
    // display all statements and jumps
    val statementsString = statements.map(_.toString).mkString("\n")
    val jumpsString = jumps.map(_.toString).mkString("\n")
    s"Block $label with $statementsString\n$jumpsString"
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
