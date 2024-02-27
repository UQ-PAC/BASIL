package analysis

import analysis.*
import ir.{BitVecLiteral, CFGPosition, Command, Procedure}

import scala.collection.mutable

// Define a case class to represent a range
case class RangeKey(var start: BigInt, var end: BigInt) extends Ordered[RangeKey]:
  def size(): BigInt = end - start
  override def compare(that: RangeKey): Int = {
    if (start < that.start) -1
    else if (start > that.start) 1
    else 0
  }

case class RegionToRangesMap():
  val stackMap: mutable.Map[RangeKey, StackRegion] = mutable.TreeMap()
  val sharedStackMap: mutable.Map[RangeKey, StackRegion] = mutable.TreeMap()
  val heapMap: mutable.Map[RangeKey, HeapRegion] = mutable.TreeMap()
  val dataMap: mutable.Map[RangeKey, DataRegion] = mutable.TreeMap()

// Custom data structure for storing range-to-object mappings
class MemoryModelMap {
  //private val functions = scala.collection.mutable.Map[CfgFunctionExitNode, RegionToRangesMap]()
  private val rangeMap = RegionToRangesMap()
  private val MAX_BIGINT: BigInt = BigInt(Long.MaxValue)
  private val contextStack = mutable.Stack.empty[List[StackRegion]]
  private val sharedContextStack = mutable.Stack.empty[List[StackRegion]]
  private val localStacks = mutable.Map[String, List[StackRegion]]()
  private val sharedStacks = mutable.Map[String, List[StackRegion]]().withDefaultValue(List.empty[StackRegion])

  /** Add a range and object to the mapping
   *
   * @param offset the offset of the range
   * @param region the region to add
   * @param shared if the region is shared. When true, the region is added to the sharedStackMap
   *                otherwise to the stackMap
   */
  def add(offset: BigInt, region: MemoryRegion, shared: Boolean = false): Unit = {
    region match {
      case s: StackRegion =>
        var stackMap = rangeMap.stackMap
        if (shared) {
          stackMap = rangeMap.sharedStackMap
        }
        if (stackMap.isEmpty) {
          stackMap(RangeKey(offset, MAX_BIGINT)) = s
        } else {
          stackMap.keys.maxBy(_.end).end = offset - 1
          stackMap(RangeKey(offset, MAX_BIGINT)) = s
        }
      case d: DataRegion =>
        val dataMap = rangeMap.dataMap
        if (dataMap.isEmpty) {
          dataMap(RangeKey(offset, MAX_BIGINT)) = d
        } else {
          dataMap.keys.maxBy(_.end).end = offset - 1
          dataMap(RangeKey(offset, MAX_BIGINT)) = d
        }
    }
  }

  /**
   * For DataRegions, the actual address used needs to be converted to the relocated address.
   * This is because when regions are found, the relocated address is used and as such match
   * the correct range.
   *
   * @param name
   * @param address
   * @param globalOffsets
   * @return DataRegion: a DataRegion representing the actual address
   */
  def resolveInverseGlobalOffset(name: String, address: BitVecLiteral, globalOffsets: Map[BigInt, BigInt]): DataRegion = {
    val inverseGlobalOffsets = globalOffsets.map(_.swap)
    var tableAddress = inverseGlobalOffsets.getOrElse(address.value, address.value)
    // addresses may be layered as in jumptable2 example for recursive search required
    var exitLoop = false
    while (inverseGlobalOffsets.contains(tableAddress) && !exitLoop) {
      val newAddress = inverseGlobalOffsets.getOrElse(tableAddress, tableAddress)
      if (newAddress == tableAddress) {
        exitLoop = true
      } else {
        tableAddress = newAddress
      }
    }

    DataRegion(name, BitVecLiteral(tableAddress, 64))
  }

  def convertMemoryRegions(memoryRegions: Map[CFGPosition, LiftedElement[Set[MemoryRegion]]], externalFunctions: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]]): Unit = {
    // map externalFunctions name, value to DataRegion(name, value) and then sort by value
    val externalFunctionRgns = externalFunctions.map((offset, name) => resolveInverseGlobalOffset(name, BitVecLiteral(offset, 64), globalOffsets))

    // we should collect all data regions otherwise the ordering might be wrong
    var dataRgns: Set[DataRegion] = Set.empty
    // get all function exit node
    val exitNodes = memoryRegions.keys.collect { case e: Command if e. => e }
    exitNodes.foreach(exitNode =>
      memoryRegions(exitNode) match {
        case Lift(node) =>
          if (procedureToSharedRegions.contains(exitNode.data)) {
            val sharedRegions = procedureToSharedRegions(exitNode.data)
            sharedStacks(exitNode.data.name) = sharedRegions.collect { case r: StackRegion => r }.toList.sortBy(_.start.value)
          }
          // for each function exit node we get the memory region and add it to the mapping
          val stackRgns = node.collect { case r: StackRegion => r }.toList.sortBy(_.start.value)
          dataRgns = dataRgns ++ node.collect { case r: DataRegion => r }

          localStacks(exitNode.data.name) = stackRgns

        case LiftedBottom =>
    }
    )
    // add externalFunctionRgn to dataRgns and sort by value
    val allDataRgns = (dataRgns ++ externalFunctionRgns).toList.sortBy(_.start.value)
    for (dataRgn <- allDataRgns) {
      add(dataRgn.start.value, dataRgn)
    }
  }
  // TODO: push and pop could be optimised by caching the results
  def pushContext(funName: String): Unit = {
    contextStack.push(localStacks(funName))
    rangeMap.stackMap.clear()
    for (stackRgn <- contextStack.top) {
      add(stackRgn.start.value, stackRgn)
    }

    sharedContextStack.push(sharedStacks(funName))
    rangeMap.sharedStackMap.clear()
    for (stackRgn <- sharedContextStack.top) {
      add(stackRgn.start.value, stackRgn, true)
    }
  }

  def popContext(): Unit = {
    if (contextStack.size > 1) {
      contextStack.pop()
      rangeMap.stackMap.clear()
      for (stackRgn <- contextStack.top) {
        add(stackRgn.start.value, stackRgn)
      }
    }

    if (sharedContextStack.size > 1) {
      sharedContextStack.pop()
      rangeMap.sharedStackMap.clear()
      for (stackRgn <- sharedContextStack.top) {
        add(stackRgn.start.value, stackRgn, true)
      }
    }
  }


  def findStackObject(value: BigInt): Option[StackRegion] = 
    rangeMap.stackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {obj.extent = Some(range); obj});

  def findSharedStackObject(value: BigInt): Option[StackRegion] =
    rangeMap.sharedStackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {obj.extent = Some(range); obj});

  def findDataObject(value: BigInt): Option[DataRegion] = 
    rangeMap.dataMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {obj.extent = Some(range); obj});

  override def toString: String =
    s"Stack: ${rangeMap.stackMap}\n Heap: ${rangeMap.heapMap}\n Data: ${rangeMap.dataMap}\n"

  def printRegionsContent(hideEmpty: Boolean = false): Unit = {
    println("Stack:")
    for name <- localStacks.keys do
      popContext()
      pushContext(name)
      println(s"  Function: $name")
      if rangeMap.stackMap.nonEmpty then println(s"    Local:")
      // must sort by ranges
      for ((range, region) <- rangeMap.stackMap) {
        if (region.content.nonEmpty || !hideEmpty) {
          println(s"       $range -> $region")
        }
      }
      if rangeMap.sharedStackMap.nonEmpty then println(s"    Shared:")
      for ((range, region) <- rangeMap.sharedStackMap) {
          if (region.content.nonEmpty || !hideEmpty) {
          println(s"       $range -> $region")
          }
      }
    println("Heap:")
    for ((range, region) <- rangeMap.heapMap) {
      if (region.content.nonEmpty || !hideEmpty) {
        println(s"  $range -> $region")
      }
    }
    println("Data:")
    for ((range, region) <- rangeMap.dataMap) {
      if (region.content.nonEmpty || !hideEmpty) {
        println(s"  $range -> $region")
      }
    }
  }
}

trait MemoryRegion {
  val regionIdentifier: String
  var extent: Option[RangeKey] = None
  val content: mutable.Set[BitVecLiteral | MemoryRegion] = mutable.Set()
}

class StackRegion(override val regionIdentifier: String, val start: BitVecLiteral, val parent: Procedure = null) extends MemoryRegion {
  override def toString: String = s"Stack($regionIdentifier, $start, ${if parent != null then parent.name else "Null"}) -> $content"
  override def hashCode(): Int = regionIdentifier.hashCode() * start.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case s: StackRegion => s.start == start && s.regionIdentifier.equals(regionIdentifier)
    case _ => false
  }
}

class HeapRegion(override val regionIdentifier: String, val size: BitVecLiteral) extends MemoryRegion {
  override def toString: String = s"Heap($regionIdentifier, $size) -> $content"
  override def hashCode(): Int = regionIdentifier.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case h: HeapRegion => h.regionIdentifier.equals(regionIdentifier)
    case _ => false
  }
}

class DataRegion(override val regionIdentifier: String, val start: BitVecLiteral) extends MemoryRegion {
  override def toString: String = s"Data($regionIdentifier, $start) -> $content"
  override def hashCode(): Int = regionIdentifier.hashCode() * start.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case d: DataRegion => d.start == start && d.regionIdentifier.equals(regionIdentifier)
    case _ => false
  }
}
