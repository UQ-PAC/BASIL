package analysis

import analysis.*
import ir.{BitVecLiteral, Procedure}
import util.Logger

import scala.collection.mutable

// Define a case class to represent a range
class RangeKey(var start: BigInt, var end: BigInt) extends Ordered[RangeKey]:
  def size(): BigInt = end - start
  override def compare(that: RangeKey): Int = {
    if (start < that.start) -1
    else if (start > that.start) 1
    else 0
  }
  override def hashCode(): Int = start.hashCode() * end.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case r: RangeKey => r.start == start && r.end == end
    case _ => false
  }
  override def toString: String = s"Range[$start, $end]"


// Custom data structure for storing range-to-object mappings
class MemoryModelMap {
  private val MAX_BIGINT: BigInt = BigInt(Long.MaxValue)
  private val contextStack = mutable.Stack.empty[List[StackRegion]]
  private val sharedContextStack = mutable.Stack.empty[List[StackRegion]]
  private val localStacks = mutable.Map[String, List[StackRegion]]()
  private val sharedStacks = mutable.Map[String, List[StackRegion]]()

  private val stackMap: mutable.Map[RangeKey, StackRegion] = mutable.TreeMap()
  private val sharedStackMap: mutable.Map[Procedure, mutable.TreeMap[RangeKey, StackRegion]] = mutable.Map[Procedure, mutable.TreeMap[RangeKey, StackRegion]]()
  private val heapMap: mutable.Map[RangeKey, HeapRegion] = mutable.TreeMap()
  private val dataMap: mutable.Map[RangeKey, DataRegion] = mutable.TreeMap()

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
        var currentStackMap = stackMap
        if (shared) {
          currentStackMap = sharedStackMap.getOrElseUpdate(s.parent, mutable.TreeMap())
        }
        if (currentStackMap.isEmpty) {
          currentStackMap(RangeKey(offset, MAX_BIGINT)) = s
        } else {
          currentStackMap.keys.maxBy(_.end).end = offset - 1
          currentStackMap(RangeKey(offset, MAX_BIGINT)) = s
        }
      case d: DataRegion =>
        val currentDataMap = dataMap
        if (currentDataMap.isEmpty) {
          currentDataMap(RangeKey(offset, MAX_BIGINT)) = d
        } else {
          currentDataMap.keys.maxBy(_.end).end = offset - 1
          currentDataMap(RangeKey(offset, MAX_BIGINT)) = d
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
  private def resolveInverseGlobalOffset(name: String, address: BitVecLiteral, globalOffsets: Map[BigInt, BigInt]): DataRegion = {
    val inverseGlobalOffsets = globalOffsets.map(_.swap)
    var tableAddress = inverseGlobalOffsets.getOrElse(address.value, address.value)
    // addresses may be layered as in jumptable2 example for which recursive search is required
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

  def convertMemoryRegions(memoryRegions: Map[CfgNode, LiftedElement[Set[MemoryRegion]]], externalFunctions: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]]): Unit = {
    // map externalFunctions name, value to DataRegion(name, value) and then sort by value
    val externalFunctionRgns = externalFunctions.map((offset, name) => resolveInverseGlobalOffset(name, BitVecLiteral(offset, 64), globalOffsets))

    // we should collect all data regions otherwise the ordering might be wrong
    var dataRgns: Set[DataRegion] = Set.empty
    // get all function exit node
    val exitNodes = memoryRegions.keys.collect { case e: CfgFunctionExitNode => e }
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
    stackMap.clear()
    for (stackRgn <- contextStack.top) {
      add(stackRgn.start.value, stackRgn)
    }

    if (!sharedStacks.contains(funName)) {
      sharedStacks(funName) = List.empty
    }
    sharedContextStack.push(sharedStacks(funName))
    sharedStackMap.clear()
    for (stackRgn <- sharedContextStack.top) {
      add(stackRgn.start.value, stackRgn, true)
    }
  }

  def popContext(): Unit = {
    if (contextStack.size > 1) {
      contextStack.pop()
      stackMap.clear()
      for (stackRgn <- contextStack.top) {
        add(stackRgn.start.value, stackRgn)
      }
    }

    if (sharedContextStack.size > 1) {
      sharedContextStack.pop()
      sharedStackMap.clear()
      for (stackRgn <- sharedContextStack.top) {
        add(stackRgn.start.value, stackRgn, true)
      }
    }
  }


  def findStackObject(value: BigInt): Option[StackRegion] = 
    stackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {
      obj.extent = Some(range)
      obj
    })

  def findSharedStackObject(value: BigInt): Set[StackRegion] =
    sharedStackMap.values.flatMap(_.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {
      obj.extent = Some(range)
      obj
    }).toSet).toSet

  def findDataObject(value: BigInt): Option[DataRegion] = 
    dataMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {
      obj.extent = Some(range)
      obj
    })

  override def toString: String =
    s"Stack: ${stackMap}\n Heap: ${heapMap}\n Data: ${dataMap}\n"

  def printRegionsContent(hideEmpty: Boolean = false): Unit = {
    Logger.debug("Stack:")
    for name <- localStacks.keys do
      popContext()
      pushContext(name)
      Logger.debug(s"  Function: $name")
      if stackMap.nonEmpty then Logger.debug(s"    Local:")
      // must sort by ranges
      for ((range, region) <- stackMap) {
        if (region.content.nonEmpty || !hideEmpty) {
          Logger.debug(s"       $range -> $region")
        }
      }
      if sharedStackMap.nonEmpty then Logger.debug(s"    Shared:")
      for ((parent, treeMap) <- sharedStackMap) {
        Logger.debug(s"        Parent: ${parent.name}")
        for ((range, region) <- treeMap) {
          if (region.content.nonEmpty || !hideEmpty) {
            Logger.debug(s"           $range -> $region")
          }
        }
      }
    Logger.debug("Heap:")
    for ((range, region) <- heapMap) {
      if (region.content.nonEmpty || !hideEmpty) {
        Logger.debug(s"  $range -> $region")
      }
    }
    Logger.debug("Data:")
    for ((range, region) <- dataMap) {
      if (region.content.nonEmpty || !hideEmpty) {
        Logger.debug(s"  $range -> $region")
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
    case s: StackRegion => s.start == start && s.regionIdentifier == regionIdentifier
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
    case d: DataRegion => d.start == start && d.regionIdentifier == regionIdentifier
    case _ => false
  }
}
