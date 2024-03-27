package analysis

import analysis.*
import ir.*
import util.Logger

import scala.collection.mutable

// Define a case class to represent a range
case class RangeKey(start: BigInt, end: BigInt) extends Ordered[RangeKey]:
  val size: BigInt = end - start
  override def compare(that: RangeKey): Int = {
    if (start < that.start) -1
    else if (start > that.start) 1
    else 0
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
          val currentMaxRange = currentStackMap.keys.maxBy(_.end)
          val currentMaxRegion = currentStackMap(currentMaxRange)
          currentStackMap.remove(currentMaxRange)
          val updatedRange = RangeKey(currentMaxRange.start, offset - 1)
          currentStackMap.addOne(updatedRange -> currentMaxRegion)
          currentStackMap(RangeKey(offset, MAX_BIGINT)) = s
        }
      case d: DataRegion =>
        val currentDataMap = dataMap
        if (currentDataMap.isEmpty) {
          currentDataMap(RangeKey(offset, MAX_BIGINT)) = d
        } else {
          val currentMaxRange = currentDataMap.keys.maxBy(_.end)
          val currentMaxRegion = currentDataMap(currentMaxRange)
          currentDataMap.remove(currentMaxRange)
          val updatedRange = RangeKey(currentMaxRange.start, offset - 1)
          currentDataMap.addOne(updatedRange -> currentMaxRegion)
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

  def convertMemoryRegions(memoryRegions: Map[CFGPosition, LiftedElement[Set[MemoryRegion]]], externalFunctions: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt], procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]]): Unit = {
    // map externalFunctions name, value to DataRegion(name, value) and then sort by value
    val externalFunctionRgns = externalFunctions.map((offset, name) => resolveInverseGlobalOffset(name, BitVecLiteral(offset, 64), globalOffsets))

    // we should collect all data regions otherwise the ordering might be wrong
    var dataRgns: Set[DataRegion] = Set.empty
    // get all function exit node
    val exitNodes = memoryRegions.keys.collect { case p: CFGPosition if IRWalk.procedure(p).end == p => p }

    exitNodes.foreach(exitNode =>
      memoryRegions(exitNode) match {
        case Lift(node) =>
          if (procedureToSharedRegions.contains(IRWalk.procedure(exitNode))) {
            val sharedRegions = procedureToSharedRegions(IRWalk.procedure(exitNode))
            sharedStacks(IRWalk.procedure(exitNode).name) = sharedRegions.collect { case r: StackRegion => r }.toList.sortBy(_.start.value)
          }
          // for each function exit node we get the memory region and add it to the mapping
          val stackRgns = node.collect { case r: StackRegion => r }.toList.sortBy(_.start.value)
          dataRgns = dataRgns ++ node.collect { case r: DataRegion => r }

          localStacks(IRWalk.procedure(exitNode).name) = stackRgns

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
    stackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => obj)

  def findSharedStackObject(value: BigInt): Set[StackRegion] =
    sharedStackMap.values.flatMap(_.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => obj)).toSet

  def findDataObject(value: BigInt): Option[DataRegion] = 
    dataMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => obj)

  override def toString: String =
    s"Stack: $stackMap\n Heap: $heapMap\n Data: $dataMap\n"

  def logRegions(content: Map[MemoryRegion, Set[BitVecLiteral | MemoryRegion]] = Map.empty): Unit = {
    def logRegion(range: RangeKey, region: MemoryRegion, shared: Boolean = false): Unit = {
      // the spacing level is based on region type
      val spacing = region match {
          case _: StackRegion => if shared then "           " else "       "
          case _: HeapRegion => "  "
          case _: DataRegion => "  "
      }
      Logger.debug(s"$spacing$range -> $region")
      if content.contains(region) then
        if content.contains(region) then
          for value <- content(region) do
            Logger.debug(s"$spacing    $value")
    }
    Logger.debug("Stack:")
    for name <- localStacks.keys do
      popContext()
      pushContext(name)
      Logger.debug(s"  Function: $name")
      if stackMap.nonEmpty then Logger.debug(s"    Local:")
      // must sort by ranges
      for ((range, region) <- stackMap) {
        logRegion(range, region)
      }
      if sharedStackMap.nonEmpty then Logger.debug(s"    Shared:")
      for ((parent, treeMap) <- sharedStackMap) {
        Logger.debug(s"        Parent: ${parent.name}")
        for ((range, region) <- treeMap) {
          logRegion(range, region, true)
        }
      }
    Logger.debug("Heap:")
    for ((range, region) <- heapMap) {
      logRegion(range, region)
    }
    Logger.debug("Data:")
    for ((range, region) <- dataMap) {
      logRegion(range, region)
    }
  }
}

trait MemoryRegion {
  val regionIdentifier: String
}

case class StackRegion(override val regionIdentifier: String, start: BitVecLiteral, parent: Procedure) extends MemoryRegion {
  override def toString: String = s"Stack($regionIdentifier, $start, ${parent.name})"
}

case class HeapRegion(override val regionIdentifier: String, size: BitVecLiteral) extends MemoryRegion {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataRegion(override val regionIdentifier: String, start: BitVecLiteral) extends MemoryRegion {
  override def toString: String = s"Data($regionIdentifier, $start)"
}
