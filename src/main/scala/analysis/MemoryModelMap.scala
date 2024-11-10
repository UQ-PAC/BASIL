package analysis

import analysis.*
import ir.*
import util.Logger

import scala.collection.immutable.TreeMap
import scala.collection.mutable

// Define a case class to represent a range
case class RangeKey(start: BigInt, end: BigInt) extends Ordered[RangeKey]:
  val size: BigInt = end - start + 1
  override def compare(that: RangeKey): Int = {
    if (start < that.start) -1
    else if (start > that.start) 1
    else 0
  }
  override def toString: String = s"Range[$start, $end] (size: $size)"

// Custom data structure for storing range-to-object mappings
class MemoryModelMap(val globalOffsets: Map[BigInt, BigInt]) {
  private val contextStack = mutable.Stack.empty[String]
  private val sharedContextStack = mutable.Stack.empty[List[StackRegion]]
  private val localStacks = mutable.Map[String, List[StackRegion]]().withDefaultValue(List.empty)
  private val sharedStacks = mutable.Map[String, List[StackRegion]]()

  private val stackMap: mutable.Map[RangeKey, StackRegion] = mutable.TreeMap()
  private val bufferedStackMap: mutable.Map[String, mutable.Map[RangeKey, StackRegion]] = mutable.Map()
  private val sharedStackMap: mutable.Map[Procedure, mutable.TreeMap[RangeKey, StackRegion]] = mutable.Map[Procedure, mutable.TreeMap[RangeKey, StackRegion]]()
  private val bufferedSharedStackMap: mutable.Map[String, mutable.Map[Procedure, mutable.TreeMap[RangeKey, StackRegion]]] = mutable.Map()
  private val heapMap: mutable.Map[RangeKey, HeapRegion] = mutable.TreeMap()
  private val dataMap: mutable.Map[RangeKey, DataRegion] = mutable.TreeMap()
  private val cfgPositionToDataRegion: mutable.Map[CFGPosition, Set[DataRegion]] = mutable.Map()
  private val heapCalls: mutable.Map[DirectCall, HeapRegion] = mutable.Map()
  private var relocatedAddressesMap: Map[BigInt, DataRegion] = Map()

  private val stackAllocationSites: mutable.Map[CFGPosition, Set[StackRegion]] = mutable.Map()

  private val uf = UnionFind()
  val relfContent: mutable.Map[DataRegion, mutable.Set[String]] = mutable.Map()
  val stackSubAccesses: mutable.Map[StackRegion, mutable.Set[BigInt]] = mutable.Map()

  /** Add a range and object to the mapping
   *
   * @param offset the offset of the range, if a heap region is given, the offsets controls the shift of regions from the start
   * @param region the region to add
   * @param shared if the region is shared. When true, the region is added to the sharedStackMap
   *                otherwise to the stackMap
   */
  def add(offset: BigInt, region: MemoryRegion, shared: Boolean = false): Unit = {
    def maxSize(r: MemoryRegion): BigInt = {
      r match {
        case DataRegion(_, start, size) => start + size
        case _: HeapRegion => ???
        case s: StackRegion =>
          if (stackSubAccesses.contains(s) && stackSubAccesses(s).nonEmpty) {
            s.start + stackSubAccesses(s).max
          } else {
            ???
          }
        case _ => ???
      }
    }

    def regionsOverlap(r1: RangeKey, r2: RangeKey): Boolean = {
      r1.start <= r2.end && r2.start <= r1.end
    }

    region match {
      case s: StackRegion =>
        var currentStackMap = stackMap
        if (shared) {
          currentStackMap = sharedStackMap.getOrElseUpdate(s.parent, mutable.TreeMap())
        }
        if (currentStackMap.isEmpty) {
          currentStackMap(RangeKey(offset, maxSize(region) - 1)) = s
        } else {
          val currentMaxRange = currentStackMap.keys.maxBy(_.end)
          val currentMaxRegion = currentStackMap(currentMaxRange)
          if (offset <= currentMaxRange.end) {
            currentStackMap.remove(currentMaxRange)
            val updatedRange = RangeKey(currentMaxRange.start, (maxSize(region) - 1).max(currentMaxRange.end))
            currentStackMap.addOne(updatedRange -> currentMaxRegion)
          } else {
            currentStackMap(RangeKey(offset, maxSize(region) - 1)) = s
          }
        }
      case d: DataRegion =>
        val currentDataMap = dataMap
        if (currentDataMap.isEmpty) {
          currentDataMap(RangeKey(offset, maxSize(d) - 1)) = d
        } else {
          val currentMaxRange = currentDataMap.keys.maxBy(_.end)
          if (regionsOverlap(currentMaxRange, RangeKey(offset, maxSize(d) - 1))) {
            currentDataMap.remove(currentMaxRange) // TODO: this removes previously overlapping parent region (jumptable2 example) which favours more fine grained regions
            currentDataMap(RangeKey(offset, maxSize(d) - 1)) = d
          } else {
            currentDataMap(RangeKey(offset, maxSize(d) - 1)) = d
          }
        }
      case h: HeapRegion =>
        val currentHeapMap = heapMap
        if (currentHeapMap.isEmpty) {
          currentHeapMap(RangeKey(offset, offset + h.size - 1)) = h
        } else {
          val currentMaxRange = currentHeapMap.keys.maxBy(_.end)
          currentHeapMap(RangeKey(currentMaxRange.start + 1, h.size - 1)) = h
        }
    }
  }

  private var relocCount: Int = 0
  private def nextRelocCount() = {
    relocCount += 1
    s"reloc_$relocCount"
  }

  // size of pointer is 8 bytes
  private val SIZE_OF_POINTER = 8

  def preLoadGlobals(externalFunctions: Map[BigInt, String], globalAddresses: Map[BigInt, String], globalSizes: Map[String, Int]): Unit = {
    val relocRegions = globalOffsets.keys.map(offset => DataRegion(nextRelocCount(), offset, SIZE_OF_POINTER))

    // map externalFunctions name, value to DataRegion(name, value) and then sort by value
    val filteredGlobalOffsets = globalAddresses.filterNot((offset, _) => externalFunctions.contains(offset))

    val externalFunctionRgns = (externalFunctions ++ filteredGlobalOffsets).map((offset, name) => DataRegion(name, offset, (globalSizes.getOrElse(name, 1).toDouble / 8).ceil.toInt))

    // add externalFunctionRgn to dataRgns and sort by value
    val allDataRgns = (externalFunctionRgns ++ relocRegions).toList.sortBy(_.start)
    for (dataRgn <- allDataRgns) {
      add(dataRgn.start, dataRgn)
    }

    // cannot fail to find any regions here
    relocatedAddressesMap = globalOffsets.map { (offset, offset2) =>
      (offset, findDataObject(offset2).get)
    }
  }

  def relocatedDataRegion(value: BigInt): Option[DataRegion] = {
    relocatedAddressesMap.get(value)
  }

  def convertMemoryRegions(stackRegionsPerProcedure: mutable.Map[Procedure, mutable.Set[StackRegion]],
                           heapRegions: mutable.Map[DirectCall, HeapRegion],
                           allocationSites: Map[CFGPosition, Set[StackRegion]],
                           procedureToSharedRegions: mutable.Map[Procedure, mutable.Set[MemoryRegion]],
                           graRegions: mutable.HashMap[BigInt, DataRegion],
                           graResults: Map[CFGPosition, Set[DataRegion]]): Unit = {
    //val keepData = dataMap.filterNot((range, region) => graRegions.contains(region.start)).map((range, region) => region)
    val oldRegions = dataMap.values.toSet
    dataMap.clear()
    for (dr <- graRegions.values) {
      add(dr.start, dr)
    }
    for (dr <- oldRegions) {
      val obj = findDataObject(dr.start)
      if (obj.isEmpty) {
        Logger.debug(s"Data region $dr not found in the new data map")
      } else {
        val isRelocated = relocatedDataRegion(dr.start)
        if (isRelocated.isDefined) {
          relfContent(obj.get) = relfContent.getOrElse(obj.get, mutable.Set()) += isRelocated.get.regionIdentifier
        } else {
          relfContent(obj.get) = relfContent.getOrElse(obj.get, mutable.Set()) += dr.regionIdentifier
        }
      }
    }

    cfgPositionToDataRegion ++= graResults
    stackAllocationSites ++= allocationSites
    stackRegionsPerProcedure.keys.foreach { proc =>
      if (procedureToSharedRegions.contains(proc)) {
        val sharedRegions = procedureToSharedRegions(proc)
        sharedStacks(proc.name) = sharedRegions.collect { case r: StackRegion => r }.toList.sortBy(_.start)
      }
      // for each function exit node we get the memory region and add it to the mapping
      val stackRgns = stackRegionsPerProcedure(proc).toList.sortBy(_.start)
      localStacks(proc.name) = stackRgns
    }

    heapCalls ++= heapRegions
    // add heap regions
    val rangeStart = 0
    for (heapRegion <- heapRegions.values) {
      add(rangeStart, heapRegion)
    }

    /* this is done because the stack regions will change after MMM transforms them
    and merges some of them based on size, thus we need to alter the results of
    the analysis to match MMM transformations
    TODO: Can this be done directly in MRA?
     */
    for ((n, stacks) <- stackAllocationSites) {
      pushContext(IRWalk.procedure(n).name)
      stackAllocationSites(n) = stacks.map(r => findStackObject(r.start).getOrElse(r))
      pushContext(IRWalk.procedure(n).name)
    }
  }

  def pushContext(funName: String): Unit = {
    contextStack.push(funName)
    stackMap.clear()
    if (bufferedStackMap.contains(funName)) {
      stackMap ++= bufferedStackMap(funName)
    } else {
      for (stackRgn <- localStacks(contextStack.top).sortBy(_.start)) {
        add(stackRgn.start, stackRgn)
      }
      bufferedStackMap(funName) = stackMap.clone()
    }

    if (!sharedStacks.contains(funName)) {
      sharedStacks(funName) = List.empty
    }
    sharedContextStack.push(sharedStacks(funName))
    sharedStackMap.clear()
    if (bufferedSharedStackMap.contains(funName)) {
      sharedStackMap ++= bufferedSharedStackMap(funName)
    } else {
      for (stackRgn <- sharedContextStack.top.sortBy(_.start)) {
        add(stackRgn.start, stackRgn, true)
      }
      bufferedSharedStackMap(funName) = sharedStackMap.clone()
    }
  }

  def popContext(): Unit = {
    if (contextStack.size > 1) {
      contextStack.pop()
      stackMap.clear()
      if (bufferedStackMap.contains(contextStack.top)) {
        stackMap ++= bufferedStackMap(contextStack.top)
      } else {
        for (stackRgn <- localStacks(contextStack.top)) {
          add(stackRgn.start, stackRgn)
        }
      }
    }

    if (sharedContextStack.size > 1) {
      sharedContextStack.pop()
      sharedStackMap.clear()
      if (bufferedSharedStackMap.contains(contextStack.top)) {
        sharedStackMap ++= bufferedSharedStackMap(contextStack.top)
      } else {
        for (stackRgn <- sharedContextStack.top) {
          add(stackRgn.start, stackRgn, true)
        }
      }
    }
  }

  def findStackObject(value: BigInt): Option[StackRegion] = 
    stackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => returnRegion(obj))

  def findSharedStackObject(value: BigInt): Set[StackRegion] =
    sharedStackMap.values.flatMap(_.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => returnRegion(obj))).toSet

  def findDataObject(value: BigInt): Option[DataRegion] =
    dataMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => returnRegion(obj))

  def findDataObjectWithSize(value: BigInt, size: BigInt): (Set[DataRegion], Set[DataRegion]) =
    // get regions that are between value and value + size and put partial regions (if part of the regions is between value and value + size) in a separate set
    dataMap.foldLeft((Set.empty[DataRegion], Set.empty[DataRegion])) { case ((fullRegions, partialRegions), (range, region)) =>
      if (range.start >= value && range.end <= value + size - 1) {
        (fullRegions + returnRegion(region), partialRegions)
      } else if ((range.start < value && range.end >= value) || (range.start <= value + size - 1 && range.end > value + size - 1)) {
        (fullRegions, partialRegions + returnRegion(region))
      } else {
        (fullRegions, partialRegions)
      }
    }

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
      region match
        case region1: DataRegion if relfContent.contains(region1) => for value <- relfContent(region1) do
          Logger.debug(s"$spacing    $value")
        case _ =>
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
    Logger.debug("Stack Union-Find Roots:")
    for name <- localStacks.keys do
      popContext()
      pushContext(name)
      Logger.debug(s"  Function: $name")
      var parentCount = 0
      // get root regions
      for ((range, region) <- stackMap) {
        val root = uf.find(region)
        if root == region then
          logRegion(range, root)
          parentCount += 1
      }
      if parentCount == 0 then Logger.debug("    No root regions") else Logger.debug(s"    Parents: $parentCount/${stackMap.size}")
    Logger.debug("Shared Stacks:")
    for (name, sharedStacks) <- sharedStacks do
      Logger.debug(s"  Function: $name")
      for region <- sharedStacks do
        Logger.debug(s"    $region")
    Logger.debug("Heap:")
    for ((range, region) <- heapMap) {
      logRegion(range, region)
    }
    Logger.debug("Data:")
    for ((range, region) <- dataMap) {
      logRegion(range, region)
    }
  }

  def mergeRegions(regions: Set[MemoryRegion]): MemoryRegion = {
    // assert regions are of the same type
    regions.foreach(uf.makeSet)
    regions.foreach(uf.union(regions.head, _))
    uf.find(regions.head)
  }

  private def returnRegion(region: MemoryRegion): MemoryRegion = {
    uf.find(region)
  }

  private def returnRegion(region: StackRegion): StackRegion = {
    uf.find(region).asInstanceOf[StackRegion]
  }

  private def returnRegion(region: DataRegion): DataRegion = {
    uf.find(region).asInstanceOf[DataRegion]
  }

  private def returnRegion(region: HeapRegion): HeapRegion = {
    uf.find(region).asInstanceOf[HeapRegion]
  }

  def getHeap(directCall: DirectCall): HeapRegion = {
    require(directCall.target.name == "malloc", "Should be a malloc call")
    heapCalls(directCall)
  }

  def getHeapRegions: Set[HeapRegion] = {
    heapMap.values.toSet
  }

  def getStack(allocationSite: CFGPosition): Set[StackRegion] = {
    stackAllocationSites.getOrElse(allocationSite, Set.empty).map(returnRegion)
  }

  def getLocalStacks: mutable.Map[String, List[StackRegion]] = localStacks

  def getData(cfgPosition: CFGPosition): Set[DataRegion] = {
    cfgPositionToDataRegion.getOrElse(cfgPosition, Set.empty).map(returnRegion)
  }

  def getDataRegions: Set[DataRegion] = {
    dataMap.values.toSet
  }

  def nodeToRegion(n: CFGPosition): Set[MemoryRegion] = {
    n match {
      case directCall: DirectCall =>
        Set(getHeap(directCall))
      case _ =>
        getStack(n) ++ getData(n)
    }
  }

  def getOffset(region: MemoryRegion): BigInt = {
    region match {
      case s: StackRegion => s.start
      case d: DataRegion => d.start
      case h: HeapRegion => h.start
    }
  }
}

trait MemoryRegion {
  val regionIdentifier: String
  val start: BigInt
}

case class StackRegion(override val regionIdentifier: String, override val start: BigInt, parent: Procedure) extends MemoryRegion {
  override def toString: String = s"Stack($regionIdentifier, $start, ${parent.name}"
}

case class HeapRegion(override val regionIdentifier: String, override val start: BigInt, size: BigInt, parent: Procedure) extends MemoryRegion {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataRegion(override val regionIdentifier: String, override val start: BigInt, size: BigInt) extends MemoryRegion {
  override def toString: String = s"Data($regionIdentifier, $start, $size)"
  val end: BigInt = start + size - 1
}

class UnionFind {
  // Map to store the parent of each region
  private val parent: mutable.Map[MemoryRegion, MemoryRegion] = mutable.Map()

  // Map to store the size of each set, used for union by rank
  private val size: mutable.Map[MemoryRegion, Int] = mutable.Map()

  // Initialise each region to be its own parent and set size to 1
  def makeSet(region: MemoryRegion): Unit = {
    parent(region) = region
    size(region) = 1
  }

  // Find operation with path compression
  def find(region: MemoryRegion): MemoryRegion = {
    if (!parent.contains(region)) {
      makeSet(region)
    }

    if (parent(region) != region) {
      parent(region) = find(parent(region)) // Path compression
    }
    parent(region)
  }

  // Union operation with union by rank
  def union(region1: MemoryRegion, region2: MemoryRegion): Unit = {
    val root1 = find(region1)
    val root2 = find(region2)

    if (root1 != root2) {
      if (size(root1) < size(root2)) {
        parent(root1) = root2
        size(root2) += size(root1)
      } else {
        parent(root2) = root1
        size(root1) += size(root2)
      }
    }
  }

}