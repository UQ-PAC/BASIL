package analysis

import analysis.*
import ir.BitVecLiteral

import scala.collection.mutable

// Define a case class to represent a range
case class RangeKey(var start: BigInt, var end: BigInt):
  def size(): BigInt = end - start

case class RegionToRangesMap():
  val stackMap: mutable.Map[RangeKey, StackRegion] = mutable.Map()
  val heapMap: mutable.Map[RangeKey, HeapRegion] = mutable.Map()
  val dataMap: mutable.Map[RangeKey, DataRegion] = mutable.Map()

// Custom data structure for storing range-to-object mappings
class MemoryModelMap {
  //private val functions = scala.collection.mutable.Map[CfgFunctionExitNode, RegionToRangesMap]()
  private val rangeMap = RegionToRangesMap()
  private val MAX_BIGINT: BigInt = BigInt(Long.MaxValue)
  private val contextStack = mutable.Stack.empty[List[StackRegion]]
  private val allStacks = mutable.Map[String, List[StackRegion]]()

  // Add a range and object to the mapping
  def add(offset: BigInt, region: MemoryRegion): Unit = {
    region match {
      case s: StackRegion =>
        val stackMap = rangeMap.stackMap
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

  def convertMemoryRegions(memoryRegions: Map[CfgNode, LiftedElement[Set[MemoryRegion]]], externalFunctions: Map[BigInt, String]): Unit = {
    // map externalFunctions name, value to DataRegion(name, value) and then sort by value
    val externalFunctionRgns = externalFunctions.map((offset, name) => DataRegion(name, BitVecLiteral(offset, 64)))

    // get all function exit node
    val exitNodes = memoryRegions.keys.collect { case e: CfgFunctionExitNode => e }
    exitNodes.foreach(exitNode =>
      memoryRegions(exitNode) match {
        case Lift(node) =>
          // for each function exit node we get the memory region and add it to the mapping
          val stackRgns = node.collect { case r: StackRegion => r }.toList.sortBy(_.start.value)
          val dataRgns = node.collect { case r: DataRegion => r }

          // add externalFunctionRgn to dataRgns and sort by value
          val allDataRgns = (dataRgns ++ externalFunctionRgns).toList.sortBy(_.start.value)

          allStacks(exitNode.data.name) = stackRgns

          for (dataRgn <- allDataRgns) {
            add(dataRgn.start.value, dataRgn)
          }
        case LiftedBottom =>
    }


    )
  }

  def pushContext(funName: String): Unit = {
    contextStack.push(allStacks(funName))
    rangeMap.stackMap.clear()
    for (stackRgn <- contextStack.top) {
      add(stackRgn.start.value, stackRgn)
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
  }

//  def set_stack_regions(node: CfgNode): Unit = {
//    rangeMap.stackMap.clear()
//    val stackRgns = MRA(node).asInstanceOf[Set[Any]].filter(_.isInstanceOf[StackRegion]).map(_.asInstanceOf[StackRegion]).toList.sortBy(_.start.asInstanceOf[BitVecLiteral].value)
//    print(MRA(node))
//    for (stackRgn <- stackRgns) {
//      add(stackRgn.start.asInstanceOf[BitVecLiteral].value, stackRgn)
//    }
//  }

  // Find an object for a given value within a range


  def findStackObject(value: BigInt): Option[StackRegion] = 
    rangeMap.stackMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {obj.extent = Some(range); obj});

  def findDataObject(value: BigInt): Option[DataRegion] = 
    rangeMap.dataMap.find((range, _) => range.start <= value && value <= range.end).map((range, obj) => {obj.extent = Some(range); obj});

  override def toString: String =
    s"Stack: ${rangeMap.stackMap}\n Heap: ${rangeMap.heapMap}\n Data: ${rangeMap.dataMap}\n"

}

trait MemoryRegion {
  val regionIdentifier: String
  var extent: Option[RangeKey] = None
  val content: mutable.Set[BitVecLiteral] = mutable.Set()
}

class StackRegion(override val regionIdentifier: String, val start: BitVecLiteral) extends MemoryRegion {
  override def toString: String = s"Stack($regionIdentifier, $start) -> $content"
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
    case h: HeapRegion => h.regionIdentifier == regionIdentifier
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

