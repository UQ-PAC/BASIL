package analysis

import analysis.*
import ir.BitVecLiteral

import scala.collection.mutable

// Define a case class to represent a range
case class RangeKey(var start: BigInt, var end: BigInt)
case class RegionToRangesMap():
  val stackMap: mutable.Map[RangeKey, MemoryRegion] = mutable.Map()
  val heapMap: mutable.Map[RangeKey, MemoryRegion] = mutable.Map()
  val dataMap: mutable.Map[RangeKey, MemoryRegion] = mutable.Map()

// Custom data structure for storing range-to-object mappings
class MemoryModelMap {
  //private val functions = scala.collection.mutable.Map[CfgFunctionExitNode, RegionToRangesMap]()
  private val rangeMap = RegionToRangesMap()
  private val MAX_BIGINT: BigInt = BigInt(Long.MaxValue)
  private val contextStack = mutable.Stack.empty[List[StackRegion]]
  private val allStacks = mutable.Map[String, List[StackRegion]]()

  // Add a range and object to the mapping
  def add(offset: BigInt, regionType: MemoryRegion): Unit = {
    regionType match {
      case s: StackRegion =>
        val stackMap = rangeMap.stackMap
        if (stackMap.isEmpty) {
          stackMap(RangeKey(offset, MAX_BIGINT)) = regionType
        } else {
          stackMap.keys.maxBy(_.end).end = offset - 1
          stackMap(RangeKey(offset, MAX_BIGINT)) = regionType
        }
      case d: DataRegion =>
        val dataMap = rangeMap.dataMap
        if (dataMap.isEmpty) {
          dataMap(RangeKey(offset, MAX_BIGINT)) = regionType
        } else {
          dataMap.keys.maxBy(_.end).end = offset - 1
          dataMap(RangeKey(offset, MAX_BIGINT)) = regionType
        }
    }
  }

  def convertMemoryRegions(memoryRegions: Map[CfgNode, MemoryRegion], externalFunctions: Map[BigInt, String]): Unit = {
    // get all function exit node
    val exitNodes = memoryRegions.keys.collect { case e: CfgFunctionExitNode => e }.toList
    exitNodes.foreach(exitNode =>
      val node = memoryRegions(exitNode).asInstanceOf[Set[Any]]
      // for each function exit node we get the memory region
      // and add it to the mapping
      val stackRgns = node.collect{ case r: StackRegion => r }.toList.sortBy(_.start.asInstanceOf[BitVecLiteral].value)
      val dataRgns = node.collect{ case r: DataRegion => r }.toList
      val heapRgns = node.collect{ case r: HeapRegion => r }.toList
      val accessRgns = node.collect{ case r: RegionAccess => r }.toList
      // map externalFunctions name, value to DataRegion(name, value) and then sort by value
      val externalFunctionRgns = externalFunctions.map(
        (offset, name) => DataRegion(name, BitVecLiteral(offset, 64))
      ).toList

      // add externalFunctionRgn to dataRgns and sort by value
      val allDataRgns = (dataRgns ++ externalFunctionRgns).sortBy(_.start.asInstanceOf[BitVecLiteral].value)


      allStacks(exitNode.data.name) = stackRgns

//      for (stackRgn <- stackRgns) {
//        add(stackRgn.start.asInstanceOf[BitVecLiteral].value, stackRgn)
//      }

      for (dataRgn <- allDataRgns) {
        add(dataRgn.start.asInstanceOf[BitVecLiteral].value, dataRgn)
      }

    )
  }

  def pushContext(funName: String): Unit = {
    contextStack.push(allStacks(funName))
    rangeMap.stackMap.clear()
    for (stackRgn <- contextStack.top) {
      add(stackRgn.start.asInstanceOf[BitVecLiteral].value, stackRgn)
    }
  }

  def popContext(): Unit = {
    if (contextStack.size <= 1) {
      return
    }
    contextStack.pop()
    rangeMap.stackMap.clear()
    for (stackRgn <- contextStack.top) {
      add(stackRgn.start.asInstanceOf[BitVecLiteral].value, stackRgn)
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
  def findObject(value: BigInt, regionType: String): Option[MemoryRegion] = {
    regionType match
      case "stack" =>
        for ((range, obj) <- rangeMap.stackMap) {
          if (range.start <= value && value <= range.end) {
            return Some(obj)
          }
        }
        None
      case "data" =>
        for ((range, obj) <- rangeMap.dataMap) {
          if (range.start <= value && value <= range.end) {
            return Some(obj)
          }
        }
        None
  }

  override def toString: String =
    s"Stack: ${rangeMap.stackMap}\n Heap: ${rangeMap.heapMap}\n Data: ${rangeMap.dataMap}\n"

}


