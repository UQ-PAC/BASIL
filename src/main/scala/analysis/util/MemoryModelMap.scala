package analysis

import analysis.*
import ir.BitVecLiteral

// Define a case class to represent a range
case class RangeKey(var start: BigInt, var end: BigInt)
case class RegionToRangesMap():
  val stackMap = scala.collection.mutable.Map[RangeKey, MemoryRegion]()
  val heapMap = scala.collection.mutable.Map[RangeKey, MemoryRegion]()
  val dataMap = scala.collection.mutable.Map[RangeKey, MemoryRegion]()

// Custom data structure for storing range-to-object mappings
class MemoryModelMap {
  private val functions = scala.collection.mutable.Map[CfgFunctionExitNode, RegionToRangesMap]()
  private val MAX_BIGINT: BigInt = BigInt(Long.MaxValue)

  // Add a range and object to the mapping
  def add(offset: BigInt, regionType: MemoryRegion, function: CfgFunctionExitNode): Unit = {
    regionType match {
      case s: StackRegion =>
        val stackMap = functions(function).stackMap
        if (stackMap.isEmpty) {
          stackMap(RangeKey(offset, MAX_BIGINT)) = regionType
        } else {
          stackMap.last._1.end = offset - 1
          stackMap(RangeKey(offset, MAX_BIGINT)) = regionType
        }
      case d: DataRegion =>
        val dataMap = functions(function).dataMap
        if (dataMap.isEmpty) {
          dataMap(RangeKey(offset, MAX_BIGINT)) = regionType
        } else {
          dataMap.last._1.end = offset - 1
          dataMap(RangeKey(offset, MAX_BIGINT)) = regionType
        }
    }
  }

  def convertMemoryRegions(memoryRegions: Map[CfgNode, _]): Unit = {
    // get all function exit node
    val exitNodes = memoryRegions.keys.filter(_.isInstanceOf[CfgFunctionExitNode]).toList
    exitNodes.foreach(exitNode =>
      functions(exitNode.asInstanceOf[CfgFunctionExitNode]) = RegionToRangesMap()
      // for each function exit node we get the memory region
      // and add it to the mapping
      val stackRgns = memoryRegions(exitNode).asInstanceOf[Set[Any]].filter(_.isInstanceOf[StackRegion]).map(_.asInstanceOf[StackRegion]).toList.sortBy(_.start.asInstanceOf[BitVecLiteral].value)
      val dataRgns = memoryRegions(exitNode).asInstanceOf[Set[Any]].filter(_.isInstanceOf[DataRegion]).map(_.asInstanceOf[DataRegion]).toList.sortBy(_.start.asInstanceOf[BitVecLiteral].value)

      for (stackRgn <- stackRgns) {
        add(stackRgn.start.asInstanceOf[BitVecLiteral].value, stackRgn, exitNode.asInstanceOf[CfgFunctionExitNode])
      }

      for (dataRgn <- dataRgns) {
        add(dataRgn.start.asInstanceOf[BitVecLiteral].value, dataRgn, exitNode.asInstanceOf[CfgFunctionExitNode])
      }

    )
  }

  // Find an object for a given value within a range
  def findObject(value: Int, regionType: MemoryRegion, funcExit: CfgFunctionExitNode): Option[MemoryRegion] = {
    regionType match
      case s:StackRegion =>
        for ((range, obj) <- functions(funcExit).stackMap) {
          if (range.start <= value && value <= range.end) {
            return Some(obj)
          }
        }
        None
      case d:DataRegion =>
        for ((range, obj) <- functions(funcExit).dataMap) {
          if (range.start <= value && value <= range.end) {
            return Some(obj)
          }
        }
        None
  }

  override def toString: String =
    val sb = new StringBuilder()
    for ((exitNode, regionMap) <- functions) {
      sb.append(s"Function: ${exitNode.data.name}\n")
      sb.append(s"Stack: ${regionMap.stackMap}\n")
      sb.append(s"Heap: ${regionMap.heapMap}\n")
      sb.append(s"Data: ${regionMap.dataMap}\n")
    }
    sb.toString()
}


