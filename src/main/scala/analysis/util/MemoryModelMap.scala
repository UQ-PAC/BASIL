//package analysis
//
//import analysis._
//
//// Define a case class to represent a range
//case class RangeKey(start: Int, end: Int)
//
//// Custom data structure for storing range-to-object mappings
//class MemoryModelMap {
//  private val rangeToObjectMap = scala.collection.mutable.Map[RangeKey, MemoryRegion]()
//
//  // Add a range and object to the mapping
//  def add(range: RangeKey, obj: MemoryRegion): Unit = {
//    rangeToObjectMap(range) = obj
//  }
//
//  def convertMemoryRegions(memoryRegions: Map[CfgNode, _]): Unit = {
//    // get all function exit node
//    val exitNode = memoryRegions.keys.filter(_.isInstanceOf[CfgFunctionExitNode]).toList
//    // get the main exit node
//    val mainExitNode = exitNode.filter(_.asInstanceOf[CfgFunctionExitNode].data.name == "lmain").head
//    // get the main exit node's memory region
//    val mainExitNodeMemoryRegion: Set[Any] = memoryRegions(mainExitNode)
//
//
//    mainExitNodeMemoryRegion.foreach { case region =>
//      region match {
//        case stackRegion: StackRegion =>
//          add(RangeKey(stackRegion.start, stackRegion.end), stackRegion)
//        case heapRegion: HeapRegion =>
//          add(RangeKey(heapRegion.start, heapRegion.end), heapRegion)
//        case dataRegion: DataRegion =>
//          add(RangeKey(dataRegion.start, dataRegion.end), dataRegion)
//        case _ =>
//      }
//    }
////  }
//
//  // Find an object for a given value within a range
//  def findObject(value: Int): Option[MemoryRegion] = {
//    rangeToObjectMap.find { case (range, _) => value >= range.start && value <= range.end }.map(_._2)
//  }
//}
//
//
