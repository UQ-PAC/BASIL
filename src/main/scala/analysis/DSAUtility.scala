package analysis

import ir.{Expr, Procedure, Register}

import scala.collection.mutable;

object NodeCounter {
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter
}


class DSG(val proc: Procedure) {
  // DSNodes owned by this graph
  val nodes: mutable.Map[MemoryRegion2, DSN] = mutable.Map()


  def addNode(memoryRegion2: MemoryRegion2, offset: BigInt, size: Int): DSN =
    if nodes.contains(memoryRegion2) then
      nodes(memoryRegion2).addCell(offset, size)
    else
      val node = DSN(Some(this), Some(memoryRegion2))
      nodes.update(memoryRegion2, node)
      node.addCell(offset, size)
    nodes(memoryRegion2)
}

class DSN(val graph: Option[DSG], var region: Option[MemoryRegion2]) {

  val id: Int = NodeCounter.getCounter

  var collapsed = false

  var size: BigInt = region match
    case Some(value) => value match
      case DataRegion2(regionIdentifier, start, size) => size
      case HeapRegion2(regionIdentifier, proc, size) => size
      case StackRegion2(regionIdentifier, proc, size) => size
      case UnknownRegion2(regionIdentifier, proc) => 0
    case None => 0

  val cells: mutable.Map[BigInt, DSC] = mutable.Map()
  this.addCell(0, 0)

  def updateSize(newSize: BigInt): Unit =

    if newSize > size then
      size = newSize
  def addCell(offset: BigInt, size: Int) =
    this.updateSize(offset + size)
    if !cells.contains(offset) then
      val cell = DSC(Some(this), offset)
      cells.update(offset, cell)
      cell.addAccessedSize(size)
    else
      cells(offset).addAccessedSize(size)

  override def equals(obj: Any): Boolean =
    obj match
      case node: DSN =>
        this.id == node.id
      case _ => false

  override def toString: String = s"Node($id${if region.isDefined then s",${region.get.toString}" else ""})"
}

class DSC(val node: Option[DSN], val offset: BigInt)
{
  val accessedSizes: mutable.Set[Int] = mutable.Set()
  def addAccessedSize(size: Int): Unit =
    if size != 0 then accessedSizes.add(size)

  override def toString: String = s"Cell(${if node.isDefined then node.get.toString else "NONE"}, $offset)"
}

class SimulationMapper
{

}

class Field {}


class Offset
{}

class Alloc
{}

class CallSite
{

}

