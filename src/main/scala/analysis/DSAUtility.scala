package analysis

import ir.{BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, IntraProcIRCursor, LocalAssign, MemoryAssign, MemoryStore, Procedure, Register, Variable, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal}

import scala.collection.mutable;

object NodeCounter {
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    if counter == 64 then
      print("")
    counter


}



class DSG(proc: Procedure,
          constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
          globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
          externalFunctions: Set[ExternalFunction],
          reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
          writesTo: Map[Procedure, Set[Register]]) {
  // DSNodes owned by this graph
  val nodes: mutable.Set[DSN] = mutable.Set()
  val pointTo: mutable.Map[DSC, DSC] = mutable.Map()

  val mallocRegister = Register("R0", BitVecType(64))
  val stackPointer = Register("R31", BitVecType(64))


  // make all globals
  private val swappedOffsets = globalOffsets.map(_.swap)
  val globalMapping: mutable.Map[(BigInt, BigInt), DSN] = globals.foldLeft(mutable.Map[(BigInt, BigInt), DSN]()) {
    (m, global) =>
      var address: BigInt = global.address
      if swappedOffsets.contains(address) then
        address = swappedOffsets(address)
      m + ((address, address + global.size) -> DSN(Some(this), Some(DataRegion2(global.name, address, global.size))))
  }
  externalFunctions.foreach(
    external =>
      var address: BigInt = external.offset
      if swappedOffsets.contains(address) then
        address = swappedOffsets(address)
      globalMapping.update((address, address), DSN(Some(this), Some(DataRegion2(external.name, address, 0))))
  )


  // determine if an address is a global and return the corresponding global if it is.
  def isGlobal(address: BigInt): Option[DSN] =
    for (elem <- globalMapping) {
      val range = elem._1
      if address >= range._1 && address <= range._2 then
        return Some(elem._2)
    }
    None

  private def replaceInEV(oldCell: DSC, newCell: DSC) =
    varToCell.foreach(
      (pos, m) =>
        m.foreach(
          (variable, cell) =>
            if cell.equals(oldCell) then
              m.update(variable, newCell)
        )
    )

  private def replaceInPointTo(oldCell: DSC, newCell: DSC) =
    pointTo.foreach {
      case (pointer, pointee) =>
        if pointee.equals(oldCell) then
          pointTo.update(pointer, newCell)
    }

  def getPointee(cell: DSC): DSC =
    if !pointTo.contains(cell) then
      val node = DSN(None, None)
      pointTo.update(cell, node.cells(0))
    pointTo(cell)


//  private def earlyCollapse(node: DSN): Unit =
//    node.collapsed = true
//    node.cells.clear()
//
//    node.addCell(0, 0)

  def collapseNode(node: DSN): Unit =
    val collapedCell = DSC(Option(node), 0, true)
    val e = DSC(None, 0)

    if node.id == 20 then
      print("")



    val cell = node.cells.foldLeft(e) {
      (c, field) =>

        if pointTo.contains(field._2) && pointTo(field._2) == field._2 then
          pointTo.update(field._2, collapedCell)
          c
        else if pointTo.contains(field._2) then
          mergeCells(c, getPointee(field._2))
        else
          c
    }

    node.cells.values.foreach(
      cell =>
        replaceInEV(cell, collapedCell)
        replaceInPointTo(cell, collapedCell)
        pointTo.foreach {
          case (pointer, pointee) =>
            if pointer.equals(cell) then
              pointTo.remove(pointer)
              pointTo.update(collapedCell, pointee)
        }
    )

    node.collapsed = true


    node.cells.clear()
    node.cells.addOne(0, collapedCell)
    if cell.node.isDefined then
      pointTo.update(node.cells(0), cell)


  def mergeCells(cell1: DSC, cell2: DSC): DSC =
    if (cell1 == cell2) {
      return cell1
    }
    if (incompatibleTypes(cell1, cell2)) then
      collapseNode(cell2.node.get)

    if cell1.node.isDefined then
      cell2.node.get.allocationRegions.addAll(cell1.node.get.allocationRegions)


    if cell2.node.get.collapsed then
      if cell1.node.isDefined then
        cell1.node.get.cells.foreach {
          case (offset, cell) =>
            if pointTo.contains(cell) then
              if pointTo.contains(cell2.node.get.cells(0)) then
                mergeCells(getPointee(cell), getPointee(cell2.node.get.cells(0)))
              else
                pointTo.update(cell2.node.get.cells(0), getPointee(cell))
              pointTo.remove(cell)
            replaceInPointTo(cell, cell2.node.get.cells(0))
            replaceInEV(cell, cell2.node.get.cells(0))
        }
        cell2.node.get.cells(0)
      else
        if pointTo.contains(cell1) then
          if pointTo.contains(cell2.node.get.cells(0)) then
            mergeCells(getPointee(cell1), getPointee(cell2.node.get.cells(0)))
          else
            pointTo.update(cell2.node.get.cells(0), getPointee(cell1))
          pointTo.remove(cell1)
        replaceInPointTo(cell1, cell2.node.get.cells(0))
        replaceInEV(cell1, cell2.node.get.cells(0))
        cell2.node.get.cells(0)
    else
      cell1.node.get.cells.foreach {
        case (offset, cell) =>
          if pointTo.contains(cell) then
            if pointTo.contains(cell2.node.get.cells(offset)) then
              mergeCells(getPointee(cell), getPointee(cell2.node.get.cells(offset)))
            else
              pointTo.update(cell2.node.get.cells(offset), getPointee(cell))
          pointTo.remove(cell)
          replaceInPointTo(cell, cell2.node.get.cells(offset))
          replaceInEV(cell, cell2.node.get.cells(offset))
      }
      cell2


  private def incompatibleTypes(cell1: DSC, cell2: DSC): Boolean =
    if cell2.node.get.collapsed then
      return false
    else if cell1.node.isEmpty || (cell1.collapsedCell && !cell2.collapsedCell) then
      return true // TODO not sure about this
    else if cell1.offset != cell2.offset then
      return true
    else if cell1.node.get.cells.size != cell2.node.get.cells.size then
      return true
    else
      (cell1.node.get.cells zip cell2.node.get.cells).foreach { //TODO remove unaccessed cells from type matching/allow unaccessed fields to merge with an accessed field
        case ((o1, c1), (o2, c2)) =>
          if o1 != o2 || !c1.accessedSizes.equals(c2.accessedSizes) then
            return true
      }
    false


  private def isFormal(pos: CFGPosition, variable: Variable): Boolean =
    variable != stackPointer && !reachingDefs(pos).contains(variable)

  val formals: mutable.Map[Variable, DSC] = mutable.Map()
  val varToCell: Map[CFGPosition, mutable.Map[Variable, DSC]] = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[CFGPosition, mutable.Map[Variable, DSC]]()) {
    (m, pos) =>
      pos match
        case LocalAssign(variable, value , label) =>
          if pos.asInstanceOf[LocalAssign].label.get.startsWith("%0000044f") then
            print("")
          value.variables.foreach(
            v =>
              if isFormal(pos, v) then
                val node = DSN(Some(this), None)
                node.rep = "formal"
                nodes.add(node)
                formals.update(v, node.cells(0))
          )
          val node = DSN(Some(this), None)
          node.rep = "ssa"
          m + (pos -> mutable.Map(variable -> node.cells(0)))
        case DirectCall(proc, target, label) if proc.name == "malloc" =>
          val node = DSN(Some(this), None)
          node.rep = "ssa"
           m + (pos -> mutable.Map(mallocRegister -> node.cells(0)))
        case DirectCall(proc, target, label) if writesTo.contains(proc) =>
          val result: Map[Variable, DSC] = writesTo(proc).foldLeft(Map[Variable, DSC]()){
            (n, variable) =>
              val node = DSN(Some(this), None)
              node.rep = "ssa"
              n + (variable -> node.cells(0))
          }
          m + (pos -> result.to(mutable.Map))
        case MemoryAssign(memory, MemoryStore(mem, index, value: Variable, endian, size), label) =>
          if isFormal(pos, value) then
            val node = DSN(Some(this), None)
            node.rep = "formal"
            nodes.add(node)
            formals.update(value, node.cells(0))
          m
        case _ => m
  }


//  def coolMergeCells(cell1: DSC, cell2: DSC): DSC =
//    val changedFormals = formals.foldLeft(Set[Variable]()) {
//      (s, f) =>
//        if f._2 == cell1 then
//          s + f._1
//        else
//          s
//    }
//
//    val changedVars = varToCell.foldLeft(Map[CFGPosition, Set[Variable]]()) {
//
//    }
//
//    cell1





  def addNode(memoryRegion2: MemoryRegion2, offset: BigInt, size: Int): DSN = ???
//    if nodes.contains(memoryRegion2) then
//      nodes(memoryRegion2).addCell(offset, size)
//    else
//      val node = DSN(Some(this), Some(memoryRegion2))
//      nodes.update(memoryRegion2, node)
//      node.addCell(offset, size)
//    nodes(memoryRegion2)
}

class DSN(val graph: Option[DSG], var region: Option[MemoryRegion2]) {

  val id: Int = NodeCounter.getCounter

  var collapsed = false

  val allocationRegions: mutable.Set[MemoryRegion2] = region match
    case Some(value) => mutable.Set(value)
    case None => mutable.Set()

  var rep: String = ""

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
      if cells(offset).accessedSizes.size > 1 then
        graph.get.collapseNode(this)


  override def equals(obj: Any): Boolean =
    obj match
      case node: DSN =>
        this.id == node.id
      case _ => false

  override def toString: String = s"Node($id, $allocationRegions ${if collapsed then ", collapsed" else ""})"
}

case class DSC(node: Option[DSN], offset: BigInt, collapsedCell: Boolean = false)
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

