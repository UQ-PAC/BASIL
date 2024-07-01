package analysis

import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, Assign, Memory, MemoryAssign, MemoryLoad,  Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, begin, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal}

import scala.util.control.Breaks.{break, breakable}
import java.math.BigInteger
import scala.collection.mutable

object NodeCounter {
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter
}


/**
 * Data Structure Graph for DSA
 * @param proc procedure of DSG
 * @param constProp
 * @param varToSym mapping flow-sensitive (position sensitive) mapping from registers to their set of symbolic accesses
 * @param globals
 * @param globalOffsets
 * @param externalFunctions
 * @param reachingDefs
 * @param writesTo
 * @param params
 */
class DSG(val proc: Procedure,
          constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
          varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]],
          globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
          externalFunctions: Set[ExternalFunction],
          val reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
          val writesTo: Map[Procedure, Set[Register]],
          val params: Map[Procedure, Set[Variable]]
         ) {
  // DSNodes owned by this graph
  val nodes: mutable.Set[DSN] = mutable.Set()
  val pointTo: mutable.Map[DSC, (DSC, BigInt)] = mutable.Map()
  val callsites: mutable.Set[CallSite] = mutable.Set()

  val mallocRegister = Register("R0", 64)
  val stackPointer = Register("R31", 64)

  // make stack nodes with
  val stackMapping: mutable.Map[BigInt, DSN] =
    computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[BigInt, DSN]()) {
      (results, pos) => stackBuilder(pos, results)
    }.to(collection.mutable.Map)

  private def visitStackAccess(pos: CFGPosition, index: Expr, size: Int, m: Map[BigInt, DSN]) : Map[BigInt, DSN] =
    val byteSize = (size.toDouble / 8).ceil.toInt
    index match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        evaluateExpression(arg2, constProp(pos)).isDefined =>
        var offset = evaluateExpression(arg2, constProp(pos)).get.value
        varToSym(pos)(arg1).foldLeft(m) {
          (m, sym) =>
            sym match
              case SymbolicAccess(accessor, StackLocation(regionIdentifier, proc, size), symOffset) =>
                offset = offset + symOffset
                if m.contains(offset) then
                  assert(!m(offset).cells(0).growSize(byteSize))
                  m
                else
                  val node = DSN(Some(this), byteSize)
                  node.allocationRegions.add(StackLocation(pos.toShortString, proc, byteSize))
                  node.flags.stack = true
                  node.addCell(0, byteSize)
                  m + (offset -> node)
              case _ => m
        }
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
        varToSym(pos)(arg).foldLeft(m) {
          (m, sym) =>
            sym match
              case SymbolicAccess(accessor, StackLocation(regionIdentifier, proc, size), offset) =>
                if m.contains(offset) then
                  assert(!m(offset).cells(0).growSize(byteSize))
                  m
                else
                  val node = DSN(Some(this), byteSize)
                  node.allocationRegions.add(StackLocation(pos.toShortString, proc, byteSize))
                  node.flags.stack = true
                  node.addCell(0, byteSize)
                  m + (offset -> node)
              case _ => m
        }
      case _ => m
  private def stackBuilder(pos: CFGPosition, m: Map[BigInt, DSN]): Map[BigInt, DSN] = {
    pos match
      case Assign(variable: Variable, expr: Expr, _) =>
        expr match
          case MemoryLoad(mem, index, endian, size) =>
            visitStackAccess(pos, index, size, m)
          case _ => m
      case MemoryAssign(mem, index: Expr, value: Expr, endian, size: Int, label) =>
        visitStackAccess(pos, index, size, m)
      case _ => m

  }


  // make all globals
  private val swappedOffsets = globalOffsets.map(_.swap)
  val globalMapping: mutable.Map[(BigInt, BigInt), (DSN, BigInt)] = mutable.Map[(BigInt, BigInt), (DSN, BigInt)]()
  globals.foreach(
    global =>
      val node = DSN(Some(this), global.size)
      node.allocationRegions.add(DataLocation(global.name, global.address, global.size/8))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update((global.address, global.address + global.size/8), (node, 0))
  )

  globals.foreach(
    global =>
      var address = global.address
      breakable {
        while swappedOffsets.contains(address) do
          val relocatedAddress = swappedOffsets(address)
          if relocatedAddress == address then
            break

          var field: BigInt = 0
          val node: DSN = isGlobal(relocatedAddress) match
            case Some(value) =>
              field = relocatedAddress - value._1._1
              val node = value._2._1
              node.addCell(field, 8)
              node

            case None =>
              val node = DSN(Some(this))
              node.allocationRegions.add(DataLocation(s"Relocated_$relocatedAddress", relocatedAddress, 8))
              node.flags.global = true
              node.flags.incomplete = true
              globalMapping.update((relocatedAddress, relocatedAddress + 8), (node, 0))
              node

          pointTo.update(node.cells(field), (isGlobal(address).get._2._1.cells(0), 0))
          address = relocatedAddress
      }
  )

  externalFunctions.foreach(
    external =>
      val node = DSN(Some(this))
      node.allocationRegions.add(DataLocation(external.name, external.offset, 0))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update((external.offset, external.offset), (node, 0))
  )



  // determine if an address is a global and return the corresponding global if it is.
  def isGlobal(address: BigInt): Option[((BigInt, BigInt), (DSN, BigInt))] =
    var global: Option[((BigInt, BigInt), (DSN, BigInt))]  = None
    breakable {
      for (elem <- globalMapping) {
        val range = elem._1
        if address >= range._1 && (address < range._2 || (range._1 == range._2 && range._2 == address)) then
          global = Some(elem)
          break
      }
    }
    global

  private def replaceInEV(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    varToCell.foreach(
      (pos, m) =>
        m.foreach {
          case (variable, (cell, offset)) =>
            if cell.equals(oldCell) then
              m.update(variable, (newCell, offset + internalOffsetChange))
        }
    )

    formals.foreach{
      case (variable, (cell, offset)) =>
        if cell.equals(oldCell) then
          formals.update(variable, (newCell, offset + internalOffsetChange))
    }

  private def replaceInPointTo(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    pointTo.foreach {
      case (pointer, (cell: DSC, pointeeInternal: BigInt)) =>
        if cell.equals(oldCell) then
          pointTo.update(pointer, (newCell, pointeeInternal + internalOffsetChange))
    }

  private def replaceInGlobals(oldCell: DSC, newCell: DSC) =
    if oldCell.node.isDefined then
      globalMapping.foreach {
        case (key, tuple) =>
          val node = tuple._1
          val offset = tuple._2
          if node.equals(oldCell.node.get) then
            globalMapping.update(key, (newCell.node.get, offset))
      }

  private def replaceInStack(oldCell: DSC, newCell: DSC) =
    if oldCell.node.isDefined then
      stackMapping.foreach{
        case (offset, node) =>
          if node.equals(oldCell.node.get) then
            stackMapping.update(offset, newCell.node.get)
      }

  private def replaceInCallSites(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    callsites.foreach(
      callSite =>
        callSite.returnCells.foreach{
          case (variable: Variable, (cell: DSC, internal: BigInt)) =>
            if cell.equals(oldCell) then
              callSite.returnCells.update(variable, (newCell, internal + internalOffsetChange))
        }

        callSite.paramCells.foreach{
          case (variable: Variable, (cell: DSC, internal: BigInt)) =>
            if cell.equals(oldCell) then
              callSite.paramCells.update(variable, (newCell, internal + internalOffsetChange))
        }
    )
      
  def replace(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    replaceInEV(oldCell, newCell, internalOffsetChange)
    replaceInPointTo(oldCell, newCell, internalOffsetChange)
    replaceInGlobals(oldCell, newCell)
    replaceInStack(oldCell, newCell)
    replaceInCallSites(oldCell, newCell, internalOffsetChange)

  def getPointee(cell: DSC): (DSC, BigInt) =
    if !pointTo.contains(cell) then
      val node = DSN(Some(this))
      pointTo.update(cell, (node.cells(0), 0))
    pointTo(cell)

  def getPointeeAdjusted(cell:DSC): DSC =
    val pointee = getPointee(cell)
    adjust(pointee)

  def getCells(pos: CFGPosition, arg: Variable): Set[(DSC, BigInt)] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[(DSC, BigInt)]()) {
        (s, defintion) =>
          s + varToCell(defintion)(arg)
      }
    else
      Set(formals(arg))

  def collectNodes =
    nodes.clear()
    nodes.addAll(formals.values.map(_._1.node.get))
    varToCell.values.foreach(
      value => nodes.addAll(value.values.map(_._1.node.get))
    )
    nodes.addAll(stackMapping.values)
    nodes.addAll(globalMapping.values.map(_._1))
  def collapseNode(node: DSN): Unit =
    val collapedCell = DSC(Option(node), 0)
    val e = DSC(None, 0)

    var pointeeInternalOffset: BigInt = 0
    val cell = node.cells.foldLeft(e) {
      (c, field) =>

        if pointTo.contains(field._2) && pointTo(field._2) == field._2 then
          pointTo.update(field._2, (collapedCell, 0))
          c
        else if pointTo.contains(field._2) then
          val (pointeeCell, internalOffset) = getPointee(field._2)
          if internalOffset > pointeeInternalOffset then
            pointeeInternalOffset = internalOffset
          mergeCells(c, getPointeeAdjusted(field._2))
        else
          c
    }

    node.cells.values.foreach(
      cell =>
        replace(cell, collapedCell, 0)
        pointTo.foreach {
          case (pointer, pointee) =>
            if pointer.equals(cell) then
              pointTo.remove(pointer)
              pointTo.update(collapedCell, pointee)
        }
    )

    node.flags.collapsed = true


    node.cells.clear()
    node.cells.addOne(0, collapedCell)
    if cell.node.isDefined then
      pointTo.update(node.cells(0), (cell, pointeeInternalOffset))

  def optionalCollapse(node: DSN): Unit = {
    var lastOffset: BigInt = -1
    var lastAccess: BigInt = -1
    val removed = mutable.Set[BigInt]()
    node.cells.toSeq.sortBy(_._1).foreach {
      case (offset: BigInt, cell: DSC) =>
        if lastOffset + lastAccess > offset then
          val result = mergeNeighbours(node.cells(lastOffset), cell)
          removed.add(offset)
          lastAccess = result.largestAccessedSize
        else
          lastOffset = offset
          lastAccess = cell.largestAccessedSize
    }
    removed.foreach(node.cells.remove)
  }

  def mergeNeighbours(cell1: DSC, cell2: DSC): DSC =
    require(cell1.node.equals(cell2.node) && cell1.offset < cell2.offset)
    if pointTo.contains(cell2) then
      if pointTo.contains(cell1) then
        val (cell1Pointee: DSC, pointee1Internal: BigInt) = getPointee(cell1)
        val (cell2Pointee: DSC, pointee2Internal: BigInt) = getPointee(cell2)
        val result = mergeCells(getPointeeAdjusted(cell1), getPointeeAdjusted(cell2))
        assert(pointTo(cell1)._1.equals(result))
        pointTo.update(cell1, (result,pointee2Internal.max(pointee1Internal)))
      else
        pointTo.update(cell1, getPointee(cell2))
      pointTo.remove(cell2)
    val internalOffsetChange = cell2.offset - cell1.offset
    replace(cell2, cell1, internalOffsetChange)
    cell1.growSize((cell2.offset - cell1.offset) + cell2.largestAccessedSize) // might cause another collapse
    cell1


  def mergeCells(cell1: DSC, cell2: DSC): DSC =

    if cell1.equals(cell2) then
      cell1
    else if cell1.node.isDefined && cell1.node.equals(cell2.node) then
      collapseNode(cell1.node.get)
      cell1.node.get.cells(0)
    else if cell1.node.isEmpty then
      replace(cell1, cell2, 0)
      cell2
    else if cell1.node.get.collapsed || cell2.node.get.collapsed then
      val node1 = cell1.node.get
      val node2 = cell2.node.get
      collapseNode(node1)
      collapseNode(node2)
      node2.allocationRegions.addAll(node1.allocationRegions)
      node2.flags.join(node1.flags)
      if pointTo.contains(node1.cells(0)) then
        if pointTo.contains(node2.cells(0)) then
          val (pointee1: DSC, internal1: BigInt) = getPointee(node1.cells(0))
          val (pointee2: DSC, internal2: BigInt) = getPointee(node2.cells(0))
          val result = mergeCells(getPointeeAdjusted(node1.cells(0)), getPointeeAdjusted(node2.cells(0)))
          pointTo.update(node2.cells(0), (result, internal1.max(internal2)))
        else 
          pointTo.update(node2.cells(0), getPointee(node1.cells(0)))
      pointTo.remove(node1.cells(0))
      replace(node1.cells(0), node2.cells(0), 0)
      node2.cells(0)
    else
      
      var delta = cell1.offset - cell2.offset
      var node1 = cell1.node.get
      var node2 = cell2.node.get
      if cell1.offset < cell2.offset then
        delta = cell2.offset - cell1.offset
        node1 = cell2.node.get
        node2 = cell1.node.get


      val cells : Seq[(BigInt, DSC)] = (node1.cells.toSeq ++ node2.cells.foldLeft(Seq[(BigInt, DSC)]()){
        (s, tuple) =>
          val offset = tuple._1
          val cell = tuple._2
          s:+ ((offset + delta, cell))
      }).sortBy(_._1)

      var lastOffset: BigInt = -1
      var lastAccess: BigInt = -1
      val resultNode = DSN(Some(this))
      resultNode.allocationRegions.addAll(node1.allocationRegions ++ node2.allocationRegions)
      resultNode.flags.join(node1.flags)
      resultNode.flags.join(node2.flags)
      if node2.flags.global then
        globalMapping.foreach{
          case ((start: BigInt, end: BigInt), (node:DSN, offset: BigInt)) =>
            if node.equals(node2) then
              globalMapping.update((start, end), (node, offset + delta))
        }
      val resultCells: mutable.Map[BigInt, (Set[DSC], BigInt)] = mutable.Map()
      cells.foreach {
        case (offset: BigInt, cell: DSC) =>
          if (lastOffset + lastAccess > offset) || lastOffset == offset then // includes this cell
            if (offset - lastOffset) + cell.largestAccessedSize > lastAccess then
              lastAccess = (offset - lastOffset) + cell.largestAccessedSize
            resultCells.update(lastOffset, (resultCells(lastOffset)._1 + cell, lastAccess))
          else
            lastOffset = offset
            lastAccess = cell.largestAccessedSize
            resultCells.update(lastOffset, (Set(cell), lastAccess))
      }

      resultCells.foreach {
        case (offset: BigInt, (cells: Set[DSC], largestAccess: BigInt)) =>
          val collapsedCell = resultNode.addCell(offset, largestAccess)
          val outgoing: Set[(DSC, BigInt)] = cells.foldLeft(Set[(DSC, BigInt)]()){
            (set, cell) =>
              // replace incoming edges
              if cell.node.get.equals(node2) then
                replace(cell, collapsedCell, delta + cell.offset - offset)
              else
                assert(cell.node.get.equals(node1))
                replace(cell, collapsedCell, cell.offset - offset)

              // collect outgoing edges
              if pointTo.contains(cell) then
                val pointee = getPointee(cell)
                pointTo.remove(cell)
                set + pointee
              else
                set
          }
          // replace outgoing edges
          if outgoing.size == 1 then
            pointTo.update(collapsedCell, outgoing.head)
          else if outgoing.size > 1 then
            var internal = outgoing.head._2
            val result = outgoing.tail.foldLeft(outgoing.head._1){
              (result, pointee) =>
                val cell = pointee._1
                val pointeeInternal = pointee._2
                internal = internal.max(pointeeInternal)
                mergeCells(result, cell)
            }
            pointTo.update(collapsedCell, (result, internal))
      }
      
      if cell1.offset >= cell2.offset then
        resultNode.getCell(cell1.offset)
      else
        resultNode.getCell(cell2.offset)


  private def isFormal(pos: CFGPosition, variable: Variable): Boolean =
    !reachingDefs(pos).contains(variable)

      
  val formals: mutable.Map[Variable, (DSC, BigInt)] = mutable.Map()
  val varToCell: mutable.Map[CFGPosition, mutable.Map[Variable, (DSC, BigInt)]] = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(mutable.Map[CFGPosition, mutable.Map[Variable, (DSC, BigInt)]]()) {
    (m, pos) =>
      pos match
        case Assign(variable, value , label) =>
          value.variables.foreach(
            v =>
              if isFormal(pos, v) then
                val node = DSN(Some(this))
                node.flags.incomplete = true
                nodes.add(node)
                formals.update(v, (node.cells(0), 0))
          )
          val node = DSN(Some(this))
          m +=(pos -> mutable.Map(variable -> (node.cells(0), 0)))
        case DirectCall(proc, target, label) if proc.name == "malloc" =>
          val node = DSN(Some(this))
           m += (pos -> mutable.Map(mallocRegister -> (node.cells(0), 0)))
        case DirectCall(proc, target, label) if writesTo.contains(proc) =>
          val result: Map[Variable, (DSC, BigInt)] = writesTo(proc).foldLeft(Map[Variable, (DSC, BigInt)]()){
            (n, variable) =>
              val node = DSN(Some(this))
              n + (variable -> (node.cells(0), 0))
          }
          m += (pos -> result.to(mutable.Map))
        case MemoryAssign(memory, index: Expr, expr: Expr, endian, size: Int, label) if unwrapPaddingAndSlicing(expr).isInstanceOf[Variable] =>
          val value: Variable = unwrapPaddingAndSlicing(expr).asInstanceOf[Variable]
          if isFormal(pos, value) then
            val node = DSN(Some(this))
            node.flags.incomplete = true
            nodes.add(node)
            formals.update(value, (node.cells(0), 0))
          m
        case _ => m
  }

  def cloneSelf(): DSG =
    val newGraph = DSG(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params)
    assert(formals.size == newGraph.formals.size)
    val idToNode: mutable.Map[Int, DSN] = mutable.Map()
    formals.foreach{
      case (variable: Variable, (cell: DSC, internalOffset: BigInt)) =>
//        assert(newGraph.formals.contains(variable))
        val node = cell.node.get
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.formals.update(variable, (idToNode(node.id).cells(cell.offset), internalOffset))
    }

    varToCell.foreach {
      case (position: CFGPosition, values: mutable.Map[Variable, (DSC, BigInt)]) =>
//        assert(newGraph.varToCell.contains(position))
        if !newGraph.varToCell.contains(position) then
          newGraph.varToCell.update(position, mutable.Map[Variable, (DSC, BigInt)]())
        values.foreach{
          case (variable: Variable, (cell: DSC, internalOffset: BigInt)) =>
//            assert(newGraph.varToCell(position).contains(variable))
            val node = cell.node.get
            if !idToNode.contains(node.id) then
              val newNode = node.cloneSelf(newGraph)
              idToNode.update(node.id, newNode)
            newGraph.varToCell(position).update(variable, (idToNode(node.id).cells(cell.offset), internalOffset))
        }
    }

    stackMapping.foreach{
      case (offset, node) =>
        assert(newGraph.stackMapping.contains(offset))
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.stackMapping.update(offset, idToNode(node.id))
    }

    globalMapping.foreach {
      case ((start: BigInt, end: BigInt), (node: DSN, internalOffset: BigInt)) =>
        assert(newGraph.globalMapping.contains((start, end)))
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.globalMapping.update((start, end), (idToNode(node.id), internalOffset))
    }

    newGraph.pointTo.clear()
    pointTo.foreach {
      case (cell1: DSC, (cell2: DSC, internalOffset: BigInt)) =>
        val node1 = cell1.node.get
        val node2 = cell2.node.get
        if !idToNode.contains(node1.id) then
          val newNode1 = node1.cloneSelf(newGraph)
          idToNode.update(node1.id, newNode1)

        if !idToNode.contains(node2.id) then
          val newNode2 = node2.cloneSelf(newGraph)
          idToNode.update(node2.id, newNode2)

        newGraph.pointTo.update(idToNode(node1.id).cells(cell1.offset), (idToNode(node2.id).cells(cell2.offset), internalOffset))
    }

    callsites.foreach(
      callSite =>
        val cs = CallSite(callSite.call, newGraph)
        newGraph.callsites.add(cs)
        assert(cs.paramCells.keySet.equals(callSite.paramCells.keySet))
        callSite.paramCells.foreach{
          case (variable: Variable, (cell: DSC, internal: BigInt)) =>
            assert(cs.paramCells.contains(variable))
            val id = cell.node.get.id
            cs.paramCells.update(variable, (idToNode(id).cells(cell.offset), internal))
        }

        callSite.returnCells.foreach{
          case (variable: Variable, (cell: DSC, internal: BigInt)) =>
            assert(cs.returnCells.contains(variable))
            val id = cell.node.get.id
            cs.returnCells.update(variable, (idToNode(id).cells(cell.offset), internal))
        }
    )



    newGraph.nodes.addAll(idToNode.values)
    newGraph

}

class Flags() {
  var collapsed = false
  var stack = false
  var heap = false
  var global = false
  var unknown = false
  var read = false
  var modified = false
  var incomplete = false
  var foreign = false

  def join(other: Flags): Unit =
    collapsed = collapsed || other.collapsed
    stack = other.stack || stack
    heap = other.heap || heap
    global = other.global || global
    unknown =other.unknown || unknown
    read = other.read || read
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    foreign = other.foreign && foreign
}

class DSN(val graph: Option[DSG], var size: BigInt = 0, val id: Int =  NodeCounter.getCounter) {

//  var collapsed = false
  var flags = Flags()
  def collapsed = flags.collapsed

  val allocationRegions: mutable.Set[MemoryLocation] = mutable.Set()

  val cells: mutable.Map[BigInt, DSC] = mutable.Map()
  this.addCell(0, 0)

  def updateSize(newSize: BigInt): Unit =

    if newSize > size then
      size = newSize

  def getCell(offset: BigInt): DSC =
    if collapsed then
      cells(0)
    else if !cells.contains(offset) then
      var result: Option[DSC] = None
      cells.foreach {
        case (start: BigInt, cell: DSC) =>
          if start < offset && offset < (start + cell.largestAccessedSize) then
            result = Some(cell)
      }
      result match
        case Some(value) => value
        case None =>
          ???
    else
      cells(offset)
      
      
  def addCell(offset: BigInt, size: BigInt) : DSC =
    this.updateSize(offset + size)
    if collapsed then
      cells(0)
    else if !cells.contains(offset) then
      val cell = DSC(Some(this), offset)
      cells.update(offset, cell)
      cell.growSize(size)
      cell
    else
      cells(offset).growSize(size)
      cells(offset)

  def cloneSelf(graph: DSG) : DSN =
    val node = DSN(Some(graph), this.size)
    node.allocationRegions.addAll(this.allocationRegions)
    node.flags.join(this.flags)
    cells.foreach{
      case (offset: BigInt, cell: DSC) =>
        node.addCell(offset, cell.largestAccessedSize)
    }
    node

  def cloneNode(from: DSG, to: DSG): Unit =
//    assert(from.nodes.contains(this)) TODO update nodes after each phase for to check this assertion
    if !to.nodes.contains(this) then
      to.nodes.add(this)


      from.varToCell.foreach(
        t =>
          val pos = t._1
          val varMap = t._2
          varMap.foreach{
            case (variable: Variable, (cell: DSC, internal: BigInt)) =>
              if cell.node.get.equals(this) then
                to.varToCell.update(
                  pos,
                  to.varToCell.getOrElseUpdate(pos,
                    mutable.Map[Variable, (DSC, BigInt)]()) ++ Map(variable -> (cell, internal))
                )
          }
      )
      from.formals.foreach{
        case (variable: Variable, (cell: DSC, internal: BigInt)) =>
          if cell.node.get.equals(this) then
            to.varToCell.update(
              begin(from.proc),
              to.varToCell.getOrElseUpdate(begin(from.proc),
                mutable.Map[Variable, (DSC, BigInt)]()) ++ Map(variable -> (cell, internal))
            )
      }

      cells.foreach {
        case (offset: BigInt, cell: DSC) =>
        if from.pointTo.contains(cell) then
          val pointee = from.getPointee(cell)
          pointee._1.node.get.cloneNode(from, to)
          to.pointTo.update(cell, pointee)
      }

  override def equals(obj: Any): Boolean =
    obj match
      case node: DSN =>
        this.id == node.id
      case _ => false

  override def hashCode(): Int = id

  override def toString: String = s"Node($id, $allocationRegions ${if collapsed then ", collapsed" else ""})"
}

/**
 * a cell in DSA
 * @param node the node this cell belongs to
 * @param offset the offset of the cell
 */
case class DSC(node: Option[DSN], offset: BigInt)
{
  var largestAccessedSize: BigInt = 0

  def growSize(size: BigInt): Boolean =
    if size > largestAccessedSize then
      largestAccessedSize = size
      true
    else false

  override def toString: String = s"Cell(${if node.isDefined then node.get.toString else "NONE"}, $offset)"
}

/**
 * represents a direct call in DSA
 * @param call instance of the call
 * @param graph caller's DSG
 */
class CallSite(val call: DirectCall, val graph: DSG) {
  val proc = call.target
  val paramCells: mutable.Map[Variable, (DSC, BigInt)] = graph.params(proc).foldLeft(mutable.Map[Variable, (DSC, BigInt)]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> (node.cells(0), 0))
  }
  val returnCells: mutable.Map[Variable, (DSC, BigInt)] = graph.writesTo(proc).foldLeft(mutable.Map[Variable, (DSC, BigInt)]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> (node.cells(0), 0))
  }
}

def unwrapPaddingAndSlicing(expr: Expr): Expr =
  expr match
    case literal: Literal => literal
    case Repeat(repeats, body) => Repeat(repeats, unwrapPaddingAndSlicing(body))
    case SignExtend(extension, body) => SignExtend(extension, unwrapPaddingAndSlicing(body))
    case UnaryExpr(op, arg) => UnaryExpr(op, arg)
    case BinaryExpr(op, arg1, arg2) => BinaryExpr(op, unwrapPaddingAndSlicing(arg1), unwrapPaddingAndSlicing(arg2))
    case MemoryLoad(mem, index, endian, size) => MemoryLoad(mem, unwrapPaddingAndSlicing(index), endian, size)
    case variable: Variable => variable
    case Extract(end, start, body) /*if start == 0 && end == 32*/ => unwrapPaddingAndSlicing(body) // this may make it unsound
    case ZeroExtend(extension, body) => unwrapPaddingAndSlicing(body)
    case _ => expr

def decToBinary(n: BigInt): Array[Int] = {
  val binaryNum: Array[Int] = new Array[Int](64)
  var i = 0
  var num = n
  while (num > 0) {
    binaryNum(i) = (num % BigInt(2)).intValue
    num = num / 2
    i += 1
  }
  binaryNum
}

def twosComplementToDec(binary: Array[Int]): BigInt = {
  var result: BigInt = BigInt(0)
  var counter: Int = 0
  binary.foreach(
    n =>
      if counter == binary.length - 1 && n == 1 then
        result = result - BigInt(2).pow(counter)
      else if n == 1 then
        result = result + BigInt(2).pow(counter)
      counter += 1
  )
  result
}

def adjust(cell: DSC, internalOffset: BigInt): DSC =
  val node = cell.node.get
  node.addCell(cell.offset+internalOffset, 0)

def adjust(tuple: (DSC, BigInt)): DSC =
  val cell = tuple._1
  val internal = tuple._2
  adjust(cell, internal)

// minimum  2's complement 64 bit negative integer
val BITVECNEGATIVE: BigInt = new BigInt(new BigInteger("9223372036854775808"))



