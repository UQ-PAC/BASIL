package analysis

import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal}

import scala.util.control.Breaks.{break, breakable}
import java.math.BigInteger
import scala.collection.mutable

import scala.collection.mutable

object NodeCounter {
  var counter: Int = 0

  def getCounter: Int =
    counter = counter + 1
    counter


}

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
  val pointTo: mutable.Map[DSC, DSC] = mutable.Map()
  val callsites: mutable.Set[CallSite] = mutable.Set()

  val mallocRegister = Register("R0", BitVecType(64))
  val stackPointer = Register("R31", BitVecType(64))

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
              case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), symOffset) =>
                offset = offset + symOffset
                if m.contains(offset) then
                  assert(!m(offset).cells(0).growSize(byteSize))
                  m
                else
                  val node = DSN(Some(this), byteSize)
                  node.allocationRegions.add(StackRegion2(pos.toShortString, proc, byteSize))
                  node.flags.stack = true
                  node.addCell(0, byteSize)
                  m + (offset -> node)
              case _ => m
        }
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
        varToSym(pos)(arg).foldLeft(m) {
          (m, sym) =>
            sym match
              case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), offset) =>
                if m.contains(offset) then
                  assert(!m(offset).cells(0).growSize(byteSize))
                  m
                else
                  val node = DSN(Some(this), byteSize)
                  node.allocationRegions.add(StackRegion2(pos.toShortString, proc, byteSize))
                  node.flags.stack = true
                  node.addCell(0, byteSize)
                  m + (offset -> node)
              case _ => m
        }
      case _ => m
  private def stackBuilder(pos: CFGPosition, m: Map[BigInt, DSN]): Map[BigInt, DSN] = {
    pos match
      case LocalAssign(variable: Variable, expr: Expr, _) =>
        expr match
          case MemoryLoad(mem, index, endian, size) =>
            visitStackAccess(pos, index, size, m)
          case _ => m
      case MemoryAssign(mem, MemoryStore(mem2, index, value, endian, size), label) =>
        visitStackAccess(pos, index, size, m)
      case _ => m

  }


  // make all globals
  private val swappedOffsets = globalOffsets.map(_.swap)
  val globalMapping: mutable.Map[(BigInt, BigInt), (DSN, BigInt)] = globals.foldLeft(mutable.Map[(BigInt, BigInt), (DSN, BigInt)]()) {
    (m, global) =>
      var address: BigInt = global.address
      if swappedOffsets.contains(address) then
        address = swappedOffsets(address)
      val node = DSN(Some(this), global.size)
      node.allocationRegions.add(DataRegion2(global.name, address, global.size))
      node.flags.global = true
      node.flags.incomplete = true
      m + ((address, address + global.size/8) -> (node, 0))

  }
  externalFunctions.foreach(
    external =>
      var address: BigInt = external.offset
      if swappedOffsets.contains(address) then
        address = swappedOffsets(address)
      val node = DSN(Some(this))
      node.allocationRegions.add(DataRegion2(external.name, address, 0))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update((address, address), (node, 0))
  )


  // determine if an address is a global and return the corresponding global if it is.
  def isGlobal(address: BigInt): Option[((BigInt, BigInt), (DSN, BigInt))] =
    for (elem <- globalMapping) {
      val range = elem._1
      if address >= range._1 && address <= range._2 then
        return Some(elem)
    }
    None

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

  private def replaceInPointTo(oldCell: DSC, newCell: DSC) =
    pointTo.foreach {
      case (pointer, pointee) =>
        if pointee.equals(oldCell) then
          pointTo.update(pointer, newCell)
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

  private def replaceInCallSites(oldCell: DSC, newCell: DSC) =
    callsites.foreach(
      callSite =>
        callSite.returnCells.foreach{
          case (variable: Variable, cell: DSC) =>
            if cell.equals(oldCell) then
              callSite.returnCells.update(variable, newCell)
        }

        callSite.paramCells.foreach{
          case (variable: Variable, cell: DSC) =>
            if cell.equals(oldCell) then
              callSite.paramCells.update(variable, newCell)
        }
    )
      
  private def replace(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    replaceInEV(oldCell, newCell, internalOffsetChange)
    replaceInPointTo(oldCell, newCell)
    replaceInGlobals(oldCell, newCell)
    replaceInStack(oldCell, newCell)
    replaceInCallSites(oldCell, newCell)

  def getPointee(cell: DSC): DSC =
    if !pointTo.contains(cell) then
      val node = DSN(Some(this))
      pointTo.update(cell, node.cells(0))
    pointTo(cell)

  def getCells(pos: CFGPosition, arg: Variable): Set[(DSC, BigInt)] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[(DSC, BigInt)]()) {
        (s, defintion) =>
          s + varToCell(defintion)(arg)
      }
    else
      Set(formals(arg))

  def collapseNode(node: DSN): Unit =
    val collapedCell = DSC(Option(node), 0)
    val e = DSC(None, 0)

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
      pointTo.update(node.cells(0), cell)

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
        mergeCells(getPointee(cell1), getPointee(cell2))
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
          pointTo.update(node2.cells(0), mergeCells(getPointee(node1.cells(0)), getPointee(node2.cells(0))))
        else 
          pointTo.update(node2.cells(0), getPointee(node1.cells(0)))
      pointTo.remove(node1.cells(0))
      replace(node1.cells(0), node2.cells(0), 0)
      node2.cells(0)
    else if cell1.node.get.allocationRegions.isEmpty && cell1.offset == 0 && cell1.node.get.cells.size == 1 && cell1.largestAccessedSize == 0 && //
      !pointTo.contains(cell1) && pointTo.values.foldLeft(true) {
      (condition, cell) => cell != cell1 && condition
    } then
      replace(cell1, cell2, 0)
      cell2
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
            resultCells.update(offset, (resultCells(offset)._1 + cell, lastAccess))
          else
            lastOffset = offset
            lastAccess = cell.largestAccessedSize
            resultCells.update(lastOffset, (Set(cell), lastAccess))
      }

      resultCells.foreach {
        case (offset: BigInt, (cells: Set[DSC], largestAccess: BigInt)) =>
          val collapsedCell = resultNode.addCell(offset, largestAccess)
          val outgoing: Set[DSC] = cells.foldLeft(Set()){
            (set, cell) =>
              // replace incoming edges
              if cell.node.get.equals(node2) then
                replace(cell, collapsedCell, delta + cell.offset - offset) // TODO reconsider offsets
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
          // replace outgoing edges TODO might have to move this out after all cells have been processed 
          if outgoing.size == 1 then
            pointTo.update(collapsedCell, outgoing.head)
          else if outgoing.size > 1 then
            val result = outgoing.tail.foldLeft(outgoing.head){
              (result, cell) =>
                mergeCells(result, cell)
            }
            pointTo.update(collapsedCell, result)
      }
      
      if cell1.offset >= cell2.offset then
        resultNode.cells(cell1.offset)
      else
        resultNode.cells(cell2.offset)


  private def isFormal(pos: CFGPosition, variable: Variable): Boolean =
    !reachingDefs(pos).contains(variable)

      
  val formals: mutable.Map[Variable, (DSC, BigInt)] = mutable.Map()
  val varToCell: Map[CFGPosition, mutable.Map[Variable, (DSC, BigInt)]] = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[CFGPosition, mutable.Map[Variable, (DSC, BigInt)]]()) {
    (m, pos) =>
      pos match
        case LocalAssign(variable, value , label) =>
          value.variables.foreach(
            v =>
              if isFormal(pos, v) then
                val node = DSN(Some(this))
                node.flags.incomplete = true
                node.rep = "formal"
                nodes.add(node)
                formals.update(v, (node.cells(0), 0))
          )
          val node = DSN(Some(this))
          node.rep = "ssa"
          m + (pos -> mutable.Map(variable -> (node.cells(0), 0)))
        case DirectCall(proc, target, label) if proc.name == "malloc" =>
          val node = DSN(Some(this))
          node.rep = "ssa"
           m + (pos -> mutable.Map(mallocRegister -> (node.cells(0), 0)))
        case DirectCall(proc, target, label) if writesTo.contains(proc) =>
          val result: Map[Variable, (DSC, BigInt)] = writesTo(proc).foldLeft(Map[Variable, (DSC, BigInt)]()){
            (n, variable) =>
              val node = DSN(Some(this))
              node.rep = "ssa"
              n + (variable -> (node.cells(0), 0))
          }
          m + (pos -> result.to(mutable.Map))
        case MemoryAssign(memory, MemoryStore(mem, index, expr: Expr, endian, size), label) if unwrapPaddingAndSlicing(expr).isInstanceOf[Variable] =>
          val value: Variable = unwrapPaddingAndSlicing(expr).asInstanceOf[Variable]
          if isFormal(pos, value) then
            val node = DSN(Some(this))
            node.flags.incomplete = true
            node.rep = "formal"
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
        assert(newGraph.formals.contains(variable))
        val node = cell.node.get
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.formals.update(variable, (idToNode(node.id).cells(cell.offset), internalOffset))
    }

    varToCell.foreach {
      case (position: CFGPosition, values: mutable.Map[Variable, (DSC, BigInt)]) =>
        assert(newGraph.varToCell.contains(position))
        values.foreach{
          case (variable: Variable, (cell: DSC, internalOffset: BigInt)) =>
            assert(newGraph.varToCell(position).contains(variable))
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

    callsites.foreach(
      callSite =>
        val cs = CallSite(callSite.call, newGraph)
        newGraph.callsites.add(cs)
        assert(cs.paramCells.keySet.equals(callSite.paramCells.keySet))
        callSite.paramCells.foreach{
          case (variable: Variable, cell: DSC) =>
            assert(cs.paramCells.contains(variable))
            val id = cell.node.get.id
            cs.paramCells.update(variable, idToNode(id).cells(cell.offset))
        }

        callSite.returnCells.foreach{
          case (variable: Variable, cell: DSC) =>
            assert(cs.returnCells.contains(variable))
            val id = cell.node.get.id
            cs.returnCells.update(variable, idToNode(id).cells(cell.offset))
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

  val allocationRegions: mutable.Set[MemoryRegion2] = mutable.Set()

  var rep: String = ""

//  var size: BigInt = region match
//    case Some(value) => value match
//      case DataRegion2(regionIdentifier, start, size) => size
//      case HeapRegion2(regionIdentifier, proc, size) => size
//      case StackRegion2(regionIdentifier, proc, size) => size
//      case UnknownRegion2(regionIdentifier, proc) => 0
//    case None => 0

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
    assert(from.equals(graph.get))
    if !to.nodes.contains(this) then
      to.nodes.add(this)
      cells.foreach {
        case (offset: BigInt, cell: DSC) =>
        if from.pointTo.contains(cell) then
          val pointee = from.getPointee(cell)
          pointee.node.get.cloneNode(to,from)
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

class CallSite(val call: DirectCall, val graph: DSG) {
  val proc = call.target
  val paramCells: mutable.Map[Variable, DSC] = graph.params(proc).foldLeft(mutable.Map[Variable, DSC]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> node.cells(0))
  }
  val returnCells: mutable.Map[Variable, DSC] = graph.writesTo(proc).foldLeft(mutable.Map[Variable, DSC]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> node.cells(0))
  }
}

def unwrapPaddingAndSlicing(expr: Expr): Expr =
  expr match
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

val BITVECNEGATIVE: BigInt = new BigInt(new BigInteger("9223372036854775808"))



