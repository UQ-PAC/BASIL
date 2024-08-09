package analysis

import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, Memory, MemoryAssign, MemoryLoad, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, begin, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal, SymbolTableEntry}

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
          globals: Set[SymbolTableEntry], globalOffsets: Map[BigInt, BigInt],
          externalFunctions: Set[ExternalFunction],
          val reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
          val writesTo: Map[Procedure, Set[Register]],
          val params: Map[Procedure, Set[Variable]]
         ) {

  // DSNodes owned by this graph, only updated once analysis is done,
  val nodes: mutable.Set[DSN] = mutable.Set()

  // this is mapping of point-relations in the graph
  val pointTo: mutable.Map[DSC, Slice] = mutable.Map()

  // represent callees in proc
  val callsites: mutable.Set[CallSite] = mutable.Set()

  val mallocRegister = Register("R0", 64)
  val stackPointer = Register("R31", 64)

  // this is the mapping from offsets/positions on the stack to their representative DS nodes
  val stackMapping: mutable.Map[BigInt, DSN] =
    computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[BigInt, DSN]()) {
      (results, pos) => stackBuilder(pos, results)
    }.to(collection.mutable.Map)


  /**
   * this function takes a stackMapping and updates it based on a memory load and memory store
   * @param pos memory load or store IL position
   * @param index memory location of load or store
   * @param size size of the load or store
   * @param m stack mapping
   * @return updated stack mapping
   */
  private def visitStackAccess(pos: CFGPosition, index: Expr, size: Int, m: Map[BigInt, DSN]) : Map[BigInt, DSN] =
    val byteSize = (size.toDouble / 8).ceil.toInt
    index match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        evaluateExpression(arg2, constProp(pos)).isDefined =>
        var offset = evaluateExpression(arg2, constProp(pos)).get.value
        varToSym(pos)(arg1).foldLeft(m) { // go through all the symbolic accesses tied to arg1 at pos
          (m, sym) =>
            sym match
              case SymbolicAccess(accessor, StackLocation(regionIdentifier, proc, size), symOffset) => // only consider stack accesses
                offset = offset + symOffset
                createStackMapping(pos.toShortString, offset, m, byteSize)
              case _ => m
        }
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
        varToSym(pos)(arg).foldLeft(m) {
          (m, sym) =>
            sym match
              case SymbolicAccess(accessor, StackLocation(regionIdentifier, proc, size), offset) =>
                createStackMapping(pos.toShortString, offset, m, byteSize)
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

  private def createStackMapping(label: String, offset: BigInt, m:  Map[BigInt, DSN], byteSize: Int) :  Map[BigInt, DSN]=
    if m.contains(offset) then
      assert(!m(offset).cells(0).growSize(byteSize))
      m
    else
      val node = DSN(Some(this), byteSize)
      node.allocationRegions.add(StackLocation(label, proc, byteSize))
      node.flags.stack = true
      node.addCell(0, byteSize)
      m + (offset -> node)


  private val swappedOffsets = globalOffsets.map(_.swap)

  // creates the globals from the symbol tables
  val globalMapping: mutable.Map[AddressRange, Field] = mutable.Map[AddressRange, Field]()
  globals.foreach(
    global =>
      val node = DSN(Some(this), global.size)
      node.allocationRegions.add(DataLocation(global.name, global.address, global.size/8))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update(AddressRange(global.address, global.address + global.size/8), Field(node, 0))
  )

  // creates a global for each relocation entry in the symbol table
  // the global corresponding to the relocated address points to the global corresponding to the original address
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
              globalMapping.update(AddressRange(relocatedAddress, relocatedAddress + 8), Field(node, 0))
              node

          pointTo.update(node.cells(field), Slice(isGlobal(address).get._2._1.cells(0), 0))
          address = relocatedAddress
      }
  )

  externalFunctions.foreach(
    external =>
      val node = DSN(Some(this))
      node.allocationRegions.add(DataLocation(external.name, external.offset, 0))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update(AddressRange(external.offset, external.offset), Field(node, 0))
  )



  // determine if an address is a global and return the corresponding global if it is.
  def isGlobal(address: BigInt): Option[(AddressRange, Field)] =
    var global: Option[(AddressRange, Field)]  = None
    breakable {
      for (elem <- globalMapping) {
        val range = elem._1 // TODO
        if address >= range.start && (address < range.end || (range.start == range.end && range.end == address)) then
          global = Some(elem)
          break
      }
    }
    global

  private def replaceInEV(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    varToCell.foreach(
      (pos, m) =>
        m.foreach {
          case (variable, slice) =>
            if slice.cell.equals(oldCell) then
              m.update(variable, Slice(newCell, slice.internalOffset + internalOffsetChange))
        }
    )

    formals.foreach{
      case (variable, slice) =>
        if slice.cell.equals(oldCell) then
          formals.update(variable, Slice(newCell, slice.internalOffset + internalOffsetChange))
    }

  private def replaceInPointTo(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    pointTo.foreach {
      case (pointer, slice: Slice) =>
        if slice.cell.equals(oldCell) then
          pointTo.update(pointer, Slice(newCell, slice.internalOffset + internalOffsetChange))
    }

  private def replaceInGlobals(oldCell: DSC, newCell: DSC) =
    if oldCell.node.isDefined then
      globalMapping.foreach {
        case (key, Field(node, offset)) =>
          if node.equals(oldCell.node.get) then
            globalMapping.update(key, Field(newCell.node.get, offset))
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
          case (variable: Variable, slice: Slice) =>
            if slice.cell.equals(oldCell) then
              callSite.returnCells.update(variable, Slice(newCell, slice.internalOffset + internalOffsetChange))
        }

        callSite.paramCells.foreach{
          case (variable: Variable, slice: Slice) =>
            if slice.cell.equals(oldCell) then
              callSite.paramCells.update(variable, Slice(newCell, slice.internalOffset + internalOffsetChange))
        }
    )


  // replaces an old cell with a new cell in all the mappings and updates their slice offset if applicable
  // This is inefficient looking to replace it with a union-find approach
  def replace(oldCell: DSC, newCell: DSC, internalOffsetChange: BigInt) =
    replaceInEV(oldCell, newCell, internalOffsetChange)
    replaceInPointTo(oldCell, newCell, internalOffsetChange)
    replaceInGlobals(oldCell, newCell)
    replaceInStack(oldCell, newCell)
    replaceInCallSites(oldCell, newCell, internalOffsetChange)

  def getPointee(cell: DSC): Slice =
    if !pointTo.contains(cell) then
      val node = DSN(Some(this))
      pointTo.update(cell, Slice(node.cells(0), 0))
    pointTo(cell)

  def getPointeeAdjusted(cell:DSC): DSC =
    val pointee = getPointee(cell)
    adjust(pointee)

  def getCells(pos: CFGPosition, arg: Variable): Set[Slice] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[Slice]()) {
        (s, defintion) =>
          s + varToCell(defintion)(arg)
      }
    else
      Set(formals(arg))

  /**
   * collects all the nodes that are currently in the DSG and updates nodes member variable
   */
  def collectNodes =
    nodes.clear()
    nodes.addAll(formals.values.map(_._1.node.get))
    varToCell.values.foreach(
      value => nodes.addAll(value.values.map(_._1.node.get))
    )
    nodes.addAll(stackMapping.values)
    nodes.addAll(globalMapping.values.map(_._1))

  /**
   * Collapses the node causing it to lose field sensitivity
   */
  def collapseNode(node: DSN): Unit =
    val collapedCell = DSC(Option(node), 0)
    val e = DSC(None, 0)

    var pointeeInternalOffset: BigInt = 0
    val cell = node.cells.foldLeft(e) {
      (c, field) =>

        if pointTo.contains(field._2) && pointTo(field._2) == field._2 then
          pointTo.update(field._2, Slice(collapedCell, 0))
          c
        else if pointTo.contains(field._2) then
          val slice = getPointee(field._2)
          if slice.internalOffset > pointeeInternalOffset then
            pointeeInternalOffset = slice.internalOffset
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
      pointTo.update(node.cells(0), Slice(cell, pointeeInternalOffset))

  /**
   * this function merges all the overlapping cells in the given node
   * The node DOESN'T lose field sensitivity after this
   */
  def selfCollapse(node: DSN): Unit = {
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

  /**
   * merges two neighbouring cells into one
   */
  def mergeNeighbours(cell1: DSC, cell2: DSC): DSC =
    require(cell1.node.equals(cell2.node) && cell1.offset < cell2.offset)
    if pointTo.contains(cell2) then
      if pointTo.contains(cell1) then
        val slice1 = getPointee(cell1)
        val slice2 = getPointee(cell2)
        val result = mergeCells(getPointeeAdjusted(cell1), getPointeeAdjusted(cell2))
        assert(pointTo(cell1)._1.equals(result))
        pointTo.update(cell1, Slice(result, slice2.internalOffset.max(slice1.internalOffset)))
      else
        pointTo.update(cell1, getPointee(cell2))
      pointTo.remove(cell2)
    val internalOffsetChange = cell2.offset - cell1.offset
    replace(cell2, cell1, internalOffsetChange)
    cell1.growSize((cell2.offset - cell1.offset) + cell2.largestAccessedSize) // might cause another collapse
    cell1


  /**
   * merges two cells and unifies their nodes
   * @param cell1
   * @param cell2
   * @return the resulting cell in the unified node
   */
  def mergeCells(cell1: DSC, cell2: DSC): DSC =

    if cell1.equals(cell2) then // same cell no action required
      cell1
    else if cell1.node.isDefined && cell1.node.equals(cell2.node) then // same node different cells causes collapse
      collapseNode(cell1.node.get)
      cell1.node.get.cells(0)
    else if cell1.node.isEmpty then
      replace(cell1, cell2, 0)
      cell2
    else if cell1.node.get.collapsed || cell2.node.get.collapsed then // a collapsed node
      val node1 = cell1.node.get
      val node2 = cell2.node.get
      collapseNode(node1) // collapse the other node
      collapseNode(node2)
      node2.allocationRegions.addAll(node1.allocationRegions) // add regions and flags of node 1 to node 2
      node2.flags.join(node1.flags)
      if pointTo.contains(node1.cells(0)) then // merge the pointees of the two collapsed (single cell) nodes
        if pointTo.contains(node2.cells(0)) then
          val slice1 = getPointee(node1.cells(0))
          val slice2 = getPointee(node2.cells(0))
          val result = mergeCells(getPointeeAdjusted(node1.cells(0)), getPointeeAdjusted(node2.cells(0)))
          pointTo.update(node2.cells(0), Slice(result, slice1.internalOffset.max(slice2.internalOffset)))
        else 
          pointTo.update(node2.cells(0), getPointee(node1.cells(0)))
      pointTo.remove(node1.cells(0))
      replace(node1.cells(0), node2.cells(0), 0)
      node2.cells(0)
    else // standard merge

      // node 1 is the cell with the higher offset
      var delta = cell1.offset - cell2.offset
      var node1 = cell1.node.get
      var node2 = cell2.node.get
      if cell1.offset < cell2.offset then
        delta = cell2.offset - cell1.offset
        node1 = cell2.node.get
        node2 = cell1.node.get


      // create a seq of all cells from both nodes in order of their offsets in the resulting unified node
      val cells : Seq[(BigInt, DSC)] = (node1.cells.toSeq ++ node2.cells.foldLeft(Seq[(BigInt, DSC)]()){
        (s, tuple) =>
          val offset = tuple._1
          val cell = tuple._2
          s:+ ((offset + delta, cell)) // cells from nodes two are adjusted by the difference between cell1 and cell2 offsets
      }).sortBy(_._1)

      var lastOffset: BigInt = -1
      var lastAccess: BigInt = -1
      // create a new node to represent the unified node
      val resultNode = DSN(Some(this))
      // add nodes flags and regions to the resulting node
      resultNode.allocationRegions.addAll(node1.allocationRegions ++ node2.allocationRegions)
      resultNode.flags.join(node1.flags)
      resultNode.flags.join(node2.flags)
      if node2.flags.global then // node 2 may have been adjusted depending on cell1 and cell2 offsets
        globalMapping.foreach{ // update global mapping if node 2 was global
          case (range: AddressRange, Field(node, offset))=>
            if node.equals(node2) then
              globalMapping.update(range, Field(node, offset + delta))
        }

      // compute the cells present in the resulting unified node
      // a mapping from offsets to the set of old cells which are merged to form a cell in the new unified node
      // values in the mapping also include the largest access size so far computed for each resulting cell
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
          val outgoing: Set[Slice] = cells.foldLeft(Set[Slice]()){
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
            pointTo.update(collapsedCell, Slice(result, internal))
      }
      
      if cell1.offset >= cell2.offset then
        resultNode.getCell(cell1.offset)
      else
        resultNode.getCell(cell2.offset)



  private def isFormal(pos: CFGPosition, variable: Variable): Boolean =
    !reachingDefs(pos).contains(variable)

  // formal arguments to this function
  val formals: mutable.Map[Variable, Slice] = mutable.Map()

  //  mapping from each SSA variable (position, variable) to a slice
  val varToCell: mutable.Map[CFGPosition, mutable.Map[Variable, Slice]] = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(mutable.Map[CFGPosition, mutable.Map[Variable, Slice]]()) {
    (m, pos) =>
      pos match
        case Assign(variable, value , label) =>
          value.variables.foreach(
            v =>
              if isFormal(pos, v) then
                val node = DSN(Some(this))
                node.flags.incomplete = true
                nodes.add(node)
                formals.update(v, Slice(node.cells(0), 0))
          )
          val node = DSN(Some(this))
          m +=(pos -> mutable.Map(variable -> Slice(node.cells(0), 0)))
        case DirectCall(proc, target, label) if proc.name == "malloc" =>
          val node = DSN(Some(this))
           m += (pos -> mutable.Map(mallocRegister -> Slice(node.cells(0), 0)))
        case DirectCall(proc, target, label) if writesTo.contains(proc) =>
          val result: Map[Variable, Slice] = writesTo(proc).foldLeft(Map[Variable, Slice]()){
            (n, variable) =>
              val node = DSN(Some(this))
              n + (variable -> Slice(node.cells(0), 0))
          }
          m += (pos -> result.to(mutable.Map))
        case MemoryAssign(memory, index: Expr, expr: Expr, endian, size: Int, label) if unwrapPaddingAndSlicing(expr).isInstanceOf[Variable] =>
          val value: Variable = unwrapPaddingAndSlicing(expr).asInstanceOf[Variable]
          if isFormal(pos, value) then
            val node = DSN(Some(this))
            node.flags.incomplete = true
            nodes.add(node)
            formals.update(value, Slice(node.cells(0), 0))
          m
        case _ => m
  }


  def cloneSelf(): DSG =
    val newGraph = DSG(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params)
    assert(formals.size == newGraph.formals.size)
    val idToNode: mutable.Map[Int, DSN] = mutable.Map()
    formals.foreach{
      case (variable: Variable, slice: Slice) =>
//        assert(newGraph.formals.contains(variable))
        val node = slice.node
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.formals.update(variable, Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset))
    }

    varToCell.foreach {
      case (position: CFGPosition, values: mutable.Map[Variable, Slice]) =>
//        assert(newGraph.varToCell.contains(position))
        if !newGraph.varToCell.contains(position) then
          newGraph.varToCell.update(position, mutable.Map[Variable, Slice]())
        values.foreach{
          case (variable: Variable, slice: Slice) =>
//            assert(newGraph.varToCell(position).contains(variable))
            val node = slice.node
            if !idToNode.contains(node.id) then
              val newNode = node.cloneSelf(newGraph)
              idToNode.update(node.id, newNode)
            newGraph.varToCell(position).update(variable, Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset))
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
      case (range: AddressRange, Field(node, offset)) =>
        assert(newGraph.globalMapping.contains(range))
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.globalMapping.update(range, Field(idToNode(node.id), offset))
    }

    newGraph.pointTo.clear()
    pointTo.foreach {
      case (cell1: DSC, slice: Slice) =>
        val node1 = cell1.node.get
        val node2 = slice.node
        if !idToNode.contains(node1.id) then
          val newNode1 = node1.cloneSelf(newGraph)
          idToNode.update(node1.id, newNode1)

        if !idToNode.contains(node2.id) then
          val newNode2 = node2.cloneSelf(newGraph)
          idToNode.update(node2.id, newNode2)

        newGraph.pointTo.update(idToNode(node1.id).cells(cell1.offset), Slice(idToNode(node2.id).cells(slice.offset), slice.internalOffset))
    }

    callsites.foreach(
      callSite =>
        val cs = CallSite(callSite.call, newGraph)
        newGraph.callsites.add(cs)
        assert(cs.paramCells.keySet.equals(callSite.paramCells.keySet))
        callSite.paramCells.foreach{
          case (variable: Variable, slice: Slice) =>
            assert(cs.paramCells.contains(variable))
            val id = slice.node.id
            cs.paramCells.update(variable, Slice(idToNode(id).cells(slice.offset), slice.internalOffset))
        }

        callSite.returnCells.foreach{
          case (variable: Variable, slice: Slice) =>
            assert(cs.returnCells.contains(variable))
            val id = slice.node.id
            cs.returnCells.update(variable, Slice(idToNode(id).cells(slice.offset), slice.internalOffset))
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

/**
 * a Data structure Node
 */
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
            case (variable: Variable, slice: Slice) =>
              if slice.node.equals(this) then
                to.varToCell.update(
                  pos,
                  to.varToCell.getOrElseUpdate(pos,
                    mutable.Map[Variable, Slice]()) ++ Map(variable -> slice)
                )
          }
      )
      from.formals.foreach{
        case (variable: Variable, slice: Slice) =>
          if slice.node.equals(this) then
            to.varToCell.update(
              begin(from.proc),
              to.varToCell.getOrElseUpdate(begin(from.proc),
                mutable.Map[Variable, Slice]()) ++ Map(variable -> slice)
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
 * a slice made from a cell and an internal offset
 */
case class Slice(cell: DSC, internalOffset: BigInt) {
  def node: DSN  = cell.node.get
  def offset: BigInt = cell.offset
}

/**
 * represents a direct call in DSA
 * @param call instance of the call
 * @param graph caller's DSG
 */
class CallSite(val call: DirectCall, val graph: DSG) {
  val proc = call.target
  val paramCells: mutable.Map[Variable, Slice] = graph.params(proc).foldLeft(mutable.Map[Variable, Slice]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> Slice(node.cells(0), 0))
  }
  val returnCells: mutable.Map[Variable, Slice] = graph.writesTo(proc).foldLeft(mutable.Map[Variable, Slice]()) {
    (m, reg) =>
      val node = DSN(Some(graph))
      node.flags.incomplete = true
      m += (reg -> Slice(node.cells(0), 0))
  }
}

// global address range
case class AddressRange(start: BigInt, end: BigInt)

// a node, offset pair, difference to a cell is that it doesn't represent a DSG construct,
case class Field(node: DSN, offset: BigInt)

// unwraps internal padding and slicing and returns the expression
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

def adjust(cell: DSC, internalOffset: BigInt): DSC =
  val node = cell.node.get
  node.addCell(cell.offset+internalOffset, 0)

def adjust(slice: Slice): DSC =
  val cell = slice.cell
  val internal = slice.internalOffset
  adjust(cell, internal)




