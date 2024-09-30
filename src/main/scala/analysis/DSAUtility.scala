package analysis

import analysis.solvers.{DSAUniTerm, DSAUnionFindSolver, UnionFindSolver, Var}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, Memory, MemoryAssign, MemoryLoad, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, begin, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal, SymbolTableEntry}
import util.Logger

import scala.util.control.Breaks.{break, breakable}
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
          varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAddress]]],
          globals: Set[SymbolTableEntry], globalOffsets: Map[BigInt, BigInt],
          externalFunctions: Set[ExternalFunction],
          val reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
          val writesTo: Map[Procedure, Set[Register]],
          val params: Map[Procedure, Set[Variable]]
         ) {

  // DSNodes owned by this graph, only updated once analysis is done,
  val nodes: mutable.Set[DSN] = mutable.Set()


  // Points-to relations in this graph, only updated once the analysis is done,
  val pointsto: mutable.Map[DSC, Slice] = mutable.Map()

  // represent callees in proc
  val callsites: mutable.Set[CallSite] = mutable.Set()

  val mallocRegister = Register("R0", 64)
  val stackPointer = Register("R31", 64)

  // collect all stack access and their maximum accessed size
  // BigInt is the offset of the stack position and Int is it's size
  val stackAccesses: Map[BigInt, Int] = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[BigInt, Int]()) {
    (results, pos) =>
      pos match
        case Assign(variable: Variable, expr: Expr, _) =>
          expr match
            case MemoryLoad(mem, index, endian, size) =>
              visitStackAccess(pos, index, size).foldLeft(results) {
                (res, access) =>
                  if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                    res + (access.offset -> access.size)
                  else
                    res
              }
            case _ =>
              visitStackAccess(pos, expr, 0).foldLeft(results) {
                (res, access) =>
                  if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                    res + (access.offset -> access.size)
                  else
                    res
              }
        case MemoryAssign(mem, index: Expr, value: Expr, endian, size: Int, label) =>
          visitStackAccess(pos, index, size).foldLeft(results) {
            (res, access) =>
              if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                res + (access.offset -> access.size)
              else
                res
          }
        case _ => results
  }

  case class StackAccess(offset: BigInt, size: Int)
  private def visitStackAccess(pos: CFGPosition, index: Expr, size: Int): Set[StackAccess] = {
    assert( size % 8 == 0)
    val byteSize = size / 8
    index match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        evaluateExpression(arg2, constProp(pos)).isDefined =>
        val offset = evaluateExpression(arg2, constProp(pos)).get.value
        varToSym(pos)(arg1).foldLeft(Set[StackAccess]()) { // go through all the symbolic accesses tied to arg1 at pos
          (m, sym) =>
            sym match
              case SymbolicAddress(accessor, StackLocation(regionIdentifier, proc, size), symOffset) => // only consider stack accesses
                m + StackAccess(offset + symOffset, byteSize)
              case _ => m
        }
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
        varToSym(pos)(arg).foldLeft(Set[StackAccess]()) {
          (m, sym) =>
            sym match
              case SymbolicAddress(accessor, StackLocation(regionIdentifier, proc, size), offset) =>
//                 createStackMapping(pos.toShortString, offset, m, byteSize)
                m + StackAccess(offset, byteSize)
              case _ => m
        }
      case _ => Set.empty
  }


  // this is the mapping from offsets/positions on the stack to their representative DS nodes
  val stackMapping: mutable.Map[BigInt, DSN] = mutable.Map()
  var lastOffset: BigInt = -1
  var nextValidOffset: BigInt = 0
  stackAccesses.keys.toSeq.sorted.foreach(
    offset =>
      val byteSize = stackAccesses(offset)
      if offset >= nextValidOffset then
        val node = DSN(Some(this), byteSize)
        node.allocationRegions.add(StackLocation(s"Stack_${proc}_${offset}", proc, byteSize))
        node.flags.stack = true
        node.addCell(0, byteSize)
        stackMapping.update(offset, node)
        lastOffset = offset
      else
        val diff = nextValidOffset - offset
        stackMapping(lastOffset).addCell(diff, byteSize)
      nextValidOffset = offset + byteSize
  )

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

          node.cells(field).pointee = Some(Slice(isGlobal(address).get._2._1.cells(0), 0))
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
  def isGlobal(address: BigInt): Option[DSAGlobal] =
    var global: Option[DSAGlobal]  = None
    breakable {
      for (elem <- globalMapping) {
        val range = elem._1
        val field = elem._2
        if address >= range.start && (address < range.end || (range.start == range.end && range.end == address)) then
          global = Some(DSAGlobal(range, field))
          break
      }
    }
    global

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
    pointsto.clear()
    nodes.addAll(formals.values.map(_._1.node.get).map(n => find(n).node))
    varToCell.values.foreach(
      value => nodes.addAll(value.values.map(_._1.node.get).map(n => find(n).node))
    )
    nodes.addAll(stackMapping.values.map(n => find(n).node))
    nodes.addAll(globalMapping.values.map(_._1).map(n => find(n).node))

    val queue: mutable.Queue[DSN] = mutable.Queue()
    queue.enqueueAll(nodes)
    while queue.nonEmpty do
      val cur = queue.dequeue()
      cur.cells.foreach {
        case (offset: BigInt, cell: DSC) if cell.pointee.isDefined =>
          val node = find(cell.getPointee.node).node
          if !nodes.contains(node) then
            nodes.add(node)
            queue.enqueue(node)
          assert(!pointsto.contains(cell))
          pointsto.update(cell, find(cell.getPointee))
        case _ =>
      }

  def toDot: String = {
    collectNodes


    var structs = nodes.foldLeft(Set[DotStruct]()) {
      (s, n) =>
        s + DotStruct(n.id.toString, n.toString, Some(n.cells.keys.map(o => o.toString)))
    }

    structs ++= formals.foldLeft(Set[DotStruct]()) {
      (s, n) =>
        val variable = n._1.name
        s + DotStruct(s"Formal_$variable", s"Formal_$variable", None)
    }

    structs ++= varToCell.foldLeft(Set[DotStruct]()) {
      (s, r) =>
        val pos = r._1
        val mapping = r._2
        s ++ mapping.foldLeft(Set[DotStruct]()) {
          (k, n) =>
            val variable = n._1.name
            k + DotStruct(s"SSA_${
              if pos.toShortString.startsWith("%") then pos.toShortString.drop(1) else pos.toShortString
            }_$variable", s"SSA_${pos}_$variable", None, false)
        }
    }


    structs ++= globalMapping.foldLeft(Set[DotStruct]()) {
      (s, n) =>
        val range = n._1
        s + DotStruct(s"Global_${range.start}_${range.end}", s"Global_$range", None)
    }

    structs ++= stackMapping.foldLeft(Set[DotStruct]()) {
      (s, n) =>
        val offset = n._1
        s + DotStruct(s"Stack_$offset", s"Stack_$offset", None)
    }

    var arrows =
      pointsto.foldLeft(Set[StructArrow]()) {
        case (s: Set[StructArrow], (cell: DSC, pointee: Slice)) =>
          val pointerID = cell.node.get.id.toString
          val pointerOffset = cell.offset.toString

          s + StructArrow(DotStructElement(pointerID, Some(pointerOffset)), DotStructElement(pointee.node.id.toString, Some(pointee.cell.offset.toString)), pointee.internalOffset.toString)
      }

    arrows ++= formals.foldLeft(Set[StructArrow]()) {
      (s, n) =>
        val variable = n._1.name
        val value = find(n._2)
        s + StructArrow(DotStructElement(s"Formal_$variable", None), DotStructElement(value.node.id.toString, Some(value.cell.offset.toString)), value.internalOffset.toString)
    }

    arrows ++= varToCell.foldLeft(Set[StructArrow]()) {
      (s, r) =>
        val pos = r._1
        val mapping = r._2
        s ++ mapping.foldLeft(Set[StructArrow]()) {
          (k, n) =>
            val variable = n._1.name
            val value = find(n._2)
            k + StructArrow(DotStructElement(s"SSA_${
              if pos.toShortString.startsWith("%") then pos.toShortString.drop(1) else pos.toShortString
            }_$variable", None), DotStructElement(value.node.id.toString, Some(value.cell.offset.toString)), value.internalOffset.toString)
        }
    }


    arrows ++= globalMapping.foldLeft(Set[StructArrow]()) {
      (s, n) =>
        val range = n._1
        val node= find(n._2.node).node
        val offset = n._2.offset + find(n._2.node).offset
        val cellOffset = node.getCell(offset).offset
        val internalOffset = offset - cellOffset
        s + StructArrow(DotStructElement(s"Global_${range.start}_${range.end}", None), DotStructElement(node.id.toString, Some(cellOffset.toString)), internalOffset.toString)
    }

    arrows ++= stackMapping.foldLeft(Set[StructArrow]()) {
      (s, n) =>
        val offset = n._1
        val node = find(n._2).node
        val nodeOffset =  find(n._2).offset
        val cellOffset = node.getCell(nodeOffset).offset
        val internalOffset = nodeOffset - cellOffset
        s + StructArrow(DotStructElement(s"Stack_$offset", None), DotStructElement(node.id.toString, Some(cellOffset.toString)), internalOffset.toString)
    }


    StructDotGraph(proc.name, structs, arrows).toDotString
  }


  /**
   * Collapses the node causing it to lose field sensitivity
   */
  def collapseNode(n: DSN): DSN =


    val (term, offset) = solver.findWithOffset(n.term)
    val node: DSN = term.node

    if  !(n.collapsed || find(n).node.collapsed) then

      val collapsedNode: DSN = DSN(n.graph)
      val collapedCell = DSC(Some(collapsedNode), 0)

      n.flags.collapsed = true
      collapsedNode.flags.collapsed = true

      var pointeeInternalOffset: BigInt = 0
      var pointToItself = false
      var cell = node.cells.tail.foldLeft(adjust(node.cells.head._2.getPointee)) {
        (c, field) =>
          val cell = field._2
          val pointee = cell.pointee
          if pointee.isDefined && adjust(cell.getPointee) == cell then
            pointToItself = true
            c
          else if pointee.isDefined then
            val slice = cell.getPointee
            if slice.internalOffset > pointeeInternalOffset then
              pointeeInternalOffset = slice.internalOffset
            mergeCells(c, adjust(slice))
          else
            c
      }

      if pointToItself then
        cell = mergeCells(cell, collapedCell)


      collapedCell.pointee = Some(Slice(collapedCell, 0))

      assert(collapsedNode.cells.size == 1)


      collapsedNode.children.addAll(node.children)
      collapsedNode.children += (node -> 0)
      collapsedNode.allocationRegions.addAll(node.allocationRegions) // add regions and flags of node 1 to node 2
      collapsedNode.flags.join(node.flags)


      solver.unify(n.term, collapsedNode.term, 0)

      collapsedNode
    else
      assert(find(n).node.collapsed)
      find(n).node

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
    if cell2.pointee.isDefined then
      if cell1.pointee.isDefined then
        val slice1 = cell1.getPointee
        val slice2 = cell2.getPointee
        val result = mergeCells(adjust(slice1), adjust(slice2))
        cell1.pointee = Some(Slice(result, slice2.internalOffset.max(slice1.internalOffset)))
      else
        cell1.pointee = cell2.pointee
    val internalOffsetChange = cell2.offset - cell1.offset
    cell2.node.get.cells.remove(cell2.offset)
    cell1.growSize((cell2.offset - cell1.offset).toInt + cell2.largestAccessedSize) // might cause another collapse
    cell1


  //  private val parent = mutable.Map[DSC, DSC]()
  val solver: DSAUnionFindSolver = DSAUnionFindSolver()

  /**
   * wrapper for find functionality of the union-find
   * @param node the node to perform find on
   * @return a field which is the tuple (parent node of the input node, starting offset of the input node in its parent)
   */
  def find(node: DSN) : Field =
    val (n, offset) = solver.findWithOffset(node.term)
    val resultNode = n.node
    Field(resultNode, offset)

  /**
   * wrapper for find functionality of the union-find
   *
   * @param cell the cell to perform find on
   * @return the input cell's equivalent cell in the parent
   */
  def find(cell: DSC) : DSC =
    val node = cell.node.get
    val offset = cell.offset
    val parent: Field = find(node)
    parent.node.addCell(cell.offset + parent.offset, cell.largestAccessedSize)

  def find(slice: Slice) : Slice =
    deadjust(adjust(slice))

  /**
   * merges two cells and unifies their nodes
   * @param cell1
   * @param cell2
   * @return the resulting cell in the unified node
   */
  def mergeCells(c1: DSC, c2: DSC): DSC =

    var cell1 = c1
    var cell2 = c2
    if c1.node.isDefined then
      cell1 = find(c1)

    if c2.node.isDefined then
      cell2 = find(c2)

    if cell1.equals(cell2) then // same cell no action required
      cell1
    else if cell1.node.isDefined && cell1.node.equals(cell2.node) then // same node different cells causes collapse
      val ne = collapseNode(cell1.node.get)
      ne.cells(0)
    else if cell1.node.isEmpty then
      ??? // not sure how to handle this yet TODO possibly take it out of the merge?
      cell2
    else if cell1.node.get.collapsed || cell2.node.get.collapsed then // a collapsed node

      var node1 = cell1.node.get
      var node2 = cell2.node.get

      assert(node1.collapsed || node2.collapsed)

      node1 = collapseNode(node1) // collapse the other node
      node2 = collapseNode(node2)
      node2.children.addAll(node1.children)
      node2.children += (node1 -> 0)
      node2.allocationRegions.addAll(node1.allocationRegions) // add regions and flags of node 1 to node 2
      node2.flags.join(node1.flags)
      if node1.cells(0).pointee.isDefined then // merge the pointees of the two collapsed (single cell) nodes
        if node2.cells(0).pointee.isDefined then
          val slice1 = node1.cells(0).getPointee
          val slice2 = node2.cells(0).getPointee
          val result = mergeCells(adjust(slice1), adjust(slice2))
          node2.cells(0).pointee = Some(Slice(result, slice1.internalOffset.max(slice2.internalOffset)))
        else 
          node2.cells(0).pointee = node1.cells(0).pointee
//      node1.cells(0).pointee = None
//      replace(node1.cells(0), node2.cells(0), 0)
      solver.unify(node1.term, node2.term, 0)
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
      var lastAccess: Int = -1
      // create a new node to represent the unified node
      val resultNode = DSN(Some(this))
      // add nodes flags and regions to the resulting node
      resultNode.allocationRegions.addAll(node1.allocationRegions ++ node2.allocationRegions)
      resultNode.flags.join(node1.flags)
      resultNode.flags.join(node2.flags)
      resultNode.children.addAll(node1.children)
      resultNode.children += (node1 -> 0)
      resultNode.children.addAll(node2.children.map(f => (f._1, f._2 + delta)))
      resultNode.children += (node2 -> delta)
      if node2.flags.global then // node 2 may have been adjusted depending on cell1 and cell2 offsets
        globalMapping.foreach{ // update global mapping if node 2 was global
          case (range: AddressRange, Field(node, offset))=>
            if node.equals(node2) then
              globalMapping.update(range, Field(node, offset + delta))
        }

      // compute the cells present in the resulting unified node
      // a mapping from offsets to the set of old cells which are merged to form a cell in the new unified node
      // values in the mapping also include the largest access size so far computed for each resulting cell
      val resultCells: mutable.Map[BigInt, (Set[DSC], Int)] = mutable.Map()
      cells.foreach {
        case (offset: BigInt, cell: DSC) =>
          if (lastOffset + lastAccess > offset) || lastOffset == offset then // includes this cell
            if (offset - lastOffset) + cell.largestAccessedSize > lastAccess then
              lastAccess = (offset - lastOffset).toInt + cell.largestAccessedSize
            resultCells.update(lastOffset, (resultCells(lastOffset)._1 + cell, lastAccess))
          else
            lastOffset = offset
            lastAccess = cell.largestAccessedSize
            resultCells.update(lastOffset, (Set(cell), lastAccess))
      }

      resultCells.foreach {
        case (offset: BigInt, (cells: Set[DSC], largestAccess: Int)) =>
          val collapsedCell = resultNode.addCell(offset, largestAccess)
          val outgoing: Set[Slice] = cells.foldLeft(Set[Slice]()){
            (set, cell) =>

              // collect outgoing edges
              if cell.pointee.isDefined then
                val pointee = cell.getPointee
                set + pointee
              else
                set
          }
          // replace outgoing edges
          if outgoing.size == 1 then
            collapsedCell.pointee = Some(outgoing.head)
          else if outgoing.size > 1 then
            val result = outgoing.tail.foldLeft(adjust(outgoing.head)){
              (result, pointee) =>
                mergeCells(result, adjust(pointee))
            }
            collapsedCell.pointee = Some(deadjust(result))
      }

      solver.unify(node1.term, resultNode.term, 0)
      solver.unify(node2.term, resultNode.term, delta)
      if cell1.offset >= cell2.offset then
        resultNode.getCell(cell1.offset)
      else
        resultNode.getCell(cell2.offset)


  def adjust(cell: DSC, internalOffset: BigInt): DSC =
    val link = solver.findWithOffset(cell.node.get.term)
    val node = link._1.node
    val linkOffset = link._2
    node.addCell(cell.offset + internalOffset + linkOffset, 0)

  def adjust(slice: Slice, offset: BigInt = 0): DSC =
    val cell = slice.cell
    val internal = slice.internalOffset
    adjust(cell, internal + offset)

  def deadjust(cell: DSC) : Slice =
    val node = cell.node.get
    val offset = cell.offset
    selfCollapse(node)
    val newCell = node.getCell(offset)
    assert(offset >= newCell.offset)
    Slice(newCell, offset - newCell.offset)


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
    val nodes = mutable.Set[DSN]()
    val idToNode: mutable.Map[Int, DSN] = mutable.Map()
    formals.foreach{
      case (variable: Variable, slice: Slice) =>
//        assert(newGraph.formals.contains(variable))
        val node = find(slice).node
        nodes.add(node)
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
          case (variable: Variable, s: Slice) =>
//            assert(newGraph.varToCell(position).contains(variable))
            val slice = find(s)
            val node = slice.node
            nodes.add(node)
            if !idToNode.contains(node.id) then
              val newNode = node.cloneSelf(newGraph)
              idToNode.update(node.id, newNode)
            newGraph.varToCell(position).update(variable, Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset))
        }
    }

    stackMapping.foreach{
      case (offset, oldNode) =>
        val node = find(oldNode).node
        nodes.add(node)
        assert(newGraph.stackMapping.contains(offset))
        if !idToNode.contains(node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        newGraph.stackMapping.update(offset, idToNode(node.id))
    }

    globalMapping.foreach {
      case (range: AddressRange, Field(node, offset)) =>
        assert(newGraph.globalMapping.contains(range))
        val field = find(node)
        nodes.add(field.node)
        if !idToNode.contains(field.node.id) then
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(field.node.id, newNode)
        newGraph.globalMapping.update(range, Field(idToNode(field.node.id), field.offset + offset))
    }

    val queue: mutable.Queue[DSN] = mutable.Queue()
    queue.addAll(nodes)
    while queue.nonEmpty do

      val node = queue.dequeue()
      node.cells.foreach {
        case (offset: BigInt, cell: DSC) if cell.pointee.isDefined =>
          val id = cell.node.get.id
          val pointee = find(cell.getPointee)
          val pointeeId = pointee.node.id
          if !idToNode.contains(pointeeId) then
            queue.enqueue(pointee.node)
            val newNode = pointee.node.cloneSelf(newGraph)
            idToNode.update(pointeeId, newNode)
          idToNode(id).cells(cell.offset).pointee = Some(Slice(idToNode(pointeeId).cells(pointee.offset), pointee.internalOffset))


        case _ =>
      }

    callsites.foreach(
      callSite =>
        val cs = CallSite(callSite.call, newGraph)
        newGraph.callsites.add(cs)
        assert(cs.paramCells.keySet.equals(callSite.paramCells.keySet))
        callSite.paramCells.foreach{
          case (variable: Variable, oldSlice : Slice) =>
            val slice = find(oldSlice)
            assert(cs.paramCells.contains(variable))
            val id = slice.node.id
            cs.paramCells.update(variable, Slice(idToNode(id).cells(slice.offset), slice.internalOffset))
        }

        callSite.returnCells.foreach{
          case (variable: Variable, oldSlice: Slice) =>
            val slice = find(oldSlice)
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

  val term = DSAUniTerm(this)
  val children : mutable.Map[DSN, BigInt] = mutable.Map()
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
          if start <= offset && offset < (start + cell.largestAccessedSize) then
            result = Some(cell)
      }
      result match
        case Some(value) => value
        case None => ???
//          Logger.warn(s"$this didn't have a cell at offset: $offset. An empty cell was added in")
//          addCell(offset, 0)
    else
      cells(offset)
      
      
  def addCell(offset: BigInt, size: Int) : DSC =
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
              if from.find(slice).node.equals(this) then
                to.varToCell.update(
                  pos,

                  to.varToCell.getOrElseUpdate(pos,
                    mutable.Map[Variable, Slice]()) ++ Map(variable -> from.find(slice))
                )
          }
      )
      from.formals.foreach{
        case (variable: Variable, slice: Slice) =>
          if from.find(slice).node.equals(this) then
            to.varToCell.update(
              begin(from.proc),
              to.varToCell.getOrElseUpdate(begin(from.proc),
                mutable.Map[Variable, Slice]()) ++ Map(variable -> from.find(slice))
            )
      }

      cells.foreach {
        case (offset: BigInt, cell: DSC) =>
        if cell.pointee.isDefined then
          val pointee = cell.getPointee
          pointee.node.cloneNode(from, to)
//          to.pointTo.update(cell, pointee) TODO check this is not necessary
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
class DSC(val node: Option[DSN], val offset: BigInt)
{
  var largestAccessedSize: Int = 0

  // the cell's pointee
  var pointee : Option[Slice] = None

  // returns the cell's pointee if it has one.
  // if not it will create a placeholder, set it as the pointee of this cell and return it
  def getPointee : Slice =
    if pointee.isEmpty then
      val node = DSN(Some(this.node.get.graph.get))
      pointee = Some(Slice(node.cells(0), 0))
    else

      val graph = pointee.get.node.graph.get
      val resolvedPointee = graph.find(graph.adjust(pointee.get))

      pointee = Some(graph.deadjust(resolvedPointee))
    pointee.get

  def growSize(size: Int): Boolean =
    if size > largestAccessedSize then
      largestAccessedSize = size
      true
    else false


  override def equals(obj: Any): Boolean =
    obj match
      case cell:DSC => this.node.equals(cell.node) && this.offset.equals(cell.offset)
      case _ => false


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



case class DSAGlobal(addressRange: AddressRange, field: Field) {
  def start: BigInt = addressRange.start
  def end: BigInt = addressRange.end
  def node: DSN = field.node
  def offset: BigInt = field.offset
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







