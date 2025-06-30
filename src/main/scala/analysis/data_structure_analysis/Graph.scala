package analysis.data_structure_analysis

import analysis.solvers.DSAUnionFindSolver
import analysis.{FlatElement, evaluateExpression}
import boogie.SpecGlobal
import cfg_visualiser.*
import ir.*
import specification.{ExternalFunction, FuncEntry, SymbolTableEntry}
import util.Counter
import util.assertion.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

/** Data Structure Graph for DSA
  *
  * @param proc
  *   procedure of DSG
  * @param constProp
  * @param varToSym
  *   mapping flow-sensitive (position sensitive) mapping from registers to their set of symbolic accesses
  * @param globals
  * @param globalOffsets
  * @param externalFunctions
  * @param reachingDefs
  * @param writesTo
  * @param params
  */
class Graph(using Counter)(
  val proc: Procedure,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAddress]]],
  globals: Set[SymbolTableEntry],
  globalOffsets: Map[BigInt, BigInt],
  externalFunctions: Set[ExternalFunction],
  val reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
  val writesTo: Map[Procedure, Set[GlobalVar]],
  val params: Map[Procedure, Set[Variable]]
) {

  // DSNodes owned by this graph, only updated once analysis is done,
  val nodes: mutable.Set[Node] = mutable.Set()

  // Points-to relations in this graph, only updated once the analysis is done,
  val pointsto: mutable.Map[Cell, Slice] = mutable.Map()

  // represent callees in proc
  val callsites: mutable.Set[CallSite] = mutable.Set()

  private val mallocRegister = Register("R0", 64)

  // collect all stack access and their maximum accessed size
  // BigInt is the offset of the stack position and Int is it's size
  private val stackAccesses: Map[BigInt, Int] = {
    computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).foldLeft(Map[BigInt, Int]()) {
      (results, pos) =>
        pos match {
          case LocalAssign(_, expr, _) =>
            visitStackAccess(pos, expr, 0).foldLeft(results) { (res, access) =>
              if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                res + (access.offset -> access.size)
              else res
            }
          case MemoryStore(_, index, _, _, size, _) =>
            visitStackAccess(pos, index, size).foldLeft(results) { (res, access) =>
              if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                res + (access.offset -> access.size)
              else res
            }
          case MemoryLoad(_, _, index, _, size, _) =>
            visitStackAccess(pos, index, size).foldLeft(results) { (res, access) =>
              if !res.contains(access.offset) || (res.getOrElse(access.offset, -1) < access.size) then
                res + (access.offset -> access.size)
              else res
            }
          case _ => results
        }
    }
  }

  private case class StackAccess(offset: BigInt, size: Int)

  private def visitStackAccess(pos: CFGPosition, index: Expr, size: Int): Set[StackAccess] = {
    debugAssert(size % 8 == 0)
    val byteSize = size / 8
    index match
      case BinaryExpr(_, arg1: Variable, arg2)
          if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
            evaluateExpression(arg2, constProp(pos)).isDefined =>
        val offset = evaluateExpression(arg2, constProp(pos)).get.value
        varToSym(pos)(arg1).flatMap {
          case SymbolicAddress(_, _: StackLocation, symOffset) => // only consider stack accesses
            Some(StackAccess(offset + symOffset, byteSize))
          case _ => None
        }
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
        varToSym(pos)(arg).flatMap {
          case SymbolicAddress(_, _: StackLocation, offset) =>
            //                 createStackMapping(pos.toShortString, offset, m, byteSize)
            Some(StackAccess(offset, byteSize))
          case _ =>
            None
        }
      case _ => Set.empty
  }

  // this is the mapping from offsets/positions on the stack to their representative DS nodes
  val stackMapping: mutable.Map[BigInt, Node] = mutable.Map()
  private var lastOffset: BigInt = -1
  private var nextValidOffset: BigInt = 0
  stackAccesses.keys.toSeq.sorted.foreach { offset =>
    val byteSize = stackAccesses(offset)
    if offset >= nextValidOffset then
      val node = Node(Some(this), byteSize)
      node.allocationRegions.add(StackLocation(s"Stack_${proc.name}_$offset", proc, byteSize))
      node.flags.stack = true
      node.addCell(0, 0)
      stackMapping.update(offset, node)
      lastOffset = offset
    else
      val diff = nextValidOffset - offset
      stackMapping(lastOffset).addCell(diff, byteSize)
    nextValidOffset = offset + byteSize
  }

  /** Takes a cell and returns all corresponding stack offsets to it if any
    */
  def getStackOffsets(cell: Cell): Set[BigInt] = { // TODO replace with tracking through merges
    stackMapping.foldLeft(Set[BigInt]()) { (s, f) =>
      f match
        case (offset: BigInt, node: Node) =>
          s ++ node.cells.foldLeft(Set[BigInt]()) { (se, g) =>
            g match
              case (internal: BigInt, stackCell: Cell) =>
                if cell == find(stackCell) then se + (offset + internal)
                else se
          }
    }
  }

  def getStack(offset: BigInt, size: Int): Cell = {
    var last: BigInt = 0
    var headNodeOffset: BigInt = -1

    val head: Cell =
      if stackMapping.contains(offset) then
        headNodeOffset = offset
        stackMapping(offset).cells(0)
      else
        breakable {
          stackMapping.keys.toSeq.sorted.foreach(elementOffset =>
            if offset < elementOffset then break
            else last = elementOffset
          )
        }
        val diff = offset - last
        headNodeOffset = last
        debugAssert(stackMapping.contains(last))
        stackMapping(last).getCell(diff)

    // DSA grows cell size with size
    // selfCollapse at the end to merge all the overlapping accessed size
    // However, the above approach prevents distinct multi loads
    // find(head).growSize(size)
    val headOffset = headNodeOffset + head.offset
    stackMapping.keys.toSeq.filter(off => off > headOffset && off < headOffset + size).sorted.foreach { off =>
      val stackDiff = off - headOffset
      val updatedHead = find(head)
      val newHeadOffset = updatedHead.offset
      val headNode = updatedHead.node.get
      mergeCells(headNode.addCell(newHeadOffset + stackDiff, 0), find(stackMapping(off).cells(0)))
    }
    // selfCollapse(head.node.get)
    find(head)
  }

  private val swappedOffsets = globalOffsets.map(_.swap)

  /** Converts a global name to an Address range
    * @param name
    *   the name of the global region used as a reference
    * @param size
    *   size of the Address region to be returned if 0 the size is instead one specified by SpecGlobals
    * @param relocated
    *   value determines the level of relocation, if 0 return address range begining at start of global region name.
    *   otherwise attempt to find a global address pointing to x through "relocated" number of indirections
    * @return
    */
  def getGlobal(name: String, size: Int = 0, relocated: Int = 0): Option[AddressRange] = {
    val matchedName = globals.filter(_.name == name)
    debugAssert(matchedName.size <= 1)
    if matchedName.isEmpty then None
    else
      val global = matchedName.head
      if relocated == 0 then
        Some(AddressRange(global.address, global.address + (if size == 0 then global.size / 8 else size)))
      else
        var i = relocated
        var address = global.address
        while i > 0 do
          address = swappedOffsets.getOrElse(address, -1)
          i -= 1
        if address == -1 then None
        else Some(AddressRange(address, address + size))
  }
  // creates the globals from the symbol tables
  val globalMapping = mutable.Map[AddressRange, Field]()
  globals.foreach {
    case FuncEntry(name, size, address) =>

      val function = Node(Some(this), size / 8)
      function.cells(0).growSize(size / 8)
      function.allocationRegions.add(Function(name, address, size / 8)) // todo check that size 0 is correct
      function.flags.global = true
      function.flags.incomplete = true
      globalMapping.update(AddressRange(address, address + (size / 8)), Field(function, 0))
    case SpecGlobal(name, size, arraySize, address) =>

      val node = Node(Some(this), size / 8)
      node.allocationRegions.add(DataLocation(name, address, size / 8))
      node.flags.global = true
      node.flags.incomplete = true
      globalMapping.update(AddressRange(address, address + size / 8), Field(node, 0))
    case _ => ???
  }

  // creates a global for each relocation entry in the symbol table
  // the global corresponding to the relocated address points to the global corresponding to the original address
  globals.foreach { global =>
    var address = global.address
    breakable {
      while (swappedOffsets.contains(address)) {
        val relocatedAddress = swappedOffsets(address)
        if relocatedAddress == address then break

        var field: BigInt = 0
        val node: Node = isGlobal(relocatedAddress) match
          case Some(value) =>
            field = relocatedAddress - value.addressRange.start
            val node = value.field.node
            node.addCell(field, 8)
            node

          case None =>
            val node = Node(Some(this))
            node.allocationRegions.add(DataLocation(s"Relocated_$relocatedAddress", relocatedAddress, 8))
            node.flags.global = true
            node.flags.incomplete = true
            globalMapping.update(AddressRange(relocatedAddress, relocatedAddress + 8), Field(node, 0))
            node

        node.cells(field).pointee = Some(Slice(isGlobal(address).get.field.node.cells(0), 0))
        address = relocatedAddress
      }
    }
  }

  externalFunctions.foreach { external =>
    val relocationNode = Node(Some(this))
    relocationNode.allocationRegions.add(DataLocation(s"${external.name}_relocation", external.offset, 8))
    relocationNode.flags.global = true
    relocationNode.flags.incomplete = true
    globalMapping.update(AddressRange(external.offset, external.offset + 8), Field(relocationNode, 0))
    relocationNode.addCell(0, 8)
    val externalNode = Node(Some(this))
    externalNode.allocationRegions.add(ExternalLocation(s"${external.name}"))
    externalNode.flags.global = true
    externalNode.flags.incomplete = true
    relocationNode.cells(0).pointee = Some(Slice(externalNode.cells(0), 0))
  }

  // determine if an address is a global and return the corresponding global if it is.
  def isGlobal(address: BigInt): Option[DSAGlobal] = {
    var global: Option[DSAGlobal] = None
    breakable {
      for ((range, field) <- globalMapping) {
        if address >= range.start && (address < range.end || (range.start == range.end && range.end == address)) then
          global = Some(DSAGlobal(range, field))
          break
      }
    }
    global
  }

  // determine if an address is a global and return the corresponding global(s) if it is.
  private def getGlobals(address: BigInt, size: Int): Seq[DSAGlobal] =
    var global: Seq[DSAGlobal] = Seq.empty
    for ((range, field) <- globalMapping) {
      if (address < range.end && range.start < address + size) ||
        (address + size > range.end && address < range.end) ||
        (address >= range.start && (address < range.end || (range.start == range.end && range.end == address)))
      then global = global ++ Seq(DSAGlobal(range, field))

    }
    global.sortBy(f => f.addressRange.start)

  def getGlobal(address: BigInt, size: Int): Option[Cell] = {
    val globals = getGlobals(address, size)
    if globals.nonEmpty then
      val head = globals.head
      val DSAGlobal(range: AddressRange, Field(node, internal)) = head
      val headOffset: BigInt = if address > range.start then address - range.start + internal else internal
      val headNode = node
      val headCell: Cell = node.addCell(headOffset, 0) // DSA has the size of the added cell should as size with
      // selfCollapse at the end to merge all the overlapping accessed size
      // However, the above approach prevent distinct multi loads
      // graph.selfCollapse(headNode)
      val tail = globals.tail
      tail.foreach { g =>
        val DSAGlobal(range: AddressRange, Field(node, internal)) = g
        val offset: BigInt = if address > range.start then address - range.start + internal else internal
        node.addCell(offset, 0)
        selfCollapse(node)
        debugAssert(range.start >= address)
        mergeCells(find(headNode.addCell(range.start - address, 0)), find(node.getCell(offset)))
      }
      selfCollapse(find(headCell).node.get)
      Some(find(headCell))
    else None
  }

  def getGlobalAddresses(cell: Cell): Set[BigInt] = {
    globalMapping.foldLeft(Set[BigInt]()) { (s, g) =>
      g match
        case (range: AddressRange, field: Field) =>
          if cell == find(field.node.getCell(field.offset)) then s + range.start
          else s
    }
  }

  def getCells(pos: CFGPosition, arg: Variable): Set[Slice] = {
    if (reachingDefs(pos).contains(arg)) {
      reachingDefs(pos)(arg).map(definition => varToCell(definition)(arg))
    } else {
      Set(formals(arg))
    }
  }

  /** collects all the nodes that are currently in the DSG and updates nodes member variable
    */
  def collectNodes(): Unit = {
    nodes.clear()
    pointsto.clear()
    nodes.addAll(formals.values.map(n => find(n.cell.node.get).node))
    varToCell.values.foreach { value =>
      nodes.addAll(value.values.map(n => find(n.cell.node.get).node))
    }
    accessIndexToSlice.values.foreach { slice =>
      nodes.add(find(slice.cell.node.get).node)
    }
    nodes.addAll(stackMapping.values.map(n => find(n).node))
    nodes.addAll(globalMapping.values.map(n => find(n.node).node))

    val queue: mutable.Queue[Node] = mutable.Queue()
    queue.enqueueAll(nodes)
    while (queue.nonEmpty) {
      val cur = queue.dequeue()
      cur.cells.values.foreach { cell =>
        if (cell.pointee.isDefined) {
          val node = find(cell.getPointee.node).node
          if !nodes.contains(node) then
            nodes.add(node)
            queue.enqueue(node)
          debugAssert(!pointsto.contains(cell))
          pointsto.update(cell, find(cell.getPointee))
        }
      }
    }
  }

  def toDot: String = {
    collectNodes()
    val toRemove = Set('$', '#', '%')

    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    nodes.foreach { n =>
      structs.append(DotStruct(n.id.toString, n.toString, Some(n.cells.keys.map(o => o.toString))))
    }

    formals.keys.foreach { variable =>
      val varName = variable.name.filterNot(toRemove)
      structs.append(DotStruct(s"Formal_$varName", s"Formal_$varName", None))
    }

    pointsto.foreach { (cell, pointee) =>
      val pointerID = cell.node.get.id.toString
      val pointerOffset = cell.offset.toString
      arrows.append(
        StructArrow(
          DotStructElement(pointerID, Some(pointerOffset)),
          DotStructElement(pointee.node.id.toString, Some(pointee.cell.offset.toString)),
          pointee.internalOffset.toString
        )
      )
    }

    formals.foreach { (variable, slice) =>
      val value = find(slice)
      arrows.append(
        StructArrow(
          DotStructElement(s"Formal_${variable.name.filterNot(toRemove)}", None),
          DotStructElement(value.node.id.toString, Some(value.cell.offset.toString)),
          value.internalOffset.toString
        )
      )
    }

    varToCell.foreach { (pos, mapping) =>
      var id = (pos match {
        case p: Procedure => p.name
        case b: Block => b.label
        case c: Command => c.label.getOrElse("")
      }).filterNot(toRemove)
      mapping.foreach { (variable, slice) =>
        val varName = variable.name.filterNot(toRemove)
        structs.append(DotStruct(s"SSA_${id}_$varName", s"SSA_${pos}_$varName", None, false))
        val value = find(slice)
        arrows.append(
          StructArrow(
            DotStructElement(s"SSA_${id}_$varName", None),
            DotStructElement(value.node.id.toString, Some(value.cell.offset.toString)),
            value.internalOffset.toString
          )
        )
      }
    }

    globalMapping.foreach { (range, field) =>
      structs.append(DotStruct(s"Global_${range.start}_${range.end}", s"Global_$range", None))
      val node = find(field.node).node
      val offset = field.offset + find(field.node).offset
      val cellOffset = node.getCell(offset).offset
      val internalOffset = offset - cellOffset
      arrows.append(
        StructArrow(
          DotStructElement(s"Global_${range.start}_${range.end}", None),
          DotStructElement(node.id.toString, Some(cellOffset.toString)),
          internalOffset.toString
        )
      )
    }

    stackMapping.foreach { (offset, dsn) =>
      structs.append(DotStruct(s"Stack_$offset", s"Stack_$offset", None))
      val node = find(dsn).node
      val nodeOffset = find(dsn).offset
      val cellOffset = node.getCell(nodeOffset).offset
      val internalOffset = nodeOffset - cellOffset
      arrows.append(
        StructArrow(
          DotStructElement(s"Stack_$offset", None),
          DotStructElement(node.id.toString, Some(cellOffset.toString)),
          internalOffset.toString
        )
      )
    }

    StructDotGraph(proc.procName, structs, arrows).toDotString
  }

  /** Collapses the node causing it to lose field sensitivity
    */
  def collapseNode(n: Node): Node = {
    val (term, _) = solver.findWithOffset(n.term)
    val node: Node = term.node

    if (!(n.collapsed || find(n).node.collapsed)) {
      val collapsedNode: Node = Node(n.graph)
      val collapsedCell = Cell(Some(collapsedNode), 0)

      n.flags.collapsed = true
      collapsedNode.flags.collapsed = true

      var pointeeInternalOffset: BigInt = 0
      var pointToItself = false
      val cells = node.cells.values
      var cell = cells.tail.foldLeft(adjust(cells.head.getPointee)) { (c, cell) =>
        val pointee = cell.pointee
        if (pointee.isDefined && adjust(cell.getPointee) == cell) {
          pointToItself = true
          c
        } else if (pointee.isDefined) {
          val slice = cell.getPointee
          if (slice.internalOffset > pointeeInternalOffset) {
            pointeeInternalOffset = slice.internalOffset
          }
          mergeCells(c, adjust(slice))
        } else {
          c
        }
      }

      if (pointToItself) {
        cell = mergeCells(cell, collapsedCell)
      }

      collapsedCell.pointee = Some(Slice(collapsedCell, 0))

      debugAssert(collapsedNode.cells.size == 1)

      collapsedNode.children.addAll(node.children)
      collapsedNode.children += (node -> 0)
      collapsedNode.allocationRegions.addAll(node.allocationRegions) // add regions and flags of node 1 to node 2
      collapsedNode.flags.join(node.flags)

      solver.unify(n.term, collapsedNode.term, 0)

      collapsedNode
    } else {
      debugAssert(find(n).node.collapsed)
      find(n).node
    }
  }

  /** this function merges all the overlapping cells in the given node The node DOESN'T lose field sensitivity after
    * this
    */
  def selfCollapse(node: Node): Unit = {
    var lastOffset: BigInt = -1
    var lastAccess: BigInt = -1
    val removed = mutable.Set[BigInt]()
    val sortedOffsets = node.cells.keys.toSeq.sorted
    sortedOffsets.foreach { offset =>
      val cell = node.cells(offset)
      if (lastOffset + lastAccess > offset) {
        val result = mergeNeighbours(node.cells(lastOffset), cell)
        removed.add(offset)
        lastAccess = result.largestAccessedSize
      } else {
        lastOffset = offset
        lastAccess = cell.largestAccessedSize
      }
    }
    removed.foreach(node.cells.remove)
  }

  /** merges two neighbouring cells into one
    */
  private def mergeNeighbours(cell1: Cell, cell2: Cell): Cell = {
    require(cell1.node.equals(cell2.node) && cell1.offset < cell2.offset)
    if (cell2.pointee.isDefined) {
      if (cell1.pointee.isDefined) {
        val slice1 = cell1.getPointee
        val slice2 = cell2.getPointee
        val result = mergeCells(adjust(slice1), adjust(slice2))
        cell1.pointee = Some(Slice(result, slice2.internalOffset.max(slice1.internalOffset)))
      } else {
        cell1.pointee = cell2.pointee
      }
    }
    val internalOffsetChange = cell2.offset - cell1.offset
    cell2.node.get.cells.remove(cell2.offset)
    cell1.growSize((cell2.offset - cell1.offset).toInt + cell2.largestAccessedSize) // might cause another collapse
    cell1
  }

  //  private val parent = mutable.Map[DSC, DSC]()
  val solver: DSAUnionFindSolver = DSAUnionFindSolver()

  /** wrapper for find functionality of the union-find
    * @param node
    *   the node to perform find on
    * @return
    *   a field which is the tuple (parent node of the input node, starting offset of the input node in its parent)
    */
  def find(node: Node): Field = {
    val (n, offset) = solver.findWithOffset(node.term)
    val resultNode = n.node
    Field(resultNode, offset)
  }

  /** wrapper for find functionality of the union-find
    *
    * @param cell
    *   the cell to perform find on
    * @return
    *   the input cell's equivalent cell in the parent
    */
  def find(cell: Cell): Cell = {
    val node = cell.node.get
    val parent: Field = find(node)
    parent.node.addCell(cell.offset + parent.offset, cell.largestAccessedSize)
  }

  def find(slice: Slice): Slice = deadjust(adjust(slice))

  def get(cell: Cell): Cell = {
    val newCell = find(cell)
    selfCollapse(newCell.node.get)
    newCell.node.get.getCell(newCell.offset)
  }

  def get(slice: Slice): Cell = {
    val newCell = adjust(find(slice))
    selfCollapse(newCell.node.get)
    newCell.node.get.getCell(newCell.offset)
  }

  /** merges two cells and unifies their nodes
    * @param cell1
    * @param cell2
    * @return
    *   the resulting cell in the unified node
    */
  def mergeCells(c1: Cell, c2: Cell): Cell = {
    var cell1 = c1
    var cell2 = c2
    if c1.node.isDefined then cell1 = find(c1)

    if c2.node.isDefined then cell2 = find(c2)

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

      debugAssert(node1.collapsed || node2.collapsed)

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
        else node2.cells(0).pointee = node1.cells(0).pointee
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

      val node2CellsOffset = node2.cells.toSeq.map((offset, cell) => (offset + delta, cell))

      val cells: Seq[(BigInt, Cell)] = (node1.cells.toSeq ++ node2CellsOffset).sortBy(_(0))

      var lastOffset: BigInt = -1
      var lastAccess: Int = -1
      // create a new node to represent the unified node
      val resultNode = Node(Some(this))
      // add nodes flags and regions to the resulting node
      resultNode.allocationRegions.addAll(node1.allocationRegions ++ node2.allocationRegions)
      resultNode.flags.join(node1.flags)
      resultNode.flags.join(node2.flags)
      resultNode.children.addAll(node1.children)
      resultNode.children += (node1 -> 0)
      node2.children.keys.foreach { k =>
        resultNode.children(k) = node2.children(k) + delta
      }
      resultNode.children += (node2 -> delta)
//      if node2.flags.global then // node 2 may have been adjusted depending on cell1 and cell2 offsets
//        globalMapping.foreach { // update global mapping if node 2 was global
//          case (range: AddressRange, Field(node, offset)) =>
//            if node.equals(node2) then
//              globalMapping.update(range, Field(node, offset + delta))
//        }

      // compute the cells present in the resulting unified node
      // a mapping from offsets to the set of old cells which are merged to form a cell in the new unified node
      // values in the mapping also include the largest access size so far computed for each resulting cell
      val resultCells = mutable.Map[BigInt, mutable.Set[Cell]]()
      val resultLargestAccesses = mutable.Map[BigInt, Int]()
      cells.foreach { (offset, cell) =>
        if ((lastOffset + lastAccess > offset) || lastOffset == offset) { // includes this cell
          if ((offset - lastOffset) + cell.largestAccessedSize > lastAccess) {
            lastAccess = (offset - lastOffset).toInt + cell.largestAccessedSize
          }
          if (resultCells.contains(lastOffset)) {
            resultCells(lastOffset).addOne(cell)
          } else {
            resultCells(lastOffset) = mutable.Set(cell)
          }
          resultLargestAccesses(lastOffset) = lastAccess
        } else {
          lastOffset = offset
          lastAccess = cell.largestAccessedSize
          resultCells(lastOffset) = mutable.Set(cell)
          resultLargestAccesses(lastOffset) = lastAccess
        }
      }

      resultCells.keys.foreach { offset =>
        val collapsedCell = resultNode.addCell(offset, resultLargestAccesses(offset))
        val cells = resultCells(offset)
        val outgoing: Set[Slice] = cells.flatMap { cell =>
          if (cell.pointee.isDefined) {
            Some(cell.getPointee)
          } else {
            None
          }
        }.toSet
        // replace outgoing edges
        if (outgoing.size == 1) {
          collapsedCell.pointee = Some(outgoing.head)
        } else if (outgoing.size > 1) {
          val result = outgoing.tail.foldLeft(adjust(outgoing.head)) { (result, pointee) =>
            mergeCells(result, adjust(pointee))
          }
          collapsedCell.pointee = Some(deadjust(result))
        }
      }

      solver.unify(node1.term, resultNode.term, 0)
      solver.unify(node2.term, resultNode.term, delta)
      if cell1.offset >= cell2.offset then resultNode.getCell(cell1.offset)
      else resultNode.getCell(cell2.offset)
  }

  def adjust(cell: Cell, internalOffset: BigInt): Cell = {
    val (term, linkOffset) = solver.findWithOffset(cell.node.get.term)
    val node = term.node
    node.addCell(cell.offset + internalOffset + linkOffset, 0)
  }

  def adjust(slice: Slice, offset: BigInt = 0): Cell = {
    val cell = slice.cell
    val internal = slice.internalOffset
    adjust(cell, internal + offset)
  }

  def deadjust(cell: Cell): Slice = {
    val node = cell.node.get
    val offset = cell.offset
    selfCollapse(node)
    val newCell = node.getCell(offset)
    debugAssert(offset >= newCell.offset)
    Slice(newCell, offset - newCell.offset)
  }

  def handleOverlapping(cell: Cell): Cell = {
    val size = cell.node.get.getSize - cell.offset // if it's stack the size is rest of the node
    val result =
      if cell.node.get.flags.stack then
        getStackOffsets(cell).foldLeft(cell) { (res, offset) =>
          val stack = getStack(offset, size.toInt)
          mergeCells(res, stack)
        }
      else cell

//    size = result.largestAccessedSize
    if result.node.get.flags.global then
      getGlobalAddresses(result).foldLeft(result) { (res, offset) =>
        mergeCells(res, getGlobal(offset, size.toInt).get)
      }
    else result
  }

  private def isFormal(pos: CFGPosition, variable: Variable): Boolean = !reachingDefs(pos).contains(variable)

  // formal arguments to this function
  val formals: mutable.Map[Variable, Slice] = mutable.Map()

  // mapping from each SSA variable (position, variable) to a slice
  val varToCell: mutable.Map[CFGPosition, mutable.Map[Variable, Slice]] = varToCellInit(proc)

  private def varToCellInit(proc: Procedure): mutable.Map[CFGPosition, mutable.Map[Variable, Slice]] = {
    val varToCell = mutable.Map[CFGPosition, mutable.Map[Variable, Slice]]()
    val domain = computeDomain(IntraProcIRCursor, Set(proc))
    domain.foreach {
      case a: Assume =>
        a.body.variables.foreach { v =>
          if (isFormal(a, v)) {
            val node = Node(Some(this))
            node.flags.incomplete = true
            nodes.add(node)
            formals.update(v, Slice(node.cells(0), 0))
          }
        }
      case pos @ LocalAssign(variable, value, _) =>
        value.variables.foreach { v =>
          if (isFormal(pos, v)) {
            val node = Node(Some(this))
            node.flags.incomplete = true
            nodes.add(node)
            formals.update(v, Slice(node.cells(0), 0))
          }
        }
        val node = Node(Some(this))
        varToCell(pos) = mutable.Map(variable -> Slice(node.cells(0), 0))
      case pos @ MemoryLoad(lhs, _, index, _, _, _) =>
        index.variables.foreach { v =>
          if (isFormal(pos, v)) {
            val node = Node(Some(this))
            node.flags.incomplete = true
            nodes.add(node)
            formals.update(v, Slice(node.cells(0), 0))
          }
        }
        val node = Node(Some(this))
        varToCell(pos) = mutable.Map(lhs -> Slice(node.cells(0), 0))
      case pos @ DirectCall(target, _, _, _) if target.procName == "malloc" =>
        val node = Node(Some(this))
        varToCell(pos) = mutable.Map(mallocRegister -> Slice(node.cells(0), 0))
      case pos @ DirectCall(target, _, _, _) if writesTo.contains(target) =>
        val result = mutable.Map[Variable, Slice]()
        writesTo(target).foreach { variable =>
          val node = Node(Some(this))
          result(variable) = Slice(node.cells(0), 0)
        }
        varToCell(pos) = result
      case pos @ MemoryStore(_, _, expr, _, _, _) =>
        unwrapPaddingAndSlicing(expr) match {
          case value: Variable =>
            if (isFormal(pos, value)) {
              val node = Node(Some(this))
              node.flags.incomplete = true
              nodes.add(node)
              formals.update(value, Slice(node.cells(0), 0))
            }
          case _ =>
        }
      case _ =>
    }
    varToCell
  }

  def SSAVar(posLabel: String, varName: String): Slice = {
    debugAssert(posLabel.matches("%[0-9a-f]{8}?\\$\\d"), s"posLabel not matching BAP format '$posLabel'")

    val res = varToCell.keys.filter(pos => pos.toShortString.startsWith(posLabel))
    debugAssert(res.size == 1, s"failed to get SSAVar for '$posLabel' and '$varName'. matched label: ${res}")
    val key = res.head

    val map = varToCell(key).toMap

    val temp = map.keys.filter(variable => variable.name == varName)
    debugAssert(temp.size == 1, s"failed to get SSAVar for '$posLabel' and '$varName'. matched name: ${temp}")
    val variable = temp.head
    map(variable)
  }

  val accessIndexToSlice: mutable.Map[Statement, Slice] = accessIndexToSliceInit(proc)

  private def accessIndexToSliceInit(proc: Procedure): mutable.Map[Statement, Slice] = {
    val accessIndexToSlice = mutable.Map[Statement, Slice]()
    val domain = computeDomain(IntraProcIRCursor, Set(proc))
    domain.foreach {
      case load: MemoryLoad =>
        val node = Node(Some(this))
        accessIndexToSlice(load) = Slice(node.cells(0), 0)
      case store: MemoryStore =>
        val node = Node(Some(this))
        accessIndexToSlice(store) = Slice(node.cells(0), 0)
      case _ =>
    }
    accessIndexToSlice
  }

  def cloneSelf(): Graph = {
    val newGraph =
      Graph(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params)
    debugAssert(formals.size == newGraph.formals.size)
    val nodes = mutable.Set[Node]()
    val idToNode: mutable.Map[Int, Node] = mutable.Map()
    formals.foreach { (variable, s) =>
      //        debugAssert(newGraph.formals.contains(variable))
      val slice = find(s)
      val node = slice.node
      nodes.add(node)
      if !idToNode.contains(node.id) then
        val newNode = node.cloneSelf(newGraph)
        idToNode.update(node.id, newNode)
      newGraph.formals.update(variable, Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset))
    }

    varToCell.foreach { (position, values) =>
      //        debugAssert(newGraph.varToCell.contains(position))
      if (!newGraph.varToCell.contains(position)) {
        newGraph.varToCell.update(position, mutable.Map[Variable, Slice]())
      }
      values.foreach { (variable, s) =>
        //            debugAssert(newGraph.varToCell(position).contains(variable))
        val slice = find(s)
        val node = slice.node
        nodes.add(node)
        if (!idToNode.contains(node.id)) {
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        }
        newGraph
          .varToCell(position)
          .update(variable, Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset))
      }
    }

    accessIndexToSlice.foreach {
      case (store: MemoryStore, s: Slice) =>
        val slice = find(s)
        val node = slice.node
        nodes.add(node)
        if (!idToNode.contains(node.id)) {
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        }
        newGraph.accessIndexToSlice(store) = Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset)
      case (load: MemoryLoad, s: Slice) =>
        val slice = find(s)
        val node = slice.node
        nodes.add(node)
        if (!idToNode.contains(node.id)) {
          val newNode = node.cloneSelf(newGraph)
          idToNode.update(node.id, newNode)
        }
        newGraph.accessIndexToSlice(load) = Slice(idToNode(node.id).cells(slice.offset), slice.internalOffset)
      case _ =>
    }

    stackMapping.foreach { (offset, oldNode) =>
      val node = find(oldNode).node
      nodes.add(node)
      debugAssert(newGraph.stackMapping.contains(offset))
      if !idToNode.contains(node.id) then
        val newNode = node.cloneSelf(newGraph)
        idToNode.update(node.id, newNode)
      newGraph.stackMapping.update(offset, idToNode(node.id))
    }

    globalMapping.foreach { case (range: AddressRange, Field(node, offset)) =>
      debugAssert(newGraph.globalMapping.contains(range))
      val cell: Cell = find(node.getCell(offset))
      val finalNode: Node = cell.node.get
      nodes.add(finalNode)
      if !idToNode.contains(finalNode.id) then
        val newNode = finalNode.cloneSelf(newGraph)
        idToNode.update(finalNode.id, newNode)
      newGraph.globalMapping.update(
        range,
        Field(idToNode(finalNode.id), cell.offset + (offset - finalNode.getCell(offset).offset))
      )
    }

    val queue = mutable.Queue[Node]()
    queue.addAll(nodes)
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      node.cells.values.foreach { cell =>
        if (cell.pointee.isDefined) {
          val id = cell.node.get.id
          val pointee = find(cell.getPointee)
          val pointeeId = pointee.node.id
          if (!idToNode.contains(pointeeId)) {
            queue.enqueue(pointee.node)
            val newNode = pointee.node.cloneSelf(newGraph)
            idToNode.update(pointeeId, newNode)
          }
          idToNode(id).cells(cell.offset).pointee =
            Some(Slice(idToNode(pointeeId).cells(pointee.offset), pointee.internalOffset))
        }
      }
    }

    callsites.foreach { callSite =>
      val cs = CallSite(callSite.call, newGraph)
      newGraph.callsites.add(cs)
      debugAssert(cs.paramCells.keySet.equals(callSite.paramCells.keySet))
      callSite.paramCells.foreach { (variable, oldSlice) =>
        val slice = find(oldSlice)
        debugAssert(cs.paramCells.contains(variable))
        val id = slice.node.id
        cs.paramCells.update(variable, Slice(idToNode(id).cells(slice.offset), slice.internalOffset))
      }

      callSite.returnCells.foreach { (variable, oldSlice) =>
        val slice = find(oldSlice)
        debugAssert(cs.returnCells.contains(variable))
        val id = slice.node.id
        cs.returnCells.update(variable, Slice(idToNode(id).cells(slice.offset), slice.internalOffset))
      }
    }

    newGraph.nodes.addAll(idToNode.values)
    newGraph
  }
}
