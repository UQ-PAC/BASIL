package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.{CoolDSAUnionFindSolver, DSACoolUniTerm, DSAUnionFindSolver}
import analysis.{Analysis, FlatElement}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.*
import util.writeToFile

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

enum DSAPhase {
  case Local, BU, TD
}

object CoolNodeCounter {
  var counter = 0
  def getCounter: Int = {
    counter += 1
    counter
  }

  def reset(): Unit =
    {
      counter = 0
    }
}

class CoolGraph(val proc: Procedure, val phase: DSAPhase = Local, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], inParams: Map[Procedure, Set[Variable]], outParams: Map[Procedure, Set[Register]])
{
  val solver: CoolDSAUnionFindSolver = CoolDSAUnionFindSolver()

  val sva = SVA(proc, constProp, inParams, outParams)
  sva.analyze()
  var nodes: Set[CoolNode] = Set.empty
  var pointsTo: Set[(CoolCell, CoolCell)] = Set.empty
  var exprToCell: Set[(CFGPosition, Expr, CoolCell)] = Set.empty
  val constraints: Set[Constraint] = computeConstraints()
  var symBases: Map[SymBase, CoolNode] = constraints.flatMap(c => List(c.arg1.SSAVar, c.arg2.SSAVar)).foldLeft(Map[SymBase, CoolNode]()) {
    (m, symValSet) =>
      var res = m
      for ((base: SymBase, valSet: Option[Set[Int]]) <- symValSet) {

        val node = res.getOrElse(base, CoolNode(this, mutable.Set(base)))
        valSet match
          case Some(vs) =>
            vs.foreach(f => node.addCell(f))
          case None =>
            node.collapse()
        res += (base -> node)
      }
      res
  }

  def toDot: String = {

    collect()
    val toRemove = Set('$', '#', '%')

    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    nodes.foreach { n =>
      structs.append(DotStruct(n.id.toString, n.toString, Some(n.cells.keys.toSeq.sorted.map(o => o.toString)), true))
    }

    pointsTo.foreach { (pointer, pointee) =>
      val pointerID = pointer.node.id.toString
      val pointerOffset = pointer.offset.toString
      arrows.append(StructArrow(DotStructElement(pointerID, Some(pointerOffset)), DotStructElement(pointee.node.id.toString, Some(pointee.offset.toString))))
    }

    var seen : Set[Expr] = Set.empty
    exprToCell.foreach { (pos, expr, cell) =>
      var const = false
      val id: String = expr match
        case _: Literal | BinaryExpr(_, Register("R31", 64), _) | Register("R31", 64) =>
          const = true
          s"${expr.hashCode().toString}".replace("-", ".")
        case _ => s"${pos.hashCode().toString}${expr.hashCode().toString}".replace("-", ".")
      if !const || !seen.contains(expr) then
        seen += expr
        structs.append(DotStruct(id , s"${if !const then pos.toShortString.takeWhile(_ != ':') + "_" else ""}${expr.toString}", None, true))
        arrows.append(StructArrow(DotStructElement(id, None), DotStructElement(cell.node.id.toString, Some(cell.offset.toString)), ""))
    }

    StructDotGraph(proc.name, structs, arrows).toDotString
  }

  private def collect(): Unit = {
    var nodes: Set[CoolNode] = Set.empty
    var pointsTo: Set[(CoolCell, CoolCell)] = Set.empty
    var exprToCell: Set[(CFGPosition, Expr, CoolCell)] = Set.empty
    constraints.toSeq.sortBy(f => f.id).foreach {
      case Constraint(pos, value, index, arg1, arg2, size, id) =>
        val valueCells = getCells(arg1.SSAVar).map(find)
        assert(valueCells.size == 1)
        val valueCell = valueCells.head


        exprToCell += (pos, value, valueCell)
        nodes += valueCell.node
        assert(arg1.SSAVar.keys.toSet.subsetOf(valueCell.node.symBases))

        val indexCells = getCells(arg2.SSAVar).map(find)
        assert(indexCells.size == 1)
        val indexCell = indexCells.head

        assert(arg2.SSAVar.keys.toSet.subsetOf(indexCell.node.symBases))

        exprToCell += (pos, index, indexCell)

        assert(indexCell.hasPointee)
        assert(indexCell.getPointee == valueCell)

        pointsTo += (indexCell, valueCell)
        nodes += indexCell.node
    }

    val queue: mutable.Queue[CoolNode] = mutable.Queue.empty
    queue.enqueueAll(nodes)

    while (queue.nonEmpty)
      {
        val node = queue.dequeue()

        node.cells.values.foreach(
          cell =>
            if cell.hasPointee then
              val pointee = cell.getPointee
              if !nodes.contains(pointee.node) then
                nodes += pointee.node
                queue.enqueue(pointee.node)
              pointsTo += (cell, pointee)
        )
      }

    this.nodes = nodes
    this.pointsTo = pointsTo
    this.exprToCell = exprToCell
  }


  private def computeConstraints(): Set[Constraint] =
  {
    val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet
    var constraints: Set[Constraint] = Set.empty
    domain.foreach {
      case load@MemoryLoad(lhs, _, index, _, size, _) =>
        constraints += Constraint(load, lhs, index, EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size / 8)
      case store@MemoryStore(_, index, value, _, size, _) =>
        constraints += Constraint(store, value, index, EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size / 8)
      case _ =>
    }
    constraints
  }

  private def getCells(symValSet: Map[SymBase, Option[Set[Int]]]): Set[CoolCell] =
  {
    symValSet.foldLeft(Set[CoolCell]()) {
      (s, m) =>
        m match
          case (base: SymBase, valSet: Option[Set[Int]]) =>
            s ++ getCells(base, valSet)
    }
  }

  def getCells(base: SymBase, offset: Option[Set[Int]]): Set[CoolCell] =
  {
    val node = symBases(base)
    offset match
      case Some(value) =>
        value.map(node.getCell).map(find)
      case None =>
        assert(node.isCollapsed)
        Set(find(node.getCell(0)))
  }

  def localPhase(): CoolGraph =
    {
      constraints.toSeq.sortBy(f => f.id).foreach(processConstraint) //  sorting only for debugging
      this
    }

  private def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case Constraint(pos, value, index, arg1: EV, arg2: EEV, size: Int, id: Int) =>
        val valueCell: CoolCell = mergeCells(getCells(arg1.SSAVar))
        val pointerCell = mergeCells(getCells(arg2.SSAVar))
        // pointerCells.foreach(_.growSize(size))
        val pointeeCell = pointerCell.getPointee // mergePointees(pointerCells)
        val test = mergeCells(valueCell, pointeeCell)
        test
      case _ => ???
  }

  def mergePointees(pointerCells: Set[CoolCell]): CoolCell =
    {
      val pointee = CoolNode(this, mutable.Set.empty).getCell(0)
      pointerCells.foreach(_.setPointee(pointee))
      find(pointee)
    }

  def mergeCells(cells: Set[CoolCell]): CoolCell =
  {
    require(cells.nonEmpty)
    if cells.size > 1 then
      cells.tail.foldLeft(cells.head) {
        (res, cell) =>
          mergeCells(res, cell)
      }
    else
      cells.head
  }

  def mergeCells(c1: CoolCell, c2: CoolCell): CoolCell =
  {
    var cell1 = c1
    var cell2 = c2
    cell1 = find(c1)
    cell2 = find(c2)

    if cell1.equals(cell2) then // same cell no action required
      cell1
    else if cell1.node.equals(cell2.node) then // same node different cells causes collapse
      val test = cell1.node.merge(cell1, cell2)
      test
//      val ne = cell1.node.collapse()
//      ne.getCell(0)
    else if (cell1.node.isCollapsed || cell2.node.isCollapsed) then // a collapsed node

      var node1 = cell1.node
      var node2 = cell2.node

      node1 = node1.collapse() // collapse the other node
      node2 = node2.collapse()


      node1.getCell(0).setPointee(node2.getCell(0).getPointee)

      solver.unify(node1.term, node2.term, 0)
      node2.symBases.addAll(node1.symBases)
      node1.symBases.addAll(node2.symBases)
      node2.getCell(0)
    else // standard merge

      // node 1 is the cell with the higher offset

      var delta = cell1.offset - cell2.offset
      var node1 = cell1.node
      var node2 = cell2.node
      if cell1.offset < cell2.offset then
        delta = cell2.offset - cell1.offset
        node1 = cell2.node
        node2 = cell1.node


      // create a seq of all cells from both nodes in order of their offsets in the resulting unified node

      val node2CellsOffset = node2.cells.toSeq.map((offset, cell) => (offset + delta, cell))

      val cells: Seq[(Int, CoolCell)] = (node1.cells.toSeq ++ node2CellsOffset).sortBy(_(0))

      var lastOffset: Int = Int.MinValue // allow for negative offsets
      var lastAccess: Int = 0
      // create a new node to represent the unified node
      val resultNode = CoolNode(this, node2.symBases.union(node1.symBases))
      // add nodes flags and regions to the resulting node
      // a mapping from offsets to the set of old cells which are merged to form a cell in the new unified node
      // values in the mapping also include the largest access size so far computed for each resulting cell
      val resultCells = mutable.Map[Int, mutable.Set[CoolCell]]()
      val resultLargestAccesses = mutable.Map[Int, Int]()
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

      // do this prior to merging pointees in case a cell points to the resulting node
      resultCells.keys.foreach( offset => resultNode.addCell(offset, resultLargestAccesses(offset)))

      val selfMerged = (node1.selfMerged ++ node2.selfMerged.map(f => f.map(_ + delta))).foldLeft(Set[Set[Int]]()) {
        case (s, mergedOffsets) =>
          val common = s.filter(f => f.intersect(mergedOffsets).nonEmpty)
          if common.isEmpty then s + mergedOffsets else (s -- common) + common.flatten
      }

      resultNode.selfMerged = selfMerged
      assert(resultNode.valid)

      solver.unify(node1.term, resultNode.term, 0)
      solver.unify(node2.term, resultNode.term, delta)

      resultCells.keys.foreach { off =>
        var field = find(resultNode)
        val node = field.node
        var offset = off + field.offset
        var collapsedCell = node.getCell(offset)
        val cells = resultCells(off)
        if (cells.filter(_.hasPointee).toSet.nonEmpty) {
          val pointee = mergePointees(cells.toSet)
          field = find(resultNode) // the above merge affects the result node if it is pointed to by itself
          offset = off + field.offset
          collapsedCell = field.node.getCell(offset)
          collapsedCell.setPointee(pointee)
        }
      }


      if cell1.offset >= cell2.offset then
        resultNode.getCell(cell1.offset)
      else
        resultNode.getCell(cell2.offset)
  }

  def find(node: CoolNode): CoolField = {
    val (n, offset) = solver.findWithOffset(node.term)
    val resultNode = n.node
    CoolField(resultNode, offset)
  }

  /**
   * wrapper for find functionality of the union-find
   *
   * @param cell the cell to perform find on
   * @return the input cell's equivalent cell in the parent
   */
  def find(cell: CoolCell): CoolCell = {
    val node = cell.node
    val parent: CoolField = find(node)
    parent.node.getCell(cell.offset + parent.offset)
  }
}


class CoolFlags {
  var collapsed = false
  var stack = false
  var heap = false
  var global = false
  var unknown =  false
  var incomplete = false
  var function = false
  var merged = false
  var read = false
  var modified = false
  var external = false


  def join(other: CoolFlags): Unit = {
    collapsed = collapsed || other.collapsed
    stack = other.stack || stack
    heap = other.heap || heap
    global = other.global || global
    unknown = other.unknown || unknown
    read = other.read || read
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    external = other.external && external
    merged = true
    function = function || other.function
  }
}

class CoolNode(val graph: CoolGraph, val symBases: mutable.Set[SymBase] = mutable.Set.empty, private var _size: Int = 0, val id: Int = CoolNodeCounter.getCounter) {

  val term: DSACoolUniTerm = DSACoolUniTerm(this)
  private val flags: CoolFlags = CoolFlags()
  symBases.foreach {
    case SymBase.Heap(id) =>
      flags.heap = true
    case SymBase.Unknown(id) =>
      flags.incomplete = true
      flags.unknown = true
    case SymBase.Stack(name) => flags.stack = true
    case SymBase.Par(name) => flags.incomplete = true
    case SymBase.Ret(name) => flags.incomplete = true
    case SymBase.Global => flags.global = true
  }

  override def equals(obj: Any): Boolean =
    {
      obj match
        case node: CoolNode => id == node.id
        case _ => false
    }

  override def hashCode(): Int = id

  override def toString: String = {
    s"Node($id, $symBases${if flags.collapsed then ", C" else ""})"
  }


  var selfMerged: Set[Set[Int]] = Set.empty

  // get the set of offsets merged with this cell
  private def getMerged(offset: Int): Set[Int] = {
    val equiv = selfMerged.filter(_.contains(offset))
    assert(equiv.size <= 1) // can only belong to at most one set of merged cells from this node
    if equiv.size == 1 then
      equiv.head
    else
      Set(offset)
  }

  def valid: Boolean = {
    var seen: Set[Int] = Set.empty
    selfMerged.foreach(
      s =>
        s.foreach(getCell)
        assert(s.intersect(seen).isEmpty)
        seen ++= s
    )

    true
  }

  def merge(cell1: CoolCell, cell2: CoolCell) = {
    require(cell1.node == this && cell2.node == this)

    val offset1 = cell1.offset
    val offset2 = cell2.offset

    val cell1Mergees = getMerged(offset1)
    val cell2Mergees = getMerged(offset2)

    cell1Mergees.foreach(
      f => getCell(f).growSize(cell2.largestAccessedSize)
    )

    cell2Mergees.foreach(
      f => getCell(f).growSize(cell1.largestAccessedSize)
    )

    cell1.setPointee(cell2.getPointee) // merge their pointees

    selfMerged = selfMerged.diff(Set(getMerged(offset1), getMerged(offset2))) + (getMerged(offset1) ++ getMerged(offset2))

    selfCollapse()
    cell1
  }

  def merge(offset1: Int, offset2: Int): CoolCell = {
    require(cells.contains(offset1) && cells.contains(offset2))
    val cell1 = getCell(offset1)
    val cell2 = getCell(offset2)

    merge(cell1, cell2)
  }

  def isCollapsed: Boolean = flags.collapsed

  def collapse(): CoolNode =
  {

    val field = graph.find(this)
    val node: CoolNode = field.node

    if (!(node.flags.collapsed)) {
      val collapsedNode: CoolNode = CoolNode(graph, symBases)
      val collapsedCell = collapsedNode.cells(0)

      node.flags.collapsed = true
      collapsedNode.flags.collapsed = true

      var pointToItself = false
      val cells = node.cells.values
      var cell = cells.tail.foldLeft(cells.head.getPointee) { (c, cell) =>
        if (cell.hasPointee && cell.getPointee == graph.find(cell)) {
          pointToItself = true // This is necessary to stop infinite recursion of cells pointing to themselves
          c
        } else if (cell.hasPointee) {
          val pointee  = cell.getPointee
          graph.mergeCells(c, pointee)
        } else {
          c
        }
      }

      if (pointToItself) {
        cell = graph.mergeCells(cell, collapsedCell)
      }

      collapsedCell.setPointee(cell)
      assert(collapsedNode.cells.size == 1)

      graph.solver.unify(node.term, collapsedNode.term, 0)
    }

    graph.find(node).node
  }


  /**
   * this function merges all the overlapping cells in the given node
   * The node DOESN'T lose field sensitivity after this
   */
  def selfCollapse(): Unit = {
    var lastOffset: Int = Int.MinValue
    var lastAccess: Int = 0
    val removed = mutable.Set[Int]()
    val sortedOffsets = cells.keys.toSeq.sorted
    sortedOffsets.foreach { offset =>
      if (lastOffset + lastAccess > offset) {
        val result = mergeNeighbours(lastOffset, offset)
        removed.add(offset)
        lastAccess = result.largestAccessedSize
      } else {
        lastOffset = offset
        lastAccess = cells(offset).largestAccessedSize
      }
    }

    // update self merged to remove cells that are removed
    selfMerged = selfMerged.map(_.removedAll(removed))

    // update the cells
    removed.foreach(cells.remove)
  }

  /**
   * merges two neighbouring cells into one
   */
  private def mergeNeighbours(offset1: Int, offset2: Int): CoolCell = {
    require(cells.contains(offset1) && cells.contains(offset2) && offset1 < offset2)
    val cell1 = cells(offset1)
    val cell2 = cells(offset2)
    if cell2.hasPointee then cell1.setPointee(cell2.getPointee)

    val internalOffsetChange = cell2.offset - cell1.offset
    cells.remove(cell2.offset)
    cell1.growSize((cell2.offset - cell1.offset) + cell2.largestAccessedSize) // might cause another collapse
    cell1
  }


  def getCell(offset: Int): CoolCell = {
    if flags.collapsed then cells(0) else cells(offset)
  }

  def addCell(offset: Int, accessSize: Int = 0): CoolCell = {
    if flags.collapsed then cells(0) else cells.getOrElseUpdate(offset, new CoolCell(this, offset, accessSize))
  }

  def size(): Int = {
    _size
  }

  val cells: mutable.Map[Int, CoolCell] = mutable.Map(0 -> CoolCell(this, 0))
}

class CoolCell(val node: CoolNode, val offset: Int, var largestAccessedSize: Int = 0)
{
  private val graph: CoolGraph = node.graph
  private var _pointee: Option[CoolCell] = None


  override def equals(obj: Any): Boolean =  {
    obj match
      case cell: CoolCell => node.equals(cell.node) &&
        // have the same offset or be merged together
        (offset == cell.offset || node.selfMerged.exists(s => s.contains(offset) && s.contains(cell.offset)))
      case _ => false
  }

  override def toString: String =
    {
      s"Cell($node, $offset)"
    }

  def getPointee: CoolCell =
    {
      if _pointee.isEmpty then _pointee = Some(CoolNode(graph, mutable.Set.empty).getCell(0))
      graph.find(_pointee.get)
    }

  def hasPointee: Boolean = _pointee.nonEmpty

  def setPointee(cell: CoolCell): CoolCell =
    {
      if _pointee.isEmpty then
        _pointee = Some(cell)
      else if graph.find(_pointee.get) == graph.find(this) then // if a cell points to itself break the link,
        _pointee = None
        _pointee = Some(graph.mergeCells(this, cell))
      else if graph.find(cell) != graph.find(_pointee.get) then
        _pointee = Some(graph.mergeCells(cell, graph.find(_pointee.get)))
      graph.find(_pointee.get)
    }

  def growSize(size: Int): Unit =
    {
      largestAccessedSize = math.max(largestAccessedSize, size)
    }
}

class CoolDSA(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], inParams: Map[Procedure, Set[Variable]], outParams: Map[Procedure, Set[Register]]) extends Analysis[Map[Procedure, CoolGraph]]
{

  val domain = computeDomain(program)

  private def computeDomain(program: Program): Set[Procedure] =
  {
    var domain: Set[Procedure] = Set(program.mainProcedure)
    val stack: mutable.Stack[Procedure] = mutable.Stack()
    stack.pushAll(program.mainProcedure.calls)

    // calculate the procedures used in the program
    while (stack.nonEmpty) {
      val current = stack.pop()
      domain += current
      stack.pushAll(current.calls.diff(domain))
    }

    domain
  }

  override def analyze(): Map[Procedure, CoolGraph] =
  {

    val result: mutable.Map[DSAPhase, Map[Procedure, CoolGraph]] = mutable.Map.empty
    result.update(Local, domain.foldLeft(Map[Procedure, CoolGraph]()) {
      (m, proc) => m + (proc -> CoolGraph(proc, Local, constProp, inParams, outParams).localPhase())
    })

    writeToFile(result(Local).head._2.toDot, "cooldsa.dot")
    writeToFile(toDot(program, result(Local).head._2.sva.svaMap.map(f => (f._1, f._2.toString())).toMap), "sva.dot")


    result(Local)
  }
}


// a node, offset pair, difference to a cell is that it doesn't represent a DSG construct,
case class CoolField(node: CoolNode, offset: Int)
