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

class CoolGraph(val proc: Procedure, val phase: DSAPhase = Local, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])
{

  val solver: CoolDSAUnionFindSolver = CoolDSAUnionFindSolver()

  val sva = SVA(proc, constProp)
  var nodes: Set[CoolNode] = Set.empty
  var pointsTo: Set[(CoolCell, CoolCell)] = Set.empty
  var exprToCell: Set[(Expr, CoolCell)] = Set.empty
  val constraints: Set[Constraint] = computeConstraints()
  var symBases: Map[SymBase, CoolNode] = constraints.flatMap(c => List(c.arg1.SSAVar, c.arg2.SSAVar)).foldLeft(Map[SymBase, CoolNode]()) {
    (m, symValSet) =>
      var res = m
      for ((base: SymBase, valSet: Option[Set[BitVecLiteral]]) <- symValSet) {
        val node = res.getOrElse(base, CoolNode(this, mutable.Set(base)))
        valSet match
          case Some(vs) =>
            vs.foreach(f => node.addCell(f.value.toInt))
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
    structs.append(DotStruct(n.id.toString, n.toString, Some(n.cells.keys.map(o => o.toString)), true))
  }



  pointsTo.foreach { (pointer, pointee) =>
    val pointerID = pointer.node.id.toString
    val pointerOffset = pointer.offset.toString
    arrows.append(StructArrow(DotStructElement(pointerID, Some(pointerOffset)), DotStructElement(pointee.node.id.toString, Some(pointee.offset.toString))))
  }

  exprToCell.foreach { (expr, cell) =>
      structs.append(DotStruct(expr.hashCode().toString , expr.toString, None, true))
      arrows.append(StructArrow(DotStructElement(expr.hashCode().toString, None), DotStructElement(cell.node.id.toString, Some(cell.offset.toString)), ""))
  }

  StructDotGraph(proc.name, structs, arrows).toDotString
}

  private def collect(): Unit = {
    var nodes: Set[CoolNode] = Set.empty
    var pointsTo: Set[(CoolCell, CoolCell)] = Set.empty
    var exprToCell: Set[(Expr, CoolCell)] = Set.empty
    constraints.foreach {
      case DereferenceConstraint(value, index, arg1, arg2, size) =>
        val valueCells = getCells(arg1.SSAVar).map(find)
        assert(valueCells.size == 1)
        val valueCell = valueCells.head

        exprToCell += (value, valueCell)
        nodes += valueCell.node

        val indexCells = getCells(arg2.SSAVar).map(find)
        assert(indexCells.size == 1)
        val indexCell = indexCells.head

        exprToCell += (index, indexCell)

        assert(indexCell.hasPointee)
        assert(find(indexCell.getPointee) == valueCell)

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
        constraints += DereferenceConstraint(lhs, index, EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size / 8)
      case store@MemoryStore(_, index, value, _, size, _) =>
        constraints += DereferenceConstraint(value, index, EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size / 8)
      case _ =>
    }
    constraints
  }

  private def getCells(symValSet: Map[SymBase, Option[Set[BitVecLiteral]]]): Set[CoolCell] =
  {
    symValSet.foldLeft(Set[CoolCell]()) {
      (s, m) =>
        m match
          case (base: SymBase, valSet: Option[Set[BitVecLiteral]]) =>
            s ++ getCells(base, valSet)
    }
  }

  private def getCells(base: SymBase, offset: Option[Set[BitVecLiteral]]): Set[CoolCell] =
  {
    val node = symBases(base)
    offset match
      case Some(value) =>
        value.map(_.value.toInt).map(node.getCell)
      case None =>
        assert(node.isCollapsed)
        Set(node.getCell(0))
  }

  def localPhase(): CoolGraph =
    {
      constraints.toSeq.sortBy(f => f.expr1.toString).foreach(processConstraint)
      this
    }

  private def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case DereferenceConstraint(value, index, arg1: EV, arg2: EEV, size: Int) =>
        val valueCell: CoolCell = mergeCells(getCells(arg1.SSAVar))
        val pointerCells = getCells(arg2.SSAVar)
        pointerCells.foreach(_.growSize(size))
        val pointeeCell = mergePointees(pointerCells)
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
      val ne = cell1.node.collapse()
      ne.getCell(0)
    else if (cell1.node.isCollapsed || cell2.node.isCollapsed) then // a collapsed node

      var node1 = cell1.node
      var node2 = cell2.node

      node1 = node1.collapse() // collapse the other node
      node2 = node2.collapse()


      node1.getCell(0).setPointee(node2.getCell(0).getPointee)
//      if node1.getCell(0).hasPointee then
//        node2.getCell(0).setPointee(node1.getCell(0).getPointee)
//      else
//        node1.getCell(0).setPointee(node2.getCell(0).getPointee)

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

      var lastOffset: Int = -1
      var lastAccess: Int = -1
      // create a new node to represent the unified node
      val resultNode = CoolNode(this, node2.symBases.union(node1.symBases))
      // add nodes flags and regions to the resulting node

      // compute the cells present in the resulting unified node
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

      resultCells.keys.foreach { offset =>
        val collapsedCell = resultNode.addCell(offset, resultLargestAccesses(offset))
        val cells = resultCells(offset)
        val pointee = mergePointees(cells.toSet)
        collapsedCell.setPointee(pointee)
      }

      solver.unify(node1.term, resultNode.term, 0)
      solver.unify(node2.term, resultNode.term, delta)
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

class CoolNode(val graph: CoolGraph, val symBases: mutable.Set[SymBase] = mutable.Set.empty, private var _size: Int = 0, val id: Int = CoolNodeCounter.getCounter) {

  val term: DSACoolUniTerm = DSACoolUniTerm(this)
  private var collapsed = false

  override def equals(obj: Any): Boolean =
    {
      obj match
        case node: CoolNode => id == node.id
        case _ => false
    }

  override def toString: String = {
    s"Node($id, $symBases${if collapsed then ", C" else ""})"
  }

  def isCollapsed: Boolean = collapsed

  def collapse(): CoolNode =
  {

    val field = graph.find(this)
    val node: CoolNode = field.node

    if (!(node.collapsed)) {
      val collapsedNode: CoolNode = CoolNode(graph, symBases)
      val collapsedCell = collapsedNode.cells(0)

      node.collapsed = true
      collapsedNode.collapsed = true

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

      collapsedCell.setPointee(collapsedCell)
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
    var lastOffset: Int = -1
    var lastAccess: Int = -1
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
    cell1.growSize((cell2.offset - cell1.offset).toInt + cell2.largestAccessedSize) // might cause another collapse
    cell1
  }


  def getCell(offset: Int): CoolCell = {
    if collapsed then cells(0) else cells(offset)
  }

  def addCell(offset: Int, accessSize: Int = 0): CoolCell = {
    if collapsed then cells(0) else cells.getOrElseUpdate(offset, new CoolCell(this, offset, accessSize))
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
      case cell: CoolCell => node.equals(cell.node) && offset == cell.offset
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
      else if graph.find(_pointee.get) == graph.find(this) then
        _pointee = None
        _pointee = Some(graph.mergeCells(this, cell))
      else if graph.find(cell) != graph.find(_pointee.get) then
        graph.mergeCells(cell, _pointee.get)
      graph.find(_pointee.get)
    }

  def growSize(size: Int): Unit =
    {
      largestAccessedSize = math.max(largestAccessedSize, size)
    }
}

class CoolDSA(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[Map[Procedure, CoolGraph]]
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
      (m, proc) => m + (proc -> CoolGraph(proc, Local, constProp).localPhase())
    })

    writeToFile(result(Local).head._2.toDot, "cooldsa.dot")


    result(Local)
  }
}


// a node, offset pair, difference to a cell is that it doesn't represent a DSG construct,
case class CoolField(node: CoolNode, offset: Int)
