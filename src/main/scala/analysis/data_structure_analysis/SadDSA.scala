package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.Procedure
import util.DSALogger

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}

object SadNodeCounter extends Counter
class SadDSA

case class NodeTerm(v: SadNode) extends analysis.solvers.Var[NodeTerm]

class SadGraph(proc: Procedure, phase: DSAPhase) extends DSAGraph[OffsetUnionFindSolver[NodeTerm], SadCell, SadCell, SadCell, SadNode](proc, phase, OffsetUnionFindSolver[NodeTerm]()){

  var last: Option[(SadCell, SadCell)] = None
  var secondLast: Option[(SadCell, SadCell)] = None
//  SadNodeCounter.reset()
  def toDot: String = {

    val (nodes, pointsTo) = collect()
    val toRemove = Set('$', '#', '%')

    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    nodes.foreach { n =>
      structs.append(DotStruct(n.id.toString, n.toString, Some(n.cells.map(o => o.interval.toString)), true))
    }

    pointsTo.foreach { (pointer, pointee) =>
      val pointerID = pointer.node.id.toString
      val pointerOffset = pointer.interval.toString
      arrows.append(StructArrow(DotStructElement(pointerID, Some(pointerOffset)), DotStructElement(pointee.node.id.toString, Some(pointee.interval.toString))))
    }
    StructDotGraph(proc.name, structs, arrows).toDotString
  }

  private def collect(): (Set[SadNode], Set[(SadCell, SadCell)]) = {
    var nodes: Set[SadNode] = Set.empty
    var pointsTo: Set[(SadCell, SadCell)] = Set.empty
    constraints.foreach {
      case constraint: BinaryConstraint =>
        val valueCells = constraintArgToCells(constraint.arg2).map(find)
//        assert(valueCells.size <= 1)
        var valueCell: Option[SadCell] = None
        if valueCells.size == 1 then
          valueCell = Some(valueCells.head)
          nodes += valueCell.get.node

        val indexCells = constraintArgToCells(constraint.arg1).map(find)
//        assert(indexCells.size <= 1)
//        var indexCell: Option[SadCell] = None
        if indexCells.nonEmpty then
          nodes = nodes.union(indexCells.map(_.node))

//        if indexCells.nonEmpty && valueCells.nonEmpty then
//          indexCells.foreach(
//            f => pointsTo += (f, valueCell.get)
//          )
      case _ =>
    }

    (nodes, pointsTo)
  }

  override def init(symBase: SymBase, size: Option[Int]): SadNode = SadNode(this, mutable.Set(symBase), size)
  override def constraintArgToCells(constraintArg: ConstraintArg): Set[SadCell] = {
    val exprCells = symValToCells(exprToSymVal(constraintArg.value).remove(NonPointer))
    if constraintArg.contents then
      exprCells.map(_.getPointee)
    else
      exprCells
  }

  override def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case cons: BinaryConstraint =>
        if cons.isInstanceOf[MemoryReadConstraint] && cons.asInstanceOf[MemoryReadConstraint].pos.label.get.startsWith("%00019517") then
          print("")
        val first = if constraintArgToCells(cons.arg1).nonEmpty then Some(mergeCells(constraintArgToCells(cons.arg1))) else None
        val sec = if constraintArgToCells(cons.arg2).nonEmpty then Some(mergeCells(constraintArgToCells(cons.arg2))) else None
        if first.nonEmpty && sec.nonEmpty then mergeCells(first.get, sec.get) else
          DSALogger.warn(s"$cons had an empty argument")
      case dcc: DirectCallConstraint =>
        (dcc.inConstraints ++ dcc.outConstraints).foreach(processConstraint)
      case idcc: IndirectCallConstraint => // ignore
  }

  protected def collapseAndMerge(c1: SadCell, c2:SadCell): SadCell = {
    val cell1 = c1.node.collapse()
    val cell2 = c2.node.collapse()

    if last.nonEmpty && secondLast.nonEmpty && Some(cell1, cell2) == last && last == secondLast then
      print("")


    last = Some(cell1, cell2)
    secondLast = last


    //      cell1.node.collapsed.get.setPointee(cell2.node.collapsed.get.getPointee)
    if cell2.node.collapsed.get.hasPointee then
      cell1.node.collapsed.get.setPointee(cell2.node.collapsed.get.getPointee)

    solver.unify(cell1.node.term, cell2.node.term, 0)
    cell1.node.bases.addAll(cell2.node.bases)
    cell2.node.bases.addAll(cell2.node.bases)

    cell1.node.collapsed.get
  }

  protected def mergeCellsHelper(cell1: SadCell, cell2: SadCell): SadCell = {
    val (stableCell, toBeMoved) = if cell1.interval.start > cell2.interval.start then (cell2, cell1) else (cell1, cell2)
      val delta = toBeMoved.interval.start - toBeMoved.interval.end

      val stableNode = stableCell.node
      val nodeToBeMoved = toBeMoved.node

      val stableCells = stableNode.cells
      val movedCells = nodeToBeMoved.cells.map(_.move(i => i + delta))
      val allCells = (stableCells ++ movedCells).sorted
      val resultNode = SadNode(this, stableNode.bases.union(nodeToBeMoved.bases))
      val queue: mutable.Queue[SadCell] = mutable.Queue(allCells:_*)
      val newToOlds: mutable.Map[SadCell, Set[SadCell]] = mutable.Map.empty
      while queue.nonEmpty do
        val cell = queue.dequeue()
        val (overlapping, rest) = queue.toSet.partition(cell2 => cell.interval.isOverlapping(cell2.interval))
        queue.dequeueAll(overlapping.contains)
        val unifiedInterval = if overlapping.isEmpty then cell.interval else overlapping.map(_.interval).reduce(Interval.join)
        val newCell = SadCell(resultNode, unifiedInterval)
        newToOlds.update(newCell, overlapping)
        resultNode.add(newCell)

      // compute and set selfMerged of the resultNode
      val stableSelfMerges = stableNode.selfMerged.map(f => f.map(g => g.interval))
      val movedSelfMerged = nodeToBeMoved.selfMerged.map(f => f.map(g => g.interval.move(i => i + delta)))
      val oldSelfMerged = stableSelfMerges ++ movedSelfMerged

      val eqClassQueue = mutable.Queue().enqueueAll(oldSelfMerged)
      var newEqIntervals: Set[Set[Interval]] = Set.empty
      while eqClassQueue.nonEmpty do
        val eqClass = eqClassQueue.dequeue()
        val intervals: Set[Interval] = eqClass.foldLeft(Set[Interval]()) {
          (s, interval) =>
            val overlapping = oldSelfMerged.filter(eq => eq.exists(i => i.isOverlapping(interval)))
            eqClassQueue.removeAll(overlapping.contains)
            s ++ overlapping.flatten
        }
        newEqIntervals += intervals

      resultNode.selfMerged = newEqIntervals.foldLeft(Set[Set[SadCell]]()) {
        case (s, intervals) =>
          s + intervals.map(resultNode.get)
      }

      // unify old and new nodes
      solver.unify(stableNode.term, resultNode.term, 0)
      solver.unify(nodeToBeMoved.term, resultNode.term, delta)

      //set pointees
      resultNode.cells.foreach(
        newCell =>
          val pointees = newToOlds
            .getOrElse(newCell, Set.empty)
            .collect {case cell: SadCell if cell.hasPointee => cell.getPointee}
          if pointees.nonEmpty then
            val mergedPointees = mergeCells(pointees)
            newCell.setPointee(mergedPointees)
      )

      resultNode.get(stableCell.interval)
  }

  override def mergeCells(c1: SadCell, c2: SadCell): SadCell = {
    val cell1 = find(c1)
    val cell2 = find(c2)

    if cell1.equals(cell2) then
      cell1
    else if cell1.node.equals(cell2.node) then
      cell1.node.collapse()
    else if cell1.node.isCollapsed || cell2.node.isCollapsed then
      collapseAndMerge(cell1, cell2)
    else
      mergeCellsHelper(cell1, cell2)

  }

  override def mergeCells(cells: Iterable[SadCell]): SadCell = {
    require(cells.nonEmpty, "can't merge empty set of cells")
    cells.tail.foldLeft(cells.head) {
      (result, cell) =>
        mergeCells(result, cell)
    }
  }


  override def find(cell: SadCell): SadCell = {
    val node = cell.node
    val (term, offset) = solver.findWithOffset(node.term)
    val newNode = term.asInstanceOf[NodeTerm].v
    newNode.get(cell.interval.move(i => i + offset))
  }

  def findNode(node: SadNode): (SadNode, Int) = {
    val (term, offset) = solver.findWithOffset(node.term)
    (term.asInstanceOf[NodeTerm].v, offset)
  }
}

class SadNode(val graph: SadGraph, val bases: mutable.Set[SymBase], size: Option[Int] = None, val id: Int = SadNodeCounter.increment()) extends DSANode[SadCell](size) {

  val term: NodeTerm = NodeTerm(this)
  override def hashCode(): Int = id
  override def toString: String = s"Node($id, $bases, ${if isCollapsed then "C" else selfMerged})"

  var selfMerged: Set[Set[SadCell]] = Set.empty
  private def getMerged(cell: SadCell): Set[SadCell] = {
    require(cells.contains(cell), "Provided cell must belong to this node")
    val equiv = selfMerged.filter(s => s.exists(i => i.interval.isOverlapping(cell.interval)))
    assert(equiv.size <= 1, "Expected one or less self merged equivalence sets")
    if equiv.size == 1 then
      equiv.head
    else Set(cell)
  }

  def merge(cell1: SadCell, cell2: SadCell): SadCell = {
    require(cells.contains(cell1) && cells.contains(cell2), "internal merging two cells from different nodes")
    val mergees1 = getMerged(cell1)
    val mergees2 = getMerged(cell2)
    val size = math.max(cell1.interval.size, cell2.interval.size)

    mergees1.union(mergees2).foreach(
      cell => add(Interval(cell.interval.start, cell.interval.start + size))
    )

    cell1.setPointee(cell2.getPointee)
    selfCollapse()
    graph.find(cell1)
  }

  def add(cell: SadCell): Unit = {
    require(cell.node == this, "added cell must have a reference to this node")
    _cells = _cells.appended(cell).sorted
  }

  private def selfCollapse(): Unit = {
    val queue: mutable.Queue[SadCell] = mutable.Queue(cells:_*)
    while queue.nonEmpty do
      val cell = queue.dequeue()  // cell will be in overlapping by default
      val (overlapping, rest) = cells.partition(cell2 => cell.interval.isOverlapping(cell2.interval))
      queue.dequeueAll(overlapping.contains)
      val unifiedInterval = overlapping.map(_.interval).reduce(Interval.join)
      val newCell = init(unifiedInterval)
      graph.mergeCells(overlapping.appended(newCell))
      _cells = rest.appended(newCell)
      val overlappingIntervals = overlapping.toSet

      selfMerged = selfMerged.map{
        case eqClass if eqClass.intersect(overlappingIntervals).nonEmpty  => eqClass -- overlappingIntervals + newCell
        case eqClass => eqClass
      }
  }

  override def init(interval: Interval): SadCell = SadCell(this, interval)

  override def equals(obj: Any): Boolean = {
    obj match
      case node: SadNode => id == node.id
      case _ => false
  }

  override def collapse(): SadCell = {
    val (node, _) = graph.findNode(this)

    if (!(this.isCollapsed || node.isCollapsed)) {
      val collapseNode: SadNode = SadNode(graph, bases, size)
      val collapsedCell: SadCell = collapseNode.add(0)
      collapseNode._collapsed = Some(collapsedCell)
      if  node.cells.exists(_.hasPointee) then
        var pointToItself = false
        val cells = node.cells
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

        assert(collapseNode.cells.size == 1)

        graph.solver.unify(node.term, collapseNode.term, 0)
      collapsedCell

/*      val pointToItself = node.cells.map(c => graph.find(c)).exists(cell => cell.hasPointee && cell.getPointee == cell)
      val pointees = node.cells.collect {
        case cell: SadCell if cell.hasPointee && cell.getPointee != cell => cell.getPointee
      }

      val pointee =
        if pointees.nonEmpty then
          Some(graph.mergeCells(pointees))
        else None
      if pointee.nonEmpty then collapsedCell.setPointee(pointee.get)
      if pointToItself then collapsedCell.setPointee(collapsedCell)


      graph.solver.unify(node.term, collapseNode.term, 0)*/

    } else {
      node.collapsed.get
    }

  }
}

case class SadCell(node: SadNode, override val interval: Interval) extends NodeCell(interval) {
  private var _pointee: Option[SadCell] = None
  private val graph: SadGraph = node.graph

  override def toString: String = s"Cell($node, $interval)"

  def move(f: Int => Int): SadCell = {
    val newCell = SadCell(node, interval.move(f))
    newCell._pointee = _pointee
    newCell
  }

  def getPointee: SadCell =
  {
    if _pointee.isEmpty then _pointee = Some(SadNode(graph, mutable.Set.empty).add(0))
    graph.find(_pointee.get)
  }

  def hasPointee: Boolean = _pointee.nonEmpty

  def setPointee(cell: SadCell): SadCell = {
    DSALogger.debug(s"merging pointees $cell && ${_pointee}")
    if _pointee.isEmpty then
      _pointee = Some(graph.find(cell))
    else if graph.find(_pointee.get) == graph.find(this) then // if a cell points to itself break the link,
      _pointee = None
      _pointee = Some(graph.mergeCells(this, cell))
    else if graph.find(cell) != graph.find(_pointee.get) then
      _pointee = Some(graph.mergeCells(cell, graph.find(_pointee.get)))

    graph.find(_pointee.get)
  }
}

object SadDSA {
  def getLocal(proc: Procedure): SadGraph = {
    val graph = SadGraph(proc, Local)
    graph.localPhase()
    graph
  }
}




