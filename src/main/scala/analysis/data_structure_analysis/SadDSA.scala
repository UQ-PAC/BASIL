package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.{BU, Local}
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{BitVecType, Expr, LocalVar, Procedure}
import util.SadDSALogger as Logger

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}

object SadNodeCounter extends Counter
class SadDSA

case class NodeTerm(v: SadNode) extends analysis.solvers.Var[NodeTerm]

class SadGraph(proc: Procedure, ph: DSAPhase,
               symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None)
  extends
    DSAGraph[OffsetUnionFindSolver[NodeTerm], SadCell, SadCell, SadCell, SadNode]
      (proc, ph, OffsetUnionFindSolver[NodeTerm](), symValues, cons)
{

  def BUPhase(locals: Map[Procedure, SadGraph]): Unit = {

    val skip = List("unicode", "so_recvln")
    phase = BU
    constraints.toSeq.sortBy(c => c.label).foreach {
      case dcc: DirectCallConstraint if locals.contains(dcc.target) && skip.forall(f => dcc.target.name.startsWith(f)) &&  !dcc.target.isExternal.getOrElse(false) =>
//        if dcc.target.name.startsWith("so_recvln")
        println(s"cloning ${dcc.target.name} into ${proc.name}")
        val oldToNew = mutable.Map[SadNode, SadNode]()
        println("doing in params")
        dcc.inParams.toSeq.sortBy(f => f._1.name).foreach {
          case (formal, actual) =>
            val formals = locals(dcc.target)
            .exprToCells(formal)
            .map(find)
            .map(
              cell =>
                val (node, offset) = findNode(cell
                  .node
                  .clone(this, true, oldToNew))
                node
                  .get(cell.interval.move(i => i + offset))
            )
            val actuals = exprToCells(actual).map(find)
            Logger.debug(s"got formal $formal")
            Logger.debug(s"computed formals $formals")
            Logger.debug(s"computed formals: pointee ${formals.filter(_.hasPointee).map(_.getPointee)}")
            Logger.debug(s"got actual $actual")
            Logger.debug(s"computed actuals $actuals")
            Logger.debug(s"computed actuals: pointee ${actuals.filter(_.hasPointee).map(_.getPointee)}")
            Logger.debug(s"merging formal $formal, with actual $actual ")
            localCorrectness()
            if (formals ++ actuals).nonEmpty then
//              val res = mergeCells(formals ++ actuals)
              (formals ++ actuals).reduce(
                (a, b) =>
                  Logger.warn(s"Got to merging ${find(a)}")
                  Logger.warn(s"with ${find(b)}")
                  val res = mergeCells(a, b)
                  assert(res.getPointee == find(a).getPointee)
                  assert(res.getPointee == find(b).getPointee)
                  Logger.debug(s"merging cells $a and $b ")
                  localCorrectness()
                  res
              )
              (formals ++ actuals).filter(_.hasPointee).foreach(
                f =>
                  assert(f.getPointee == find(f).getPointee)
              )
            formals.foreach(
              f =>
                actuals.foreach(
                  a =>
                    assert(find(f) == find(a))
                )
            )
            localCorrectness()
        }

        Logger.debug("doing out params")
        dcc.outParmas.toSeq.sortBy(f => f._1.name).foreach {
          case (out, actual) =>
            localCorrectness()
            val actuals = locals(dcc.target)
            .exprToCells(actual)
            .map(find)
            .map(
              cell =>
                val (node, offset) = findNode(cell
                  .node
                  .clone(this, true, oldToNew))
                assert(node.graph == this)
                node
                  .get(cell.interval.move(i => i + offset))
            )
            val outs = exprToCells(out).map(find)
            if (actuals ++ outs).nonEmpty then mergeCells(actuals ++ outs)
            localCorrectness()
        }
      case icc: IndirectCallConstraint =>
      case _ =>
    }
  }


  def exprToCells(expr: Expr): Set[SadCell] = {
    symValToCells(exprToSymVal(expr).removeNonAddress(i => i > 11000))
  }

  def localCorrectness(): Unit = {
    constraints.toSeq.sortBy(f => f.label).foreach {
      case constraint: MemoryAccessConstraint[_]  =>
        val valueCells = constraintArgToCells(constraint.arg2).map(find)
        assert(valueCells.size <= 1, s"value cells should be unified instead got $valueCells")
        var valueCell: Option[SadCell] = None
        if valueCells.size == 1 then
          valueCell = Some(valueCells.head)

        val indexCells = constraintArgToCells(constraint.arg1, ignoreContents = true).map(find)
        var indexCell: Option[SadCell] = None
        if indexCells.nonEmpty then
          if indexCells.nonEmpty && valueCells.nonEmpty then
            indexCells.foreach(
              indexCell =>
                if !(indexCell.hasPointee && indexCell.getPointee == valueCell.get) then
                  Logger.debug(s"index has a pointer: ${indexCell.hasPointee}")
                  Logger.debug(s"index cell: ${indexCell}")
                  if indexCell.hasPointee then
                    Logger.debug(s"index pointee: ${indexCell.getPointee}")
                    Logger.debug(s"index pointee children: ${indexCell.getPointee.node.children}")
                  Logger.debug(s"got valueCell: ${valueCell.get}")
                  Logger.debug(s"value childeren: ${valueCell.get.node.children}")
                assert(
                  indexCell.hasPointee && indexCell.getPointee == valueCell.get,
                  s"$constraint, $indexCell doesn't point to ${valueCell.get} instead ${indexCell.getPointee}"
                )
            )

      case _ =>
    }
  }

  var last: Option[(SadCell, SadCell)] = None
  var secondLast: Option[(SadCell, SadCell)] = None
  override def clone: SadGraph = {
    val oldToNew: mutable.Map[SadNode, SadNode] = mutable.Map()
    val copy = SadGraph(proc, phase, Some(sva), Some(constraints))
    val queue = mutable.Queue[SadNode]()
    this.nodes.foreach { // in addition to current nodes
      case (base, node) => // clone old nodes in base to node map to carry offset info
        val (current, offset) = this.findNode(node)
        queue.enqueue(current)
        val oldCopy = node.clone(copy, false, oldToNew)
//        assert(!oldToNew.contains(node))
//        oldToNew.update(node, oldCopy)
        val curCopy = current.clone(copy, true, oldToNew)
        queue.enqueue(current)
        copy.solver.unify(oldCopy.term, curCopy.term, offset)
    }

    copy.nodes = this.nodes.view.mapValues(oldToNew.apply).toMap
    assert(copy.nodes.keys == this.nodes.keys)
    copy.localCorrectness()
    copy
  }

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

  protected def collect(): (Set[SadNode], Set[(SadCell, SadCell)]) = {
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
  def init(symBases: mutable.Set[SymBase], size: Option[Int]): SadNode = SadNode(this, symBases, size)
  override def constraintArgToCells(constraintArg: ConstraintArg, ignoreContents: Boolean = false): Set[SadCell] = {
    val cells = symValToCells(exprToSymVal(constraintArg.value).removeNonAddress(i => i >= 11000))
    val exprCells = cells.map(find)

//    Logger.debug(s"resolving the cells for $constraintArg, $ignoreContents")

    if constraintArg.contents && !ignoreContents then
      val t = exprCells.map(_.getPointee)
//      Logger.debug(s"got $t")
      t
    else
      val t = exprCells
//      Logger.debug(s"got $t")
      t
  }

  override def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case cons: MemoryAccessConstraint[_] =>
        Logger.debug(s"Processing constraint $cons")
        val indices  = constraintArgToCells(cons.arg1, ignoreContents = true)
        val indexPointee = constraintArgToCells(cons.arg1)
        val values = constraintArgToCells(cons.arg2)
        val first = if indexPointee.nonEmpty then
          indices
            .map(findExact)
            .foreach {
              case (node, interval) => node.add(interval.growTo(cons.size))
            }
          val res  = mergeCells(indexPointee)
          val correctPointee =
          indices.map(find).foldLeft(true)((f: Boolean, pointer: SadCell) =>
              f && pointer.hasPointee && pointer.getPointee == find(res))
          assert(correctPointee)
          Some(res)
        else None
//        Logger.debug("got here")
//        Logger.debug(indices.map(find).map(_.getPointee).toString)
//        Logger.debug(indices.map(find).toString)
//        Logger.debug(first.map(find))
        val sec = if values.nonEmpty then Some(mergeCells(values)) else None
//        Logger.debug(sec)
        if first.nonEmpty && sec.nonEmpty then
          val res = mergeCells(first.get, sec.get)
//          Logger.debug(first.map(find))
//          Logger.debug(sec.map(find))
//          Logger.debug(find(res))
//          if find(res).hasPointee then
//            Logger.debug(s"found pointee ${find(res).getPointee}")
//          else
//            Logger.debug(s"No pointee")
          val correctPointee =
          indices.map(find).foldLeft(true)((f: Boolean, pointer: SadCell) =>
              f && pointer.hasPointee && pointer.getPointee == first.map(find).get)
          assert(correctPointee, "an index cell doesn't point to it's pointee")
          assert(first.map(find) == sec.map(find), "cells should be the same after unification")
        else
          Logger.warn(s"$cons had an empty argument")

        val test = 1
      case  _ =>// ignore
  }


  def mergePointees(c1: SadCell, c2: SadCell): Option[SadCell] = {
    val cell1 = find(c1)
    val cell2 = find(c2)

    (cell1.hasPointee, cell2.hasPointee) match
      case (_, true) => Some(cell1.setPointee(cell2.getPointee))
      case (true, _) => Some(cell2.setPointee(cell1.getPointee))
      case (_, _) => None // Logger.warn(s"neither $cell1, or $cell2 had a pointee")
  }

  protected def collapseAndMerge(c1: SadCell, c2:SadCell): SadCell = {

    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).foreach(
        f =>
          val cell2 = find(f)
          assert(cell2.hasPointee)
//          Logger.debug(f.getPointee)
//          Logger.debug(cell2.getPointee)
          assert(find(f.getPointee) == cell2.getPointee)
      )

    var cell1 = c1.node.collapse()
//    Logger.debug(s"Cell1 after collapse $cell1")
//    if cell1.hasPointee then Logger.debug(s"Collapsed Cell1 has pointee ${cell1.getPointee}")



    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).foreach(
        f =>
          val cell2 = find(f)
          assert(cell2.hasPointee)
//          Logger.debug(f.getPointee)
//          Logger.debug(cell2.getPointee)
          assert(find(f.getPointee) == cell2.getPointee)
      )

    var cell2 = c2.node.collapse()
    cell1 = find(cell1) // collapsing cell2 may affect cell1

//    Logger.debug(s"Cell2 after collapse $cell2")
//    Logger.debug(cell1)
//    Logger.debug(find(c1))
//    Logger.debug(find(c1) == find(cell1))
    assert(find(c1) == cell1)
    if c1.node.cells.filter(_.hasPointee).nonEmpty then
      c1.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(cell1.hasPointee)
          assert(find(f) == cell1.getPointee)
      )

    assert(find(c2) == cell2)
    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(cell2.hasPointee)
//          Logger.debug(find(f))
//          Logger.debug(cell2.getPointee)
          assert(find(f) == cell2.getPointee)
      )

    val pointee: Option[SadCell] = mergePointees(cell1, cell2)
    cell1 = find(cell1)
    cell2 = find(cell2)

    assert(find(c1) == cell1)
    if c1.node.cells.filter(_.hasPointee).nonEmpty then
      c1.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(find(f)== cell1.getPointee)
          assert(pointee.nonEmpty)
      )
    assert(find(c2) == cell2)
    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(find(f) == cell2.getPointee)
          assert(pointee.nonEmpty)
      )

    val collapsedNode = SadNode(this, cell1.node.bases ++ cell2.node.bases)
    collapsedNode.children.addAll(cell1.node.children)
    collapsedNode.children.addAll(cell2.node.children)
    val collapsedCell = collapsedNode.collapse()
    if c1.hasPointee || c2.hasPointee then assert(pointee.nonEmpty)
    if pointee.nonEmpty then collapsedCell.setPointee(pointee.get)
//    cell2.node.children.addAll(cell1.node.children)
//    cell1.node.children.addAll(cell2.node.children)
//    cell1.node.flags.join(cell2.node.flags)
//    cell2.node.flags.join(cell1.node.flags)

    solver.unify(cell1.node.term, collapsedNode.term, 0)
    solver.unify(cell2.node.term, collapsedNode.term, 0)

    cell1 = find(cell1)
    cell2 = find(cell2)

    assert(cell1 == cell2)
    assert(collapsedCell == cell1)
    assert(find(c1) == find(c2))
    assert(find(c1) == cell1)
    c1.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
      f => assert(collapsedCell.getPointee == find(f))
    )
    c2.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
      f => assert(collapsedCell.getPointee == find(f))
    )
//    if c1.hasPointee then
//      assert(collapsedCell.getPointee == c1.getPointee)
//    if c2.hasPointee then
//      assert(collapsedCell.getPointee == c2.getPointee)
    collapsedCell
  }

  protected def mergeCellsHelper(cell1: SadCell, cell2: SadCell): SadCell = {
    val (stableCell, toBeMoved) = if cell1.interval.start > cell2.interval.start then (cell1, cell2) else (cell2, cell1)
      val delta = stableCell.interval.start - toBeMoved.interval.start

      val stableNode = stableCell.node
      val nodeToBeMoved = toBeMoved.node

      val stableCells = stableNode.cells
      val movedCells = nodeToBeMoved.cells.map(_.move(i => i + delta))
      val allCells = (stableCells ++ movedCells).sorted
      val resultNode = SadNode(this, stableNode.bases.union(nodeToBeMoved.bases))
      Logger.debug(s"created resultnode with id ${resultNode.id}")
      resultNode.children.addAll(stableNode.children ++ nodeToBeMoved.children + stableNode.id + nodeToBeMoved.id)
      resultNode.flags.join(stableNode.flags)
      resultNode.flags.join(nodeToBeMoved.flags)
      val queue: mutable.Queue[SadCell] = mutable.Queue(allCells:_*)
//      val newToOlds: mutable.Map[SadCell, Set[SadCell]] = mutable.Map.empty
      allCells.foreach(
        c =>
          resultNode.add(c.interval)
      )
/*      while queue.nonEmpty do
        val cell = queue.dequeue()
        var unifiedInterval = cell.interval
        var overlapping: Set[SadCell] = Set.empty
        while (
          {
            val old = unifiedInterval
            overlapping ++= queue.toSet.filter(cell2 => unifiedInterval.isOverlapping(cell2.interval))
            queue.dequeueAll(overlapping.contains)
            unifiedInterval = if overlapping.isEmpty then cell.interval else overlapping.map(_.interval).reduce(Interval.join)
            unifiedInterval != old
          }
        ) {}

//        overlapping += (queue.toSet.filter(cell2 => cell.interval.isOverlapping(cell2.interval))
//          queue.dequeueAll(overlapping.contains)
//          unifiedInterval = if overlapping.isEmpty then cell.interval else overlapping.map(_.interval).reduce(Interval.join)


        val newCell = SadCell(resultNode, unifiedInterval)
        newToOlds.update(newCell, overlapping + cell)
        resultNode.add(newCell)*/

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
      Logger.debug(s"unified result Node ${resultNode.id} with ${stableNode.id}")
      solver.unify(stableNode.term, resultNode.term, 0)
      solver.unify(nodeToBeMoved.term, resultNode.term, delta)
      Logger.debug(s"unified result Node ${resultNode.id} with ${nodeToBeMoved.id}")

      //set pointees
      val pointeeResult = mutable.Map[SadCell, SadCell]()
      resultNode.cells.foreach(
        cell =>
          var newCell = find(cell)
          val pointees = allCells.filter(_.interval.isOverlapping(cell.interval)) //newToOlds
            .collect {case cell: SadCell if cell.hasPointee => cell.getPointee}
          if pointees.nonEmpty then
            val mergedPointees = mergeCells(pointees)
            newCell = find(newCell) // above merge may change the cell if the node points to itself
            pointeeResult.update(cell, mergedPointees)
            newCell.setPointee(mergedPointees)
      )
//
//      resultNode.cells.filter(pointeeResult.contains).foreach(
//        f =>
//          f.setPointee(pointeeResult(f))
//          find(f).setPointee(pointeeResult(f))
//      )

      find(resultNode.get(stableCell.interval))
  }

  override def mergeCells(c1: SadCell, c2: SadCell): SadCell = {
    require(c1.node.graph == c2.node.graph)
    val cell1 = find(c1)
    val cell2 = find(c2)
    Logger.debug(cell1)
    if cell1.hasPointee then Logger.debug(cell1.getPointee)

    Logger.debug(cell2)
    if cell2.hasPointee then Logger.debug(cell2.getPointee)


//    Logger.debug(s"Cell1 of Merge $cell1")
//    if cell1.hasPointee then Logger.debug(s"It had a pointee ${cell1.getPointee}")
//    Logger.debug(s"Cell2 of Merge $cell2")
//    if cell2.hasPointee then Logger.debug(s"It had a pointee ${cell2.getPointee}")
    val result = if cell1.equals(cell2) then
      Logger.debug(s"merged $cell1 with itself")
      cell1
    else if cell1.node.equals(cell2.node) then
      Logger.debug(s"merged $cell1 and $cell2")
      cell1.node.collapse()
    else if cell1.node.isCollapsed || cell2.node.isCollapsed then
      Logger.debug(s"merge and collapse $cell1 and $cell2")
      collapseAndMerge(cell1, cell2)
    else
      Logger.debug(s"merged $cell1 and $cell2")
      mergeCellsHelper(cell1, cell2)


    Logger.debug(s"Got result: $result")
    assert(result == find(cell1))
    assert(result == find(cell2))
//    Logger.debug(result)
    if cell1.node.cells.filter(_.hasPointee).nonEmpty then
      Logger.debug(result.node)
      Logger.debug(result.node.cells.map(_.interval))
      Logger.debug(cell1.interval)
      Logger.debug(cell2.interval)
      cell1.node.cells.filter(_.hasPointee).foreach(
        f =>
//          Logger.debug(f)
          assert(f.getPointee == find(f).getPointee)
          assert(find(f).node == result.node)
      )
    if cell2.node.cells.filter(_.hasPointee).nonEmpty then
      cell2.node.cells.filter(_.hasPointee).foreach(
        f =>
          assert(f.getPointee == find(f).getPointee)
          assert(find(f).node == result.node)
      )

//    if phase == BU then localCorrectness()
    result
  }

  override def mergeCells(cells: Iterable[SadCell]): SadCell = {
    require(cells.nonEmpty, "can't merge empty set of cells")
    cells.tail.foldLeft(cells.head) {
      (result, cell) =>
        mergeCells(result, cell)
    }
  }

  def findExact(cell: SadCell): (SadNode, Interval) = {
    val node = cell.node
    val (newNode, offset) = findNode(node)
    (newNode, cell.interval.move(i => i + offset))
  }

  override def find(cell: SadCell): SadCell = {
    val (newNode, newInterval) = findExact(cell)
    newNode.get(newInterval)
  }

  def findNode(node: SadNode): (SadNode, Int) = {
    val (term, offset) = solver.findWithOffset(node.term)
    (term.asInstanceOf[NodeTerm].v, offset)
  }

}

class SadNode(val graph: SadGraph, val bases: mutable.Set[SymBase]= mutable.Set.empty, size: Option[Int] = None, val id: Int = SadNodeCounter.increment()) extends DSANode[SadCell](size) {

  Logger.debug(s"created node with id $id")
  def clone(newGraph: SadGraph, recurse: Boolean = false,
            oldToNew: mutable.Map[SadNode, SadNode] = mutable.Map()): SadNode  = {
    //    val (node, _) = graph.findNode(this)
    val node = this
    val newNode =
      if !oldToNew.contains(node) then
        val v = newGraph.init(node.bases, node.size)
        node.cells.foreach(
          cell =>
            v.add(cell.interval)
        )
        if node.isCollapsed then
          v._collapsed = Some(v.get(0))
        oldToNew.update(node, v)
        v
      else
        val (newNode, offset) = newGraph.findNode(oldToNew(node))
//        node.cells.foreach(
//          cell =>
//            newNode.add(cell.interval.move(i => i + offset))
//        )
        newNode


    if recurse then
      val queue = mutable.Queue[SadNode](node)
      while queue.nonEmpty do
        val old = queue.dequeue()
        assert(oldToNew.contains(old))
        val (newNode, off) = newGraph.findNode(oldToNew(old))
//        assert(off == 0)
        old.cells.foreach {
          case cell: SadCell if cell.hasPointee =>
            val pointee = cell.getPointee
            val pointeeNode = pointee.node
            if !oldToNew.contains(pointeeNode) then queue.enqueue(pointeeNode)
            val (clonedNode, clonedOff) =
              if !oldToNew.contains(pointeeNode) then
                newGraph.findNode(pointeeNode.clone(newGraph, false, oldToNew))
//                pointeeNode.cells.map(_.interval).forall(i => v.cells.exists(c => c.interval == i))
//                oldToNew.update(pointeeNode, v)
              else newGraph.findNode(oldToNew(pointeeNode))
            newNode.get(cell.interval.move(i => i + off)).setPointee(clonedNode.get(pointee.interval.move(i => i + clonedOff)))
            assert(newNode.get(cell.interval.move(i => i + off)).getPointee == graph.find(clonedNode.get(pointee.interval.move(i => i + clonedOff))))
          case _ =>
        }
//        if old.isCollapsed then
//          newNode._collapsed = Some(newNode.get(0))

    newNode
  }

  val term: NodeTerm = NodeTerm(this)
  val children = mutable.Set[Int]()
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
    assert(nonOverlappingProperty)
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

    if (!(node.isCollapsed)) {
      val collapseNode: SadNode = SadNode(graph, bases, size)
      collapseNode.children.addAll(this.children)
      var collapsedCell: SadCell = collapseNode.add(0)
      collapseNode._collapsed = Some(collapsedCell)
      // delay unification
      // treat collapsed node completely distinct to current node
      if  node.cells.exists(_.hasPointee) then
        var pointToItself = false
        val cells = node.cells
        var pointee = cells.filter(_.hasPointee).map(_.getPointee).foldLeft(SadNode(graph).add(0)) {
          (c, current) =>
            val cell = graph.find(current)
            // since unification delayed check pointee against old version as well
            if ((cell.node == node || cell.node == this)) {
              pointToItself = true
              c
            } else {
              graph.mergeCells(c, cell)
            }
        }

        if (pointToItself) {
          assert(!collapsedCell.hasPointee)
//          println(pointee.node.id)
          val oldPointee = pointee.node
          Logger.debug(oldPointee.id)
          Logger.debug(collapsedCell.node.id)
          pointee = graph.mergeCells(pointee, collapsedCell)
          Logger.debug(pointee)
//          graph.solver.unify(collapsedCell.node.term, pointee.node.term, 0)
//          graph.solver.unify(oldPointee.term, pointee.node.term, 0)
          collapsedCell = pointee
        }

//        graph.solver.unify(node.term, collapsedCell.node.term, 0)
        if !pointToItself then assert(!collapsedCell.hasPointee)
        if pointToItself then println("pointing to itself") else println("not pointing to itself")
        collapsedCell.setPointee(pointee)
        collapsedCell = graph.find(collapsedCell)
        if pointToItself then assert(collapsedCell.getPointee == collapsedCell)
        assert(collapsedCell.hasPointee)
        assert(collapsedCell.getPointee == graph.find(pointee))
//        println(collapsedCell.node.cells)
        assert(collapsedCell.node.cells.size == 1)

      // unify collapsed node and current node
      graph.solver.unify(node.term, collapsedCell.node.term, 0)
      cells.foreach(f =>
        Logger.debug(graph.find(f))
        Logger.debug(collapsedCell)
        assert(graph.find(f) == collapsedCell)
      )
      if node.cells.exists(_.hasPointee) then assert(graph.find(collapsedCell).hasPointee)
      cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f => assert(graph.find(collapsedCell).getPointee == f)
      )
      node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f => assert(graph.find(collapsedCell).getPointee == f)
      )
      graph.find(collapsedCell)


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
      graph.find(node.collapsed.get)
    }

  }


  override def add(interval: Interval): SadCell = {
    val (newNode, offset) = graph.findNode(this)
    if this != newNode then
//      println(offset)
      newNode.add(interval.move(i => i + offset))
    else if !isCollapsed then
      val overlapping: Seq[SadCell] = cells.filter(_.interval.isOverlapping(interval))
//      _cells = cells.diff(overlapping)

      val newCell = if overlapping.isEmpty then
        init(interval)
      else
        val unifiedInterval = overlapping.map(_.interval).fold(interval)(Interval.join)
        val res = init(unifiedInterval)
        val pointees = overlapping.filter(_.hasPointee).map(_.getPointee)
        if pointees.nonEmpty then res.setPointee(graph.mergeCells(pointees))
        res

      _cells = cells.diff(overlapping).appended(newCell).sorted
      assert(nonOverlappingProperty)
      newCell
    else
      collapsed.get
  }


  def get(interval: Interval): SadCell = {
    val (newNode, offset) = graph.findNode(this)
    if newNode != this then
      newNode.get(interval.move(i => i + offset))
    else if isCollapsed then collapsed.get else
      val exactMatches = cells.filter(_.interval.contains(interval))
      if exactMatches.size != 1 then println(exactMatches.size)
      assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
      exactMatches.head
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

/*  def replacePointee(pointee: Option[SadCell]): Unit = {
    _pointee = pointee
  }*/

  def setPointee(cell: SadCell): SadCell = {
    if _pointee.isEmpty then
      _pointee = Some(graph.find(cell))
    else if graph.find(_pointee.get) == graph.find(this) then // if a cell points to itself break the link,
      _pointee = None
      val pointee = graph.mergeCells(this, cell)
      graph.find(this)._pointee = Some(pointee)
      _pointee = Some(pointee)
//      pointee.setPointee(pointee)
//      _pointee = Some(pointee)
    else if graph.find(cell) != graph.find(_pointee.get) then
      _pointee = Some(graph.mergeCells(cell, graph.find(_pointee.get)))
      graph.find(this)._pointee = _pointee

//    graph.find(this)._pointee.get
    graph.find(_pointee.get)
  }
}

object SadDSA {
  def getLocal(proc: Procedure, symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None,
              ): SadGraph = {
    val graph = SadGraph(proc, Local, symValues, cons)
    graph.localPhase()
    graph
  }
}




