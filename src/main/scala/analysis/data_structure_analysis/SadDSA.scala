package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.{BU, Local, TD}
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{BitVecType, Expr, LocalVar, Procedure}
import util.{DSALogger, SadDSALogger as Logger}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}

object SadNodeCounter extends Counter
class SadDSA

case class NodeTerm(v: SadNode) extends analysis.solvers.Var[NodeTerm]


/** 
 * Data Structure Graph 
 */
class SadGraph(proc: Procedure, ph: DSAPhase,
               symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None)
  extends
    DSAGraph[OffsetUnionFindSolver[NodeTerm], SadCell, SadCell, SadCell, SadNode]
      (proc, ph, OffsetUnionFindSolver[NodeTerm](), symValues, cons)
{
  
  // Procceses all non call constraints
   override def localPhase(): Unit = {
     var processed = Set[Constraint]()
     constraints.toSeq.sortBy(f => f.label).foreach(
      c =>
        localCorrectness(processed)
        processed += c
        processConstraint(c)
//        SadDSALogger.warn(c)
        localCorrectness(processed)
     )
  }

  // returns the cells corresponding to the 
  def symValToCells(symVal: SymValueSet): Set[SadCell] = {
    val pairs = symVal.state
    pairs.foldLeft(Set[SadCell]()) {
      case (results, (base: SymBase, offsets: SymOffsets)) =>
        val (node, adjustment) = findNode(nodes(base))
        if offsets.isTop then
          results + node.collapse()
        else
          results ++ offsets.getOffsets.map(i => i + adjustment).map(node.add)
    }
  }
  
  // clone and unify formals and actuals from this graph to it's callees
  def TDPhase(bus: Map[Procedure, SadGraph]): Unit = {
    phase = TD
    val skip = List()
    constraints.toSeq.sortBy(c => c.label).foreach {
      case dcc: DirectCallConstraint if bus.contains(dcc.target) && skip.forall(f => dcc.target.name.startsWith(f)) && !dcc.target.isExternal.getOrElse(false) =>
        val oldToNew = mutable.Map[SadNode, SadNode]()
        val callee = bus(dcc.call.target)
        DSALogger.warn(s"cloning ${this.proc.name} into ${callee.proc.name}")
        dcc.inParams.foreach {
          case (formal, actual) =>

            DSALogger.warn(s"cloning $actual into $formal")
            val formals = callee.exprToCells(formal).map(find)
            val actuals = exprToCells(actual).map(find).map (
              cell =>
                val (node, offset) =
                  callee.findNode(
                    cell.node.clone(callee, true, oldToNew)
                  )
                assert(node.graph == callee)

                node.get(cell.interval.move(i => i + offset))
            )
            localCorrectness()
            if (actuals ++ formals).nonEmpty then callee.mergeCells(actuals ++ formals)
            localCorrectness()
        }
        

        dcc.outParmas.foreach {
          case (out, actual) =>
            DSALogger.warn(s"cloning $actual into $out")
            val actuals = callee.exprToCells(out).map(find)
            val outs: Set[SadCell] = exprToCells(actual).map(find).map(
              cell =>
                val (node, offset) =
                  callee.findNode(
                    cell.node.clone(callee, true, oldToNew)
                  )
                node.get(cell.interval.move(i => i + offset))
            )
            localCorrectness()
            if (outs ++ actuals).nonEmpty then callee.mergeCells(outs ++ actuals)
            localCorrectness()
        }


      case _ =>
    }

  }
  
  
  
  // clone and unify proc parameters from the callees into this graph
  def BUPhase(locals: Map[Procedure, SadGraph]): Unit = {

    val skip = List() // List("so_recvln") // List("unicode", "so_recvln")
    phase = BU
    constraints.toSeq.sortBy(c => c.label).foreach {
      case dcc: DirectCallConstraint if locals.contains(dcc.target) && skip.forall(f => dcc.target.name.startsWith(f)) &&  !dcc.target.isExternal.getOrElse(false) =>
        val oldToNew = mutable.Map[SadNode, SadNode]()
        dcc.inParams/*.filterNot(f => f._1.name.startsWith("R31"))*/.toSeq.sortBy(f => f._1.name).foreach {
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
              val res = mergeCells(formals ++ actuals)
//              (formals ++ actuals).reduce(
//                (a, b) =>
//                  Logger.debug(s"Got to merging ${get(a)}")
//                  Logger.debug(s"with ${get(b)}")
//                  val res = mergeCells(a, b)
//                  assert(res.getPointee.equiv(get(a).getPointee))
//                  assert(res.getPointee.equiv(get(b).getPointee))
////                  Logger.debug(s"merging cells $a and $b ")
//                  localCorrectness()
//                  res
//              )
//              (formals ++ actuals).filter(_.hasPointee).foreach(
//                f =>
//                  assert(f.getPointee.equiv(get(f).getPointee))
//              )
//            formals.foreach(
//              f =>
//                actuals.foreach(
//                  a =>
//                    assert(get(f) == get(a))
//                )
//            )
            localCorrectness()
        }

        Logger.debug("doing out params")
        dcc.outParmas/*.filterNot(f => f._1.name.startsWith("R31"))*/.toSeq.sortBy(f => f._1.name).foreach {
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


  // find the corresponding cells for a expr from this graph's procedure
  def exprToCells(expr: Expr): Set[SadCell] = {
    symValToCells(exprToSymVal(expr).removeNonAddress(i => i > 11000))
  }

  def localCorrectness(constraints: Set[Constraint] = this.constraints): Unit = {
    constraints.toSeq.sortBy(f => f.label).foreach {
      case constraint: MemoryAccessConstraint[_]  =>
        val valueCells = constraintArgToCells(constraint.arg2).map(get)
        assert(valueCells.size <= 1, s"value cells should be unified instead got $valueCells")
        var valueCell: Option[SadCell] = None
        if valueCells.size == 1 then
          valueCell = Some(valueCells.head)

        val indexCells = constraintArgToCells(constraint.arg1, ignoreContents = true).map(get)
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
                assert(indexCell.node.isUptoDate)
                assert(indexCell.getPointee.node.isUptoDate)
                assert(valueCell.get.node.isUptoDate)

                assert(
                  indexCell.hasPointee && indexCell.getPointee.equiv(valueCell.get),
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
        copy.unify(oldCopy, curCopy, offset)
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
        val valueCells = constraintArgToCells(constraint.arg2).map(get)
//        assert(valueCells.size <= 1)
        var valueCell: Option[SadCell] = None
        if valueCells.size == 1 then
          valueCell = Some(valueCells.head)
          nodes += valueCell.get.node

        val indexCells = constraintArgToCells(constraint.arg1).map(get)
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


    if constraintArg.contents && !ignoreContents then
      val t = exprCells.map(_.getPointee)
      t
    else
      val t = exprCells
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
          indices.map(get).foldLeft(true)((f: Boolean, pointer: SadCell) =>
              f && pointer.hasPointee && pointer.getPointee.equiv(res))
          assert(correctPointee)
          Some(res)
        else None
        val sec = if values.nonEmpty then Some(mergeCells(values)) else None
        if first.nonEmpty && sec.nonEmpty then
          val res = mergeCells(first.get, sec.get)
          assert(constraintArgToCells(cons.arg1).map(get) == constraintArgToCells(cons.arg2).map(get))
          val correctPointee =
          indices.map(get).foldLeft(true)((f: Boolean, pointer: SadCell) =>
              f && pointer.hasPointee && pointer.getPointee.equiv(first.map(get).get))
          assert(correctPointee, "an index cell doesn't point to it's pointee")
          assert(first.map(get) == sec.map(get), "cells should be the same after unification")
        else
          Logger.warn(s"$cons had an empty argument")

        val test = 1
      case  _ =>// ignore
  }


  def mergePointees(c1: SadCell, c2: SadCell): Option[SadCell] = {
    var cell1 = get(c1)
    var cell2 = get(c2)
    if cell1 != cell2 then
      (cell1.hasPointee, cell2.hasPointee) match
        case (true, true) =>
          val pointee1 = cell1.removePointee.get
          val pointee2 = cell2.removePointee.get
          assert(!(cell1.getPointee == cell2 && cell2.getPointee == cell1))
          var resPointee = mergeCells(pointee1, pointee2)
          cell1 = get(cell1)
          resPointee = cell1.setPointee(resPointee)
          cell2 = get(cell2)
          val res = Some(cell2.setPointee(resPointee))
//          assert(cell1.getPointee == res)
//          assert(cell2.getPointee == res)
          res
        case (_, true) => Some(cell1.setPointee(cell2.getPointee))
        case (true, _) => Some(cell2.setPointee(cell1.getPointee))
        case (_, _) => None // Logger.warn(s"neither $cell1, or $cell2 had a pointee")
    else
      Some(cell1.getPointee)

  }

  protected def collapseAndMerge(c1: SadCell, c2:SadCell): SadCell = {
    assert(c1.node.isUptoDate)
    assert(c2.node.isUptoDate)

    var cell1 = c1.node.collapse()

    cell1 = get(cell1) // collapsing cell2 may affect cell1
    var cell2 = get(c2)
    cell2 = cell2.node.collapse()
    cell1 = get(cell1)


    assert(get(c1) == cell1)
    if c1.node.cells.filter(_.hasPointee).nonEmpty then
      c1.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(cell1.hasPointee)
          assert(get(f).equiv(cell1.getPointee))
      )

    assert(get(c2) == cell2)
    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
        f =>
          assert(cell2.hasPointee)
          assert(get(f).equiv(cell2.getPointee))
      )


    val collapsedNode = SadNode(this, cell1.node.bases ++ cell2.node.bases)
    collapsedNode.children.addAll(cell1.node.children)
    collapsedNode.children.addAll(cell2.node.children)
    val collapsedCell = collapsedNode.collapse()


    val pointee: Option[SadCell] = mergePointees(cell1, cell2)
    cell1 = get(cell1)
    cell2 = get(cell2)

    assert(get(c1) == cell1)
    if c1.node.cells.filter(_.hasPointee).nonEmpty then
      c1.node.cells.filter(_.hasPointee).map(_._pointee.get).foreach(
        f =>
          Logger.warn(get(f))
          Logger.warn(cell1.getPointee)

          assert(get(f).equiv(cell1.getPointee))
          assert(pointee.nonEmpty)
      )
    assert(get(c2) == cell2)
    if c2.node.cells.filter(_.hasPointee).nonEmpty then
      c2.node.cells.filter(_.hasPointee).map(_._pointee.get).foreach(
        f =>
          assert(get(f).equiv(cell2.getPointee))
          assert(pointee.nonEmpty)
      )


    if c1.hasPointee || c2.hasPointee then assert(pointee.nonEmpty)
    if pointee.nonEmpty then collapsedCell.setPointee(find(pointee.get))

    unify(cell1.node, collapsedNode)
    unify(cell2.node, collapsedNode)

    cell1 = get(cell1)
    cell2 = get(cell2)

    assert(cell1 == cell2)
    assert(collapsedCell == cell1)
    assert(get(c1) == get(c2))
    assert(get(c1) == cell1)
    c1.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
      f => assert(collapsedCell.getPointee.equiv(get(f)))
    )
    c2.node.cells.filter(_.hasPointee).map(_.getPointee).foreach(
      f => assert(collapsedCell.getPointee.equiv(get(f)))
    )
    collapsedCell
  }

  protected def mergeCellsHelper(cell1: SadCell, cell2: SadCell): SadCell = {
    assert(cell1.node.isUptoDate)
    assert(cell2.node.isUptoDate)
    val (stableCell, toBeMoved) = if cell1.interval.start > cell2.interval.start then (cell1, cell2) else (cell2, cell1)
    val delta = stableCell.interval.start - toBeMoved.interval.start

    val stableNode = stableCell.node
    val nodeToBeMoved = toBeMoved.node
    assert(stableCell.interval.isOverlapping(toBeMoved.move(i => i + delta).interval))

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

/*
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
    }*/

//    val oldPointees = resultNode.cells.map(
//      cell =>
//        (cell, allCells.filter(_.interval.isOverlapping((cell.interval))).collect{case cell: SadCell if cell._pointee.nonEmpty => cell._pointee})
//    ).toMap
    // unify old and new nodes
    unify(stableNode, resultNode)
    unify(nodeToBeMoved, resultNode, delta)

    assert(resultNode.cells.exists(c => c.interval.isOverlapping(stableCell.interval) && c.interval.isOverlapping(toBeMoved.move(i => i + delta).interval)))
    //set pointees
    val pointeeResult = mutable.Map[SadCell, SadCell]()
    resultNode.cells.foreach(
      cell =>
        var newCell = get(cell)
        val pointees = allCells.filter(_.interval.isOverlapping(cell.interval)) //newToOlds
          .collect {case cell: SadCell if cell._pointee.nonEmpty => find(cell._pointee.get)}
        if pointees.nonEmpty then
          val mergedPointees = mergeCells(pointees)
          newCell = get(newCell) // above merge may change the cell if the node points to itself
          pointeeResult.update(cell, mergedPointees)
          pointees.foreach(f =>
            assert(get(f).equiv(mergedPointees)))
          newCell.setPointee(mergedPointees)
          pointeeResult.keys.foreach(
            cell =>
              val actual = get(cell)
              val pointees = allCells.filter(c => c.interval.isOverlapping(cell.interval)).filter(_._pointee.nonEmpty).map(_._pointee.get)
              if !pointees.forall(f => get(f).equiv(actual.getPointee)) then
                Logger.debug(s"result node cell: $cell")
                Logger.debug(s"result cell udpated: ${actual} ")
                Logger.debug(s"Unified Cells: ${allCells.filter(_.interval.isOverlapping(cell.interval))}")
                Logger.debug(s"non-unified pointees: ${pointees}")
                Logger.debug(s"updated pointees: ${pointees.map(get)}")
                Logger.debug(s"updated result cell pointee: ${actual.getPointee}")
                pointees.foreach(f => assert(get(f).equiv(actual.getPointee), s"pointee $f has updated version ${get(f)},\n but the pointer $actual has pointee ${actual.getPointee}"))
          )
    )

    assert(resultNode.nonOverlappingProperty)
    find(stableCell)
  }

  override def mergeCells(c1: SadCell, c2: SadCell): SadCell = {
    require(c1.node.graph == c2.node.graph)
    val cell1 = find(c1)
    val cell2 = find(c2)
    Logger.debug(s"merging $cell1")
    if cell1.hasPointee then Logger.debug(s"and pointee ${cell1.getPointee}")

    Logger.debug(s"with $cell2")
    if cell2.hasPointee then Logger.debug(s"pointee ${cell2.getPointee}")


    val result = if cell1.equals(cell2) then
      Logger.debug(s"merged $cell1 with itself")
      cell1
    else if cell1.node.equals(cell2.node) then
      Logger.debug(s"collapsed $cell1 and $cell2")
      val res = cell1.node.collapse()
      Logger.debug(s"collapsed $cell1 and $cell2")
      res
    else if cell1.node.isCollapsed || cell2.node.isCollapsed then
      Logger.debug(s"merge and collapse $cell1 and $cell2")
      val res = collapseAndMerge(cell1, cell2)
      Logger.debug(s"merge and collapse $cell1 and $cell2")
      res
    else
      Logger.debug(s"merged $cell1 and $cell2")
      val res = mergeCellsHelper(cell1, cell2)
      Logger.debug(s"merged $cell1 and $cell2")
      res


    Logger.debug(s"Got result: $result")
    assert(result.equiv(get(cell1)))
    assert(result.equiv(get(cell2)))
    if cell1.hasPointee then assert(result.getPointee.equiv(get(cell1.getPointee)))
    if cell2.hasPointee then
//      Logger.debug(result.getPointee)
//      Logger.debug(get(cell2._pointee.get))
//      Logger.debug(cell2._pointee.get)
//      assert(result.getPointee == get(cell2._pointee.get))
    if cell1.node.cells.filter(_.hasPointee).nonEmpty then
      cell1.node.cells.filter(_.hasPointee).foreach(
        f =>
          if get(f.getPointee) != get(f).getPointee then
            Logger.debug(s"unifying $cell1 and $cell2")
            cell1.node.cells.foreach(
              cell =>
                Logger.debug(s"Node 1 had cell: $cell")
                if cell.hasPointee then
                  Logger.debug(s"with pointee ${cell._pointee}")
                  Logger.debug(s"updated pointee ${get(cell._pointee.get)}")
            )
            cell2.node.cells.foreach(
              cell =>
                Logger.debug(s"Node 2 had cell: $cell")
                if cell.hasPointee then
                  Logger.debug(s"with pointee ${cell._pointee}")
                  Logger.debug(s"updated pointee ${get(cell._pointee.get)}")

            )


            Logger.warn(f)
            Logger.warn(f._pointee)
            Logger.warn(f.getPointee)
            Logger.warn(get(f))
            Logger.warn(get(f).getPointee)
          assert(get(f.getPointee).equiv(get(f).getPointee))
          assert(get(f).node == result.node)
      )
    if cell2.node.cells.filter(_.hasPointee).nonEmpty then
      cell2.node.cells.filter(_.hasPointee).foreach(
        f =>
          assert(f.getPointee.equiv(get(f).getPointee))
          assert(get(f).node == result.node)
      )

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
    val res = newNode.add(newInterval)
    if (findNode(res.node)._1 != res.node) then
      Logger.warn(res.node)
      Logger.warn(findNode(res.node)._1)
    assert(findNode(res.node)._1 == res.node)
    res
  }

  def findNode(node: SadNode): (SadNode, Int) = {
    val (term, offset) = solver.findWithOffset(node.term)
    assert(solver.findWithOffset(term)._1 == term)
    (term.asInstanceOf[NodeTerm].v, offset)
  }

  override def find(node: SadNode): SadNode = {
    findNode(node)._1
  }

  def get(cell: SadCell): SadCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.get(newInterval)
    if (findNode(res.node)._1 != res.node) then
      Logger.warn(res.node)
      Logger.warn(findNode(res.node)._1)
    assert(findNode(res.node)._1 == res.node)
    res
  }

  def unify(a: SadNode, b: SadNode, offset: Int = 0): Unit = {
    Logger.debug(s"unifying ${b.id} with ${a.id} at offset ${offset}")
    solver.unify(a.term, b.term, offset)
  }
}

class SadNode(val graph: SadGraph, val bases: mutable.Set[SymBase]= mutable.Set.empty, size: Option[Int] = None, val id: Int = SadNodeCounter.increment()) extends DSANode[SadCell](size) {

  Logger.debug(s"created node with id $id")

  // clones a this node into newGraph
  //
  def clone(newGraph: SadGraph, recurse: Boolean = false,
            oldToNew: mutable.Map[SadNode, SadNode] = mutable.Map()): SadNode  = {
    //    val (node, _) = graph.findNode(this)
    if recurse then assert(isUptoDate)
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
        newNode


    if recurse then
      val queue = mutable.Queue[SadNode](node)
      while queue.nonEmpty do
        val old = queue.dequeue()
        assert(oldToNew.contains(old))
        val (newNode, off) = newGraph.findNode(oldToNew(old))
        old.cells.foreach {
          case cell: SadCell if cell.hasPointee =>
            val pointee = cell.getPointee
            assert(pointee == graph.find(cell).getPointee)
            val pointeeNode = pointee.node
            assert(pointeeNode.isUptoDate)
            if !oldToNew.contains(pointeeNode) then queue.enqueue(pointeeNode)
            val (clonedNode, clonedOff) =
              if !oldToNew.contains(pointeeNode) then
                newGraph.findNode(pointeeNode.clone(newGraph, false, oldToNew))
              else newGraph.findNode(oldToNew(pointeeNode))

            val pointer = newNode.get(cell.interval.move(i => i + off))
            assert(pointer == newGraph.find(pointer))
            val clonedPointee = clonedNode.get(pointee.interval.move(i => i + clonedOff))
            pointer.setPointee(clonedNode.get(pointee.interval.move(i => i + clonedOff)))
            assert(newNode.get(cell.interval.move(i => i + off)).getPointee == newGraph.find(clonedNode.get(pointee.interval.move(i => i + clonedOff))))
          case _ =>
        }

      oldToNew.foreach {
        case (old, cloned) =>
          old.cells.filter(_.hasPointee).map(_.getPointee).foreach(
            pointee =>
              assert(oldToNew.contains(pointee.node))
          )
      }

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

  def isUptoDate: Boolean = {
    graph.findNode(this)._1 == this
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

    assert(isUptoDate)
    val node = this
    if (!(node.isCollapsed)) {
      val oldPointees = cells.filter(_.hasPointee).map(_.getPointee)
      val collapseNode: SadNode = SadNode(graph, bases, size)
      collapseNode.children.addAll(this.children)
      var collapsedCell: SadCell = collapseNode.add(0)
      collapseNode._collapsed = Some(collapsedCell)
      // delay unification
      // treat collapsed node completely distinct to current node
      if  node.cells.exists(_.hasPointee) then
        val pointToItself = node.cells.filter(_.hasPointee).map(_.getPointee).exists(c => c.node == node)
        var pointees = node.cells.filter(_.hasPointee).map(_.getPointee).filter(_.node != node)
        if pointToItself then pointees.map(f => graph.find(f).node.collapse())
        pointees = pointees.map(graph.find)
        var pointee = if pointees.nonEmpty then graph.mergeCells(pointees) else collapsedCell

        if (pointToItself) {
          assert(!collapsedCell.hasPointee)
          val oldPointee = pointee.node
          assert(collapseNode.isUptoDate)
          assert(pointee.node.isCollapsed)
          pointee = graph.mergeCells(pointee, collapsedCell)
          Logger.debug(pointee)
          collapsedCell = pointee
        }

        if !pointToItself then assert(!collapsedCell.hasPointee)
//        if pointToItself then println("pointing to itself") else println("not pointing to itself")
        collapsedCell.setPointee(pointee)
        collapsedCell = graph.find(collapsedCell)
        if pointToItself then assert(collapsedCell.getPointee == collapsedCell)
        assert(collapsedCell.hasPointee)
        assert(collapsedCell.getPointee == graph.find(pointee))
        assert(collapsedCell.node.cells.size == 1)

      // unify collapsed node and current node
      graph.unify(node, collapsedCell.node)
      cells.foreach(f =>
        assert(graph.find(f) == collapsedCell)
      )
      if node.cells.exists(_.hasPointee) then assert(graph.find(collapsedCell).hasPointee)
/*
      if (!oldPointees.map(graph.find).forall(f => f == graph.find(collapsedCell).getPointee)) then
        print("")*/
      assert(oldPointees.map(graph.find).forall(f => f.equiv(graph.find(collapsedCell).getPointee)))
      graph.find(collapsedCell)


    } else {
      graph.find(node.collapsed.get)
    }

  }


  override def add(interval: Interval): SadCell = {
    assert(isUptoDate)
    if !isCollapsed then
      val overlapping: Seq[SadCell] = cells.filter(_.interval.isOverlapping(interval))
      val newCell = if overlapping.isEmpty then
        val res = init(interval)
        _cells = _cells.appended(res)
        res
      else if overlapping.size == 1 && overlapping.head.interval == (interval) then
        this.get(interval)
      else if overlapping.size == 1 && overlapping.head.interval.contains(interval) then
        init(interval)
      else
        val unifiedInterval = overlapping.map(_.interval).fold(interval)(Interval.join)
        val res = init(unifiedInterval)
        val pointees = overlapping.filter(_.hasPointee).map(_.getPointee)

        val pointee = if pointees.nonEmpty then  Some(graph.mergeCells(pointees)) else None
        _cells = cells.diff(overlapping).appended(res).sorted
        if pointees.nonEmpty then res.setPointee(pointee.get)
        res

      assert(nonOverlappingProperty)
      newCell
    else
      collapsed.get
  }


  def get(interval: Interval): SadCell = {
    if isCollapsed then collapsed.get else
      val exactMatches = cells.filter(_.interval.contains(interval))
      if exactMatches.size != 1 then
        println(exactMatches.size)
        println(cells)
        println(interval)
      assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
      exactMatches.head
  }
}


// A DSA Cell 
class SadCell(val node: SadNode, override val interval: Interval) extends NodeCell(interval) {
  var _pointee: Option[SadCell] = None
  private val graph: SadGraph = node.graph

  override def toString: String = s"Cell($node, $interval)"

  def move(f: Int => Int): SadCell = {
    val newCell = SadCell(node, interval.move(f))
    newCell._pointee = _pointee
    newCell
  }

  override def equals(obj: Any): Boolean = {
    obj match
      case other: SadCell => other.node == this.node && other.interval == this.interval
      case _ => false
  }

  override def hashCode(): Int = {
    node.hashCode() * 23 + interval.hashCode() * 31
  }


  def equiv(other: SadCell): Boolean = {
    this.node.get(this.interval).equals(other.node.get(other.interval))
//    other.node == this.node && (other.interval.contains(this.interval) || this.interval.contains(other.interval))
  }

  def grow(interval: Interval): SadCell = {
    require(this.interval.start == interval.start, "expected same interval start for growing cell")
    val newCell = SadCell(this.node, Interval(interval.start, math.max(interval.end, this.interval.end)))
    newCell._pointee = _pointee
    newCell
  }

  def removePointee: Option[SadCell] = {
    if node.get(this.interval) != this then
      node.get(this.interval).removePointee
    else
      val temp = _pointee.map(graph.find)
      _pointee = None
      temp
  }

  def getPointee: SadCell =
  {
    if node.get(this.interval) != this then
      node.get(this.interval).getPointee
    else if _pointee.isEmpty then
      assert(this.node.isUptoDate)
      _pointee = Some(SadNode(graph, mutable.Set.empty).add(0))
      _pointee.get
    else
      graph.find(_pointee.get)

  }
  def hasPointee: Boolean = node.get(this.interval)._pointee.nonEmpty

  def setPointee(cell: SadCell): SadCell = {
    assert(this.node.isUptoDate)
    assert(cell.node.isUptoDate)
    if node.get(this.interval) != this then
      node.get(this.interval).setPointee(cell)
    else if _pointee.isEmpty then
      _pointee = Some(cell)
      _pointee.get
    else  // if a cell points to itself break the link,
      graph.mergePointees(this.getPointee, cell)
      // TODO possibly update
      this._pointee = Some(graph.mergeCells(cell, graph.get(this).getPointee))
      graph.get(this)._pointee = this._pointee
      _pointee.get
  }
}

object SadDSA {
  def getLocal(proc: Procedure, symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None,
              ): SadGraph = {
    val graph = SadGraph(proc, Local, symValues, cons)
    graph.localPhase()
    graph.localCorrectness()
    graph
  }
}




