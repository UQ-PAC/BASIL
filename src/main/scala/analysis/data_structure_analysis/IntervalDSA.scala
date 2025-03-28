package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.{BU, Local, TD}
import analysis.data_structure_analysis.Interval.Top
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import boogie.SpecGlobal
import specification.FuncEntry
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.*
import specification.{ExternalFunction, SymbolTableEntry}
import translating.PrettyPrinter.pp_proc
import util.{DSALogger, IRContext, IntervalDSALogger as Logger}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}

private val intervalNodeCounter = util.Counter()

case class NodeTerm(v: IntervalNode) extends analysis.solvers.Var[NodeTerm]

/**
 * Data Structure Graph
 */
class IntervalGraph(
  val proc: Procedure,
  var phase: DSAPhase,
  val irContext: IRContext,
  val sva: SymValues[Interval],
  val constraints: Set[Constraint],
  val nodeBuilder: Option[() => Map[SymBase, IntervalNode]]
) {
//  def this(proc: Procedure, phase: DSAPhase, irContext: IRContext) = {
//    this(proc, phase, irContext, getSymbolicValues(proc), generateConstraints(proc), None)
//  }

  val solver = OffsetUnionFindSolver[NodeTerm]()
  val builder: () => Map[SymBase, IntervalNode] = nodeBuilder.getOrElse(buildNodes)
  var nodes: Map[SymBase, IntervalNode] = builder()

  def exprToSymVal(expr: Expr): SymValSet[Interval] = SymValues.exprToSymValSet(sva)(expr)

  protected def symValToNodes(
    symVal: SymValSet[Interval],
    current: Map[SymBase, IntervalNode],
    f: Int => Boolean = i => i >= 1000
  ): Map[SymBase, IntervalNode] = {
    symVal.state.filter((base, _) => base != NonPointer).foldLeft(current) { case (result, (base, symOffsets)) =>
      val node = find(result.getOrElse(base, init(base, None)))
      base match
        case Heap(call) => node.flags.heap = true
        case Stack(proc) => node.flags.stack = true
        case Global => node.flags.global = true
        case NonPointer =>
          throw new Exception("Attempted to create a node from an Non-pointer symbolic base")
        case unknown: (Ret | Loaded | Par) =>
          node.flags.unknown = true
          node.flags.incomplete = true
      if symOffsets == Top then node.collapse()
      else symOffsets.toIntervals.filter(i => f(i.end.get) || base != Global).map(node.add)
      result + (base -> node)
    }
  }

  // takes a map from symbolic bases to nodes and updates it based on constraint
  protected def binaryConstraintToNodes(
    constraint: BinaryConstraint,
    nodes: Map[SymBase, IntervalNode]
  ): Map[SymBase, IntervalNode] = {
    val arg1 = exprToSymVal(constraint.arg1.value) // .removeNonAddress(i => i >= 1000)
    val arg2 = exprToSymVal(constraint.arg2.value) // .removeNonAddress(i => i >= 1000)
    val res = symValToNodes(arg1, nodes)
    symValToNodes(arg2, res)
  }

  def buildNodes(): Map[SymBase, IntervalNode] = {
    val global =
      globalNode(irContext.globals ++ irContext.funcEntries, irContext.globalOffsets, irContext.externalFunctions)
    val init = sva.state.foldLeft(Map[SymBase, IntervalNode](Global -> global)) { case (m, (variable, valueSet)) =>
      symValToNodes(valueSet, m)
    }

    constraints.foldLeft(init) { case (resultMap, constraint) =>
      constraint match
        case constraint: BinaryConstraint => binaryConstraintToNodes(constraint, resultMap)
        case dcc @ DirectCallConstraint(call) =>
          (dcc.inConstraints ++ dcc.outConstraints).foldLeft(resultMap) { case (updated, constraint) =>
            binaryConstraintToNodes(constraint, updated)
          }
        case _ => resultMap
    }
  }

  def resolveIndirectCall(dcc: DirectCallConstraint): Set[Procedure] = {
    require(
      dcc.target.name == "indirect_call_launchpad",
      s"$dcc is not a constraint on a call for an indirect call wrapper"
    )
    val (formal, actual) = dcc.inParams.collectFirst {
      case (f, a) if f.name.startsWith("indirectCallTarget") => (f, a)
    }.get
    exprToCells(actual).map(get).foldLeft(Set[Procedure]()) { (s, cell) =>
      s ++ cellToProcs(cell)
    }
  }

  def resolveIndirectCalls(constraints: Set[Constraint] = this.constraints): Map[Constraint, Set[Procedure]] = {
    constraints.foldLeft(Map[Constraint, Set[Procedure]]()) { (m, con) =>
      con match
        case dcc: DirectCallConstraint if dcc.target.name == "indirect_call_launchpad" =>
          m + (con -> resolveIndirectCall(dcc))
        case _ => m
    }
  }

  def cellToProcs(cell: IntervalCell): Set[Procedure] = {
    val symBase = Global
    val globalNode = nodes(Global)
    val actual = cell.node.get(cell.interval)
    irContext.funcEntries.foldLeft(Set[Procedure]()) { (s, funEntry) =>
      if actual == get(globalNode.get(funEntry.address.toInt)) then
        s + irContext.program.procedures.filter(p => p.procName == funEntry.name).head
      else s
    }
  }

  def isGlobal(address: Int): Boolean = {
    (irContext.globals ++ irContext.funcEntries).exists(g =>
      Interval(g.address.toInt, g.address.toInt + (g.size / 8)).contains(address)
    ) || irContext.globalOffsets.exists((g1, g2) =>
      Interval(g1.toInt, g1.toInt + 8).contains(address)
        || Interval(g2.toInt, g2.toInt + 8).contains(address)
    )
  }
  def globalNode(
    globals: Set[SymbolTableEntry],
    globalOffsets: Map[BigInt, BigInt],
    externalFunctions: Set[ExternalFunction]
  ): IntervalNode = {
    val symBase = Global
    val globalNode = IntervalNode(this, mutable.Map(symBase -> 0))
    globalNode.add(0)
    globalNode.flags.global = true
    globals.foreach {
      case FuncEntry(name, size, address) =>
        globalNode.add(Interval(address.toInt, address.toInt + (size / 8)))
      case SpecGlobal(name, size, arraySize, address) =>
        globalNode.add(Interval(address.toInt, address.toInt + (size / 8)))
    }

    globalOffsets.foreach { case (address, relocated) =>
      globalNode.add(address.toInt)
      globalNode.add(relocated.toInt)
    }

    globalOffsets.map(_.swap).foreach { case (address, relocated) =>
      val pointee = find(globalNode.get(address.toInt))
      val pointer = find(globalNode).add(Interval(relocated.toInt, relocated.toInt + 8))
      pointer.setPointee(pointee)
    }

    globalNode
  }

  // Processes all non call constraints
  def localPhase(): Unit = {
    var processed = Set[Constraint]()
    constraints.toSeq
      .sortBy(f => f.label)
      .foreach(c =>
        localCorrectness(processed)
        processed += c
        processConstraint(c)
        localCorrectness(processed)
      )
  }

  // returns the cells corresponding to the
  def symValToCells(symVal: SymValSet[Interval]): Set[IntervalCell] = {
    val pairs = symVal.state.filter((base, _) => base != NonPointer)
    pairs.foldLeft(Set[IntervalCell]()) { case (results, (base: SymBase, offsets: Interval)) =>
      val (node, adjustment) = findNode(nodes(base))
      if offsets == Top then results + node.collapse()
      else
        results ++ offsets.toIntervals
          .filter(i => base != Global || i.end.get >= 1000)
          .map(_.move(i => i + adjustment))
          .map(node.add)
    }
  }

  def contextTransfer(phase: DSAPhase, graphs: Map[Procedure, IntervalGraph]): Unit = {
    require(phase == TD || phase == BU)
    this.phase = phase
    constraints.toSeq.sortBy(c => c.label).foreach {
      case dcc: DirectCallConstraint if graphs.contains(dcc.target) && !dcc.target.isExternal.getOrElse(false) =>
        val (source, target) = if phase == TD then (this, graphs(dcc.target)) else (graphs(dcc.target), this)
        callTransfer(phase, dcc, source, target)
      case dcc: DirectCallConstraint if dcc.target.name == "indirect_call_launchpad" =>
        resolveIndirectCall(dcc)
          .filterNot(proc =>
            dcc.call.parent.parent == proc || proc.isExternal.getOrElse(false) || proc.name.startsWith("_")
          )
          .foreach(proc =>

            val (source, target) = if phase == TD then (this, graphs(proc)) else (graphs(proc), this)
            callTransfer(phase, dcc, source, target)
          )
      case _ =>
    }

  }

  def globalTransfer(source: IntervalGraph, target: IntervalGraph): Unit = {
    DSALogger.info(s"cloning globalNode from ${source.proc.procName}")
    val oldToNew = mutable.Map[IntervalNode, IntervalNode]()
    val targetGlobal = target.find(target.nodes(Global).get(0))
    var sourceGlobal = source.find(source.nodes(Global).get(0))
    val old = source.nodes(Global).clone(target, false, oldToNew)
    val globalNode = sourceGlobal.node.clone(target, true, oldToNew)

    sourceGlobal = globalNode.get(targetGlobal.interval)
    target.mergeCells(sourceGlobal, targetGlobal)
  }
  def callTransfer(phase: DSAPhase, cons: DirectCallConstraint, source: IntervalGraph, target: IntervalGraph): Unit = {
    require(phase == TD || phase == BU)
    val oldToNew = mutable.Map[IntervalNode, IntervalNode]()
//    val targetGlobal = target.find(target.nodes(Global).get(0))
//    var sourceGlobal = source.find(source.nodes(Global).get(0))
//    val old = source.nodes(Global).clone(target, false, oldToNew)
//    val globalNode = sourceGlobal.node.clone(target, true, oldToNew)
//
//    sourceGlobal = globalNode.get(targetGlobal.interval)
//    target.mergeCells(sourceGlobal, targetGlobal)
    DSALogger.info(s"cloning ${source.proc.procName} into ${target.proc.procName}, $phase")
    cons.inParams
      .filterNot(f => f._1.name.startsWith("R31"))
      .filter(f => cons.target.formalInParam.contains(f._1))
      .foreach { case (formal, actual) =>
        val (sourceExpr, targetExpr) = if phase == TD then (actual, formal) else (formal, actual)
        exprTransfer(sourceExpr, targetExpr, source, target, oldToNew)
      }

    cons.outParams
      .filterNot(f => f._1.name.startsWith("R31"))
      .filter(f => cons.target.formalOutParam.contains(f._1))
      .foreach { case (out, actual) =>
        val (sourceExpr, targetExpr) = if phase == TD then (actual, out) else (out, actual)
        exprTransfer(sourceExpr, targetExpr, source, target, oldToNew)
      }
    // TODO add unification between unused indirect call out params and their corresponding input version
    /*
    if phase == BU then
      cons.outParams.filterNot(f => cons.target.formalOutParam.contains(f._2)).foreach {
        f =>
      }*/
  }

  def exprTransfer(
    sourceExpr: Expr,
    targetExpr: Expr,
    source: IntervalGraph,
    target: IntervalGraph,
    oldToNew: mutable.Map[IntervalNode, IntervalNode]
  ): Unit = {
    val sourceCells = source
      .exprToCells(sourceExpr)
      .map(source.find)
      .map(cell =>
        assert(cell.node.isUptoDate)
        val (node, offset) =
          target.findNode(cell.node.clone(target, true, oldToNew))
        if offset == 0 then node.get(cell.interval)
        node.get(cell.interval.move(i => i + offset))
      )
    val targetCells = target.exprToCells(targetExpr).map(target.find)
    target.localCorrectness()

    if (targetCells ++ sourceCells).nonEmpty then target.mergeCells(targetCells ++ sourceCells)
    target.localCorrectness()
  }

  private def isIndirectCall(dcc: DirectCallConstraint): Boolean = {
    dcc.target.name == "indirect_call_launchpad"
  }

  // find the corresponding cells for a expr from this graph's procedure
  def exprToCells(expr: Expr): Set[IntervalCell] = {
    symValToCells(exprToSymVal(expr))
  }

  /**
   * Checks local phase memory (memory access) constraints are maintained
   *
   * For a memory access constraint
   * checks that the cell representing the memory accessed index points to
   * the cell representing the value assigned or read from the memory region
   *
   * It additionally checks that constraint checking is performed on the
   * most up-to-date version of each node,
   * guarantees that a later unification hasn't broken the local memory constraints
   * @param constraints constriants to check
   */
  def localCorrectness(constraints: Set[Constraint] = this.constraints): Unit = {
    constraints.toSeq.sortBy(f => f.label).foreach {
      case constraint: MemoryAccessConstraint[_] =>
        val valueCells = constraintArgToCells(constraint.arg2).map(get)
        assert(valueCells.size <= 1, s"value cells should be unified instead got $valueCells")
        var valueCell: Option[IntervalCell] = None
        if valueCells.size == 1 then valueCell = Some(valueCells.head)

        val indexCells = constraintArgToCells(constraint.arg1, ignoreContents = true).map(get)
        var indexCell: Option[IntervalCell] = None
        if indexCells.nonEmpty then
          if indexCells.nonEmpty && valueCells.nonEmpty then
            indexCells.foreach(indexCell =>
              if !(indexCell.hasPointee && indexCell.getPointee == valueCell.get) then
                Logger.debug(s"index has a pointer: ${indexCell.hasPointee}")
                Logger.debug(s"index cell: ${indexCell}")
                if indexCell.hasPointee then
                  Logger.debug(s"index pointee: ${indexCell.getPointee}")
                  Logger.debug(s"index pointee children: ${indexCell.getPointee.node.children}")
                Logger.debug(s"got valueCell: ${valueCell.get}")
                Logger.debug(s"value childeren: ${valueCell.get.node.children}")
              assert(indexCell.node.isUptoDate, "outdated cell in local correctness check")
              assert(indexCell.getPointee.node.isUptoDate, "outdated cell in local correctness check")
              assert(valueCell.get.node.isUptoDate, "outdated cell in local correctness check")

              assert(
                indexCell.hasPointee && indexCell.getPointee.equiv(valueCell.get),
                s"$constraint, $indexCell doesn't point to ${valueCell.get} instead ${indexCell.getPointee}"
              )
            )

      case _ =>
    }
  }

  var last: Option[(IntervalCell, IntervalCell)] = None
  var secondLast: Option[(IntervalCell, IntervalCell)] = None

  /**
   * Clones a graph
   * all middle nodes in the union-find are lost
   * @return cloned graph
   */
  override def clone: IntervalGraph = {
    val oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map()
    val copy = IntervalGraph(proc, phase, irContext, sva, constraints, Some(() => Map[SymBase, IntervalNode]()))
    this.nodes.foreach { // in addition to current nodes
      case (base, node) => // clone old nodes in base to node map to carry offset info
        val (current, offset) = this.findNode(node)
        if current != node then
          val oldCopy = node.clone(copy, false, oldToNew)
          val curCopy = current.clone(copy, true, oldToNew)
          copy.unify(oldCopy, curCopy, offset)
        else current.clone(copy, true, oldToNew)
    }

    assert(
      copy.solver.size <= this.solver.size,
      s"size of copy's solver ${copy.solver.size}," +
        s"size of this's solver ${this.solver.size}"
    )
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
      structs.append(DotStruct(n.id.toString, n.toString, Some(n.cells.map(o => o.interval.start.get.toString)), true))
    }

    pointsTo.foreach { (pointer, pointee) =>
      val pointerID = pointer.node.id.toString
      val pointerOffset = pointer.interval.start.get.toString
      arrows.append(
        StructArrow(
          DotStructElement(pointerID, Some(pointerOffset)),
          DotStructElement(pointee.node.id.toString, Some(pointee.interval.start.get.toString))
        )
      )
    }
    StructDotGraph(proc.name, structs, arrows).toDotString
  }

  protected def collect(): (Set[IntervalNode], Set[(IntervalCell, IntervalCell)]) = {
    val nodes: mutable.Set[IntervalNode] = mutable.Set()
    val pointsTo: mutable.Set[(IntervalCell, IntervalCell)] = mutable.Set()
    constraints.foreach {
      case constraint: BinaryConstraint =>
        val valueCells = constraintArgToCells(constraint.arg2).map(get)
        val indexCells = constraintArgToCells(constraint.arg1, ignoreContents = true).map(get)
        valueCells.foreach(c => nodes.add(c.node))
        indexCells.foreach(c => nodes.add(c.node))
        indexCells.foreach(pointer => valueCells.foreach(pointee => pointsTo.add(pointer, pointee)))
      case _ =>
    }

    (nodes.toSet, pointsTo.toSet)
  }

  def init(symBase: SymBase, size: Option[Int]): IntervalNode = IntervalNode(this, mutable.Map(symBase -> 0), size)
  def init(symBases: mutable.Map[SymBase, Int], size: Option[Int]): IntervalNode = IntervalNode(this, symBases, size)
  def constraintArgToCells(constraintArg: ConstraintArg, ignoreContents: Boolean = false): Set[IntervalCell] = {
    val cells = symValToCells(exprToSymVal(constraintArg.value))
    val exprCells = cells.map(find)

    if constraintArg.contents && !ignoreContents then exprCells.map(_.getPointee)
    else exprCells

  }

  def processConstraint(constraint: Constraint): Unit = {
    constraint match
      case cons: MemoryAccessConstraint[_] =>
        Logger.debug(s"Processing constraint $cons")
        val indices = constraintArgToCells(cons.arg1, ignoreContents = true)
        val indexPointee = constraintArgToCells(cons.arg1)
        val values = constraintArgToCells(cons.arg2)
        val first = if indexPointee.nonEmpty then
          indices
            .map(findExact)
            .foreach { case (node, interval) =>
              if cons.size > 0 then node.add(interval.growTo(cons.size - 1))
            }
          val res = mergeCells(indexPointee)
          val correctPointee =
            indices
              .map(get)
              .foldLeft(true)((f: Boolean, pointer: IntervalCell) =>
                f && pointer.hasPointee && pointer.getPointee.equiv(res)
              )
          assert(correctPointee)
          Some(res)
        else None
        val sec = if values.nonEmpty then Some(mergeCells(values)) else None
        if first.nonEmpty && sec.nonEmpty then
          val res = mergeCells(first.get, sec.get)
          assert(constraintArgToCells(cons.arg1).map(get) == constraintArgToCells(cons.arg2).map(get))
          val correctPointee =
            indices
              .map(get)
              .foldLeft(true)((f: Boolean, pointer: IntervalCell) =>
                f && pointer.hasPointee && pointer.getPointee.equiv(first.map(get).get)
              )
          assert(correctPointee, "an index cell doesn't point to it's pointee")
          assert(first.map(get) == sec.map(get), "cells should be the same after unification")
        else Logger.warn(s"$cons had an empty argument")

      case dc: DirectCallConstraint =>
        val h = dc.inParams.filter(f => f._1.name.startsWith("R31"))
        val g = dc.outParams.filter(f => f._1.name.startsWith("R31"))
        if g.nonEmpty && h.nonEmpty then
          val (_, in) = h.head
          val (_, out) = g.head
          mergeCells(exprToCells(in) ++ exprToCells(out))
      case _ => // ignore
  }

  /**
   * merge the pointees of the two cells and return it
   * None if neither of them has a pointee
   */
  def mergePointees(c1: IntervalCell, c2: IntervalCell): Option[IntervalCell] = {
    var cell1 = get(c1)
    var cell2 = get(c2)
    if cell1 != cell2 then
      (cell1.hasPointee, cell2.hasPointee) match
        case (true, true) =>
          val pointee1 = cell1.removePointee.get
          val pointee2 = cell2.removePointee.get
          var resPointee = mergeCells(pointee1, pointee2)
          cell1 = get(cell1)
          resPointee = cell1.setPointee(resPointee)
          cell2 = get(cell2)
          val res = Some(cell2.setPointee(resPointee))
          res
        case (_, true) => Some(cell1.setPointee(cell2.getPointee))
        case (true, _) => Some(cell2.setPointee(cell1.getPointee))
        case (_, _) => None // Logger.warn(s"neither $cell1, or $cell2 had a pointee")
    else Some(cell1.getPointee)

  }

  protected def collapseAndMerge(c1: IntervalCell, c2: IntervalCell): IntervalCell = {
    assert(c1.node.isUptoDate)
    assert(c2.node.isUptoDate)

    var cell1 = c1.node.collapse()

    cell1 = get(cell1) // collapsing cell2 may affect cell1
    var cell2 = get(c2)
    cell2 = cell2.node.collapse()
    cell1 = get(cell1)

    val collapsedNode = IntervalNode(this, cell1.node.bases ++ cell2.node.bases)
    collapsedNode.flags.join(cell1.node.flags)
    collapsedNode.flags.join(cell2.node.flags)
    collapsedNode.children.addAll(cell1.node.children)
    collapsedNode.children.addAll(cell2.node.children)
    val collapsedCell = collapsedNode.collapse()

    val pointee: Option[IntervalCell] = mergePointees(cell1, cell2)
    cell1 = get(cell1)
    cell2 = get(cell2)

    if c1.hasPointee || c2.hasPointee then assert(pointee.nonEmpty)
    if pointee.nonEmpty then collapsedCell.setPointee(find(pointee.get))

    unify(cell1.node, collapsedNode)
    unify(cell2.node, collapsedNode)

    cell1 = get(cell1)
    cell2 = get(cell2)

    collapsedCell
  }

  protected def mergeCellsHelper(cell1: IntervalCell, cell2: IntervalCell): IntervalCell = {
    assert(cell1.node.isUptoDate)
    assert(cell2.node.isUptoDate)
    val (stableCell, toBeMoved) =
      if cell1.interval.start.get > cell2.interval.start.get then (cell1, cell2) else (cell2, cell1)
    val delta = stableCell.interval.start.get - toBeMoved.interval.start.get

    val stableNode = stableCell.node
    val nodeToBeMoved = toBeMoved.node
    assert(stableCell.interval.isOverlapping(toBeMoved.moved(i => i + delta).interval))

    val stableCells = stableNode.cells
    val movedCells = nodeToBeMoved.cells.map(_.moved(i => i + delta))
    val allCells = (stableCells ++ movedCells).sorted
    val updatedBases = stableNode.bases ++ (nodeToBeMoved.bases.view.mapValues(f => f + delta))
    val resultNode = IntervalNode(this, updatedBases)
    Logger.debug(s"created result node with id ${resultNode.id}")
    resultNode.children.addAll(stableNode.children ++ nodeToBeMoved.children ++ Set(stableNode.id, nodeToBeMoved.id))
    resultNode.flags.join(stableNode.flags)
    resultNode.flags.join(nodeToBeMoved.flags)
    val queue: mutable.Queue[IntervalCell] = mutable.Queue(allCells: _*)
    allCells.foreach(c => resultNode.add(c.interval))

    // unify old and new nodes
    unify(stableNode, resultNode)
    unify(nodeToBeMoved, resultNode, delta)

    assert(
      resultNode.cells.exists(c =>
        c.interval.isOverlapping(stableCell.interval) && c.interval
          .isOverlapping(toBeMoved.moved(i => i + delta).interval)
      )
    )
    // set pointees
    val pointeeResult = mutable.Map[IntervalCell, IntervalCell]()
    resultNode.cells.foreach(cell =>
      var newCell = get(cell)
      val pointees = allCells
        .filter(_.interval.isOverlapping(cell.interval)) // newToOlds
        .collect { case cell: IntervalCell if cell._pointee.nonEmpty => find(cell._pointee.get) }
      if pointees.nonEmpty then
        val mergedPointees = mergeCells(pointees)
        newCell = get(newCell) // above merge may change the cell if the node points to itself
        pointeeResult.update(cell, mergedPointees)
        pointees.foreach(f => assert(get(f).equiv(mergedPointees)))
        newCell.setPointee(mergedPointees)
        pointeeResult.keys.foreach(cell =>
          val actual = get(cell)
          val pointees = allCells
            .filter(c => c.interval.isOverlapping(cell.interval))
            .filter(_._pointee.nonEmpty)
            .map(_._pointee.get)
          if !pointees.forall(f => get(f).equiv(actual.getPointee)) then
            Logger.debug(s"result node cell: $cell")
            Logger.debug(s"result cell udpated: ${actual} ")
            Logger.debug(s"Unified Cells: ${allCells.filter(_.interval.isOverlapping(cell.interval))}")
            Logger.debug(s"non-unified pointees: ${pointees}")
            Logger.debug(s"updated pointees: ${pointees.map(get)}")
            Logger.debug(s"updated result cell pointee: ${actual.getPointee}")
            pointees.foreach(f =>
              assert(
                get(f).equiv(actual.getPointee),
                s"pointee $f has updated version ${get(f)},\n but the pointer $actual has pointee ${actual.getPointee}"
              )
            )
        )
    )

    assert(resultNode.nonOverlappingProperty)
    find(stableCell)
  }

  def mergeCells(c1: IntervalCell, c2: IntervalCell): IntervalCell = {

    require(c1.node.graph == c2.node.graph && c1.node.graph == this)
    val cell1 = find(c1)
    val cell2 = find(c2)
    Logger.debug(s"merging $cell1")
    if cell1.hasPointee then Logger.debug(s"and pointee ${cell1.getPointee}")
    val node1Pointees = cell1.node.cells.filter(_.hasPointee).map(c => (c, c.getPointee)).toMap
    val node2Pointees = cell2.node.cells.filter(_.hasPointee).map(c => (c, c.getPointee)).toMap

    Logger.debug(s"with $cell2")
    if cell2.hasPointee then Logger.debug(s"pointee ${cell2.getPointee}")

    val result = if cell1.node == cell2.node && cell1.interval.start.equals(cell2.interval.start) then
      assert(cell1.node.get(cell1.interval).interval.contains(cell2.interval))
      Logger.debug(s"merged $cell1 with itself")
      cell1
    else if cell1.node.equals(cell2.node) then
      println(s"hit this case in $proc")
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
    if cell2.hasPointee then assert(result.getPointee.equiv(get(cell2.getPointee)))

    if node1Pointees.nonEmpty then
      node1Pointees.foreach((pointer, pointee) =>
        assert(
          get(pointee).equiv(get(pointer).getPointee),
          s"$pointer doesn't point to updated version of its pointee after merge"
        )
        assert(get(pointer).node == result.node)
      )

    if node2Pointees.nonEmpty then
      node2Pointees.foreach((pointer, pointee) =>
        assert(
          get(pointee).equiv(get(pointer).getPointee),
          s"$pointer doesn't point to updated version of its pointee after merge"
        )
        assert(get(pointer).node == result.node)
      )
    result
  }

  def mergeCells(cells: Iterable[IntervalCell]): IntervalCell = {
    require(cells.nonEmpty, "can't merge empty set of cells")
    cells.tail.foldLeft(cells.head) { (result, cell) =>
      mergeCells(result, cell)
    }
  }

  def findExact(cell: IntervalCell): (IntervalNode, Interval) = {
    val node = cell.node
    val (newNode, offset) = findNode(node)
    (newNode, cell.interval.move(i => i + offset))
  }

  def findNode(node: IntervalNode): (IntervalNode, Int) = {
    val (term, offset) = solver.findWithOffset(node.term)
    assert(solver.findWithOffset(term)._1 == term, "find doesnt' get the most up-to-date node")
    (term.asInstanceOf[NodeTerm].v, offset)
  }

  def find(node: IntervalNode): IntervalNode = {
    findNode(node)._1
  }

  // find the most uptodate version of cell
  // if the cell was unified changing it's size
  // creates a dummy cell with cell's interval size
  def find(cell: IntervalCell): IntervalCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.add(newInterval)
    if (findNode(res.node)._1 != res.node) then
      Logger.warn(res.node)
      Logger.warn(findNode(res.node)._1)
    assert(findNode(res.node)._1 == res.node)
    res
  }

  // find the most uptodate version of cell
  // if cell was unified with others, retuns a cell with
  // unified interval
  def get(cell: IntervalCell): IntervalCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.get(newInterval)
    if (findNode(res.node)._1 != res.node) then
      Logger.warn(res.node)
      Logger.warn(findNode(res.node)._1)
    assert(findNode(res.node)._1 == res.node)
    res
  }

  def unify(a: IntervalNode, b: IntervalNode, offset: Int = 0): Unit = {
    Logger.debug(s"unifying ${b.id} with ${a.id} at offset ${offset}")
    solver.unify(a.term, b.term, offset)
  }
}

class IntervalNode(
  val graph: IntervalGraph,
  var bases: mutable.Map[SymBase, Int] = mutable.Map.empty,
  val size: Option[Int] = None,
  val id: Int = intervalNodeCounter.next().toInt
) {
  Logger.debug(s"created node with id $id")

  val term: NodeTerm = NodeTerm(this)
  val children = mutable.Set[Int]()

  val flags: DSFlag = DSFlag()
  protected var _cells: Seq[IntervalCell] = Seq.empty
  def cells: Seq[IntervalCell] = _cells
  protected var _collapsed: Option[IntervalCell] = None
  def collapsed: Option[IntervalCell] = _collapsed

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval).sorted
      !intervals.exists(interval1 =>
        intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2))
      )
  }

  def isCollapsed: Boolean = collapsed.nonEmpty
  def add(offset: Int): IntervalCell = {
    add(Interval(offset, offset))
  }

  def get(offset: Int): IntervalCell = {
    get(Interval(offset, offset))
  }

  /**
   * clones this node into the another newGraph
   * clone pointee nodes if recurse
   * oldToNew set of already cloned nodes don't clone if the node is a key in this map
   * will clone pointees if recurse is set and pointee nodes aren't in oldToNew
   * returns the cloned version of this node in the newGraph
   */
  def clone(
    newGraph: IntervalGraph,
    recurse: Boolean = false,
    oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map()
  ): IntervalNode = {
    if recurse then assert(isUptoDate)
    val node = this
    val newNode =
      if !oldToNew.contains(node) then
        val v = newGraph.init(node.bases, node.size)
        v.bases = node.bases
        v.flags.join(node.flags)
        node.cells.foreach(cell => v.add(cell.interval))
        if node.isCollapsed then v._collapsed = Some(v.get(0))
        oldToNew.update(node, v)
        v
      else oldToNew(node)

    if recurse then
      val queue = mutable.Queue[IntervalNode](node)
      while queue.nonEmpty do
        val old = queue.dequeue()
        assert(old.isUptoDate)
        assert(oldToNew.contains(old))
        old.cells.foreach {
          case cell: IntervalCell if cell.hasPointee =>
            val (newNode, off) = newGraph.findNode(oldToNew(old))
            assert(
              newNode.isCollapsed || newNode.cells.exists(c => c.interval.contains(cell.interval.move(i => i + off))),
              s"expected cloned cell to include same intervals"
            )
            assert(newGraph.find(newNode) == newNode)
            val pointee = cell.getPointee
            assert(pointee.node.isUptoDate, s"expected updated pointee")
            if !oldToNew.contains(pointee.node) then queue.enqueue(pointee.node)
            val (clonedPointee, pointeeOff) =
              if !oldToNew.contains(pointee.node) then (pointee.node.clone(newGraph, false, oldToNew), 0)
              else newGraph.findNode(oldToNew(pointee.node))

            assert(newGraph.find(clonedPointee) == clonedPointee, s"expected cloned pointee to remain uptodate")
            val newPointee = clonedPointee.add(pointee.interval.move(i => i + pointeeOff))
            assert(
              pointee.interval.move(i => i + pointeeOff) == newPointee.interval || newPointee.node.isCollapsed,
              s"pointee interval: ${pointee.interval}, moved by $pointeeOff, cloned Pointee interval: ${newPointee.interval}"
            )
            val pointer = newNode.get(cell.interval.move(i => i + off))
            assert(pointer.node.isCollapsed || pointer.interval.contains(cell.interval.move(i => i + off)))
            assert(!pointer.hasPointee || pointer.getPointee == newPointee)
            if !pointer.hasPointee then pointer.setPointee(newPointee)
          case _ =>
        }

    newNode
  }

  override def hashCode(): Int = id
  override def toString: String =
    s"Node($id, ${bases.keys}, ${if isCollapsed then "C" else cells.map(_.interval).sorted})"

  def add(cell: IntervalCell): Unit = {
    require(cell.node == this, "added cell must have a reference to this node")
    _cells = _cells.appended(cell).sorted
    assert(nonOverlappingProperty, "expected non overlapping cells")
  }

  /**
   * Checks this node is set representative in `this.graph` union-find solver
   * If true the node is the most "up-to-date" version of itself. meaning it hasn't been
   *  involved in any merges which will create a new version
   *
   * New nodes could be created invalidating this check if a node is involved in
   * any merge or collapse operations.
   */
  def isUptoDate: Boolean = {
    graph.find(this) == this
  }

  def init(interval: Interval): IntervalCell = IntervalCell(this, interval)

  override def equals(obj: Any): Boolean = {
    obj match
      case node: IntervalNode => id == node.id
      case _ => false
  }

  def collapse(): IntervalCell = {
    assert(isUptoDate)
    val node = this
    if (!(node.isCollapsed)) {
      val oldPointees = cells.filter(_.hasPointee).map(_.getPointee)
      val collapseNode: IntervalNode = IntervalNode(graph, bases, size)
      collapseNode.children.addAll(this.children)
      collapseNode.children.add(this.id)
      collapseNode.flags.join(this.flags)
      var collapsedCell: IntervalCell = collapseNode.add(0)
      collapseNode._collapsed = Some(collapsedCell)
      // delay unification
      // treat collapsed node completely distinct to current node
      if node.cells.exists(_.hasPointee) then
        val pointToItself = node.cells.filter(_.hasPointee).map(_.getPointee).exists(c => c.node == node)
        var pointees = node.cells.filter(_.hasPointee).map(_.getPointee).filter(_.node != node)
        val pointeeNodes = pointees.map(_.node).toSet
        // if a collapsed node points to itself collapse all other pointees first
        if pointToItself then
          pointees.map(f =>
            // break all points-to relationship from pointees to this or other pointees
            // all pointees are collapsed and merged together
            graph
              .find(f)
              .node
              .cells
              .filter(_.hasPointee)
              .filter(f => pointeeNodes.map(graph.find).contains(f.getPointee.node))
              .foreach(_.removePointee)
            graph.find(f).node.collapse()
          )
        pointees = pointees.map(graph.find)
        var pointee = if pointees.nonEmpty then graph.mergeCells(pointees) else collapsedCell

        if (pointToItself) {
          assert(!collapsedCell.hasPointee)
          val oldPointee = pointee.node
          assert(collapseNode.isUptoDate)
          assert(pointee.node.isCollapsed)
          pointee = graph.mergeCells(pointee, collapsedCell)
          assert(pointee.node.isCollapsed)
          Logger.debug(pointee)
          collapsedCell = pointee
        }

        if !pointToItself then assert(!collapsedCell.hasPointee)
//        DSALogger.warn(pointToItself)
        collapsedCell.setPointee(pointee)
        collapsedCell = graph.find(collapsedCell)
        if pointToItself then assert(collapsedCell.getPointee == collapsedCell)
        assert(collapsedCell.hasPointee)
        assert(collapsedCell.getPointee == graph.find(pointee))
        assert(collapsedCell.node.cells.size == 1)

      // unify collapsed node and current node
      graph.unify(node, collapsedCell.node)
      cells.foreach(f => assert(graph.find(f) == collapsedCell))
      if node.cells.exists(_.hasPointee) then assert(graph.find(collapsedCell).hasPointee)
      assert(oldPointees.map(graph.find).forall(f => f.equiv(graph.find(collapsedCell).getPointee)))
      graph.find(collapsedCell)

    } else {
      graph.find(node.collapsed.get)
    }

  }

  /**
   * if the nodes is collapsed return collapsed node else
   * Adds a cell with this interval if it doesn't exists already
   * if a cell with the exact interval exists return it
   * if a cell exists which contains this interval return a dummy cell with the exact intervals provided
   * otherwise unify all the cells with overlapping intervals and return a dummy cell with the provided interval
   * @param interval interval of the newCell
   * @return a cell with this node and the provided interval
   */
  def add(interval: Interval): IntervalCell = {
    assert(isUptoDate)
    if !isCollapsed then
      val overlapping: Seq[IntervalCell] = cells.filter(_.interval.isOverlapping(interval))
      val newCell = if overlapping.isEmpty then
        val res = init(interval)
        _cells = _cells.appended(res)
        res
      else if overlapping.size == 1 && overlapping.head.interval == (interval) then this.get(interval)
      else if overlapping.size == 1 && overlapping.head.interval.contains(interval) then init(interval)
      else
        val unifiedInterval = overlapping.map(_.interval).fold(interval)(Interval.join)
        val res = init(unifiedInterval)
        val pointees = overlapping.filter(_.hasPointee).map(_.getPointee)

        val pointee = if pointees.nonEmpty then Some(graph.mergeCells(pointees)) else None
        _cells = cells.diff(overlapping).appended(res).sorted
        if pointees.nonEmpty then graph.find(res).setPointee(pointee.get)
        init(interval)

      assert(nonOverlappingProperty, "expected non overlapping cells")
      newCell
    else collapsed.get
  }

  // get the cell which contains this interval in the node
  // expects exactly 1 corresponding cell since they are non-overlapping
  def get(interval: Interval): IntervalCell = {
    if isCollapsed then collapsed.get
    else
      val exactMatches = cells.filter(_.interval.contains(interval))
      assert(
        exactMatches.size == 1,
        s"Expected exactly one overlapping interval instead got ${exactMatches.size}, with ${cells.map(_.interval)}"
      )
      exactMatches.head
  }
}

object IntervalCell {
  implicit def orderingByInterval[T <: IntervalCell]: Ordering[T] =
    Ordering.by(sc => sc.interval)
}

/**
 * A data structure cell
 * @param node the node this cell belongs to
 * @param interval the interval of the cell
 *
 * Additionally may have a pointee only cells in the node have their pointees updated
 * All pointee operations (set, remove, hasPointee) on
 * dummy cells (old slices) are treated as operations on their corresponding cell in the node
 *
 * the reason is that a node cell may not have pointee when dummy cell corresponding to it is created
 * if that cell is later assigned a pointee, avoid the need for update to all dummy cells
 */
class IntervalCell(val node: IntervalNode, val interval: Interval) {
  var _pointee: Option[IntervalCell] = None
  private val graph: IntervalGraph = node.graph

  override def toString: String = s"Cell($node, $interval)"

  def moved(f: Int => Int): IntervalCell = {
    val newCell = IntervalCell(node, interval.move(f))
    newCell._pointee = _pointee
    newCell
  }

  def grown(interval: Interval): IntervalCell = {
    require(this.interval.start == interval.start, "expected same interval start for growing cell")
    val newCell = IntervalCell(this.node, this.interval.join(interval))
    newCell._pointee = _pointee
    newCell
  }

  override def equals(obj: Any): Boolean = {
    obj match
      case other: IntervalCell => other.node == this.node && other.interval == this.interval
      case _ => false
  }

  override def hashCode(): Int = {
    node.hashCode() * 23 + interval.hashCode() * 31
  }

  // this checks if two cells correspond to the same unified cell in their node
  // weaker equals
  def equiv(other: IntervalCell): Boolean = {
    graph.get(this.node.get(this.interval)).equals(graph.get(other.node.get(other.interval)))
  }

  def removePointee: Option[IntervalCell] = {
    if node.get(this.interval) ne this then node.get(this.interval).removePointee
    else
      val temp = _pointee.map(graph.find)
      _pointee = None
      temp
  }

  /**
   * returns this cell's pointee if it has one
   * if the cell doesn't have a pointee create a placeholder cell,
   * sets it as the cell pointee and return it
   *
   * Can check if a cell has pointee without creating one for it with hasPointee
   */
  def getPointee: IntervalCell = {
    if node.get(this.interval) ne this then node.get(this.interval).getPointee
    else if _pointee.isEmpty then
//      throw Exception("expected a pointee")
      assert(this.node.isUptoDate)
      _pointee = Some(IntervalNode(graph, mutable.Map.empty).add(0))
      _pointee.get
    else graph.find(_pointee.get)
  }

  def hasPointee: Boolean = node.get(this.interval)._pointee.nonEmpty

  def setPointee(cell: IntervalCell): IntervalCell = {
    assert(this.node.isUptoDate)
    assert(cell.node.isUptoDate)
    if node.get(this.interval) ne this then node.get(this.interval).setPointee(cell)
    else if _pointee.isEmpty then
      assert(node.cells.contains(this))
      _pointee = Some(cell)
      _pointee.get
    else if cell.equiv(this) then
      assert(node.cells.contains(this))
      val pointee = this.removePointee.get
      val newThis = graph.mergeCells(pointee, this)
      graph.mergePointees(newThis, newThis.getPointee)
      this._pointee = Some(graph.mergeCells(newThis, newThis.getPointee))
      assert(this._pointee.get.equiv(pointee))
      assert(this._pointee.get.equiv(this))
      _pointee.get
    else // if a cell points to itself break the link,
      assert(node.cells.contains(this))
      graph.mergePointees(this.getPointee, cell)
      this._pointee = Some(graph.mergeCells(cell, graph.get(this).getPointee))
      graph.get(this)._pointee = this._pointee
      _pointee.get
  }
}

object IntervalDSA {

  /**
   *  checks that each region only belongs to a single node
   *  holds after local phase
   *
   *  potentially should be maintained through BU and TD phases
   */
  def checkUniqueNodesPerRegion(graph: IntervalGraph): Unit = {
    val found = mutable.Map[SymBase, IntervalNode]()
    val seen = mutable.Set[IntervalNode]()
    val queue = mutable.Queue[IntervalNode]().enqueueAll(graph.nodes.values.map(graph.find))
    while queue.nonEmpty do {
      val node = queue.dequeue()
      node.bases.keys.foreach(base =>
        assert(!found.contains(base) || found(base) == node, s"$base was in $node and ${found(base)}")
      )
      assert(node.bases.keys.forall(base => !found.contains(base) || found(base) == node))
      node.bases.keys.foreach(found.update(_, node))
      seen.add(node)
      val toDo = node.cells.filter(_.hasPointee).map(_.getPointee).map(_.node).filterNot(seen.contains)
      queue.enqueueAll(toDo)
    }
  }

  /**
   *  checks that all reachable memory load and stores in DSA's domain have a dsa node
   *  which corresponds to their index expr
   */
  def checkReachable(program: Program, DSA: Map[Procedure, IntervalGraph]): Unit = {
    val reachable = computeDomain(IntraProcIRCursor, program.procedures)
    for (pos <- reachable) {
      val proc = IRWalk.procedure(pos)
      if DSA.contains(proc) then
        val dsg = DSA(proc)
        pos match
          case load: MemoryLoad =>
            assert(dsg.exprToCells(load.index).nonEmpty)
          case store: MemoryStore =>
            assert(dsg.exprToCells(store.index).nonEmpty)
          case _ =>
    }
  }

  /**
   * Checks that the same regions are unified with global across all procedures
   * that is if (A and Global) are unified at offset C in one procedure they are unified at offset C
   * across all procedures
   * Should hold at the end of DSA
   */
  def checkConsistGlobals(DSA: Map[Procedure, IntervalGraph], global: IntervalGraph): Unit = {
    // collect all the regions  from all the resulting graphs
    val unifiedRegions = global.nodes(Global).bases
    DSA
      .filterNot((proc, _) => proc.procName == "indirect_call_launchpad")
      .foreach((p, graph) =>
        val graphRegions = graph.nodes(Global).bases
        assert(
          unifiedRegions == graphRegions,
          s"Procedure ${p.procName} had a differing unified global sets than compared to the global graph"
        )
      )
  }


  def getLocal(
    proc: Procedure,
    context: IRContext,
    symValues: SymValues[Interval],
    cons: Set[Constraint]
  ): IntervalGraph = {
    val graph = IntervalGraph(proc, Local, context, symValues, cons, None)
    graph.localPhase()
    graph.localCorrectness()
    graph.clone
  }

  def getLocals(
    ctx: IRContext,
    svas: Map[Procedure, SymValues[Interval]],
    cons: Map[Procedure, Set[Constraint]]
  ): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing local DSA")
    computeDSADomain(ctx.program.mainProcedure, ctx).toSeq
      .sortBy(_.name)
      .foldLeft(Map[Procedure, IntervalGraph]())((m, proc) =>
        m + (proc -> IntervalDSA.getLocal(proc, ctx, svas(proc), cons(proc)))
      )
  }

  def solveBUs(locals: Map[Procedure, IntervalGraph]): Map[Procedure, IntervalGraph] = {

    DSALogger.info("Performing DSA BU phase")
    val bus = locals.view.mapValues(_.clone).toMap
    bus.values.foreach(_.localCorrectness())
    DSALogger.info("performed cloning")
    val visited: mutable.Set[Procedure] = mutable.Set.empty
    val queue = mutable.Queue[Procedure]().enqueueAll(bus.keys.toSeq.sortBy(p => p.name))

    // TODO instead of skipping merge the scc and use it directly
    var skip = Seq("croak", "myexit")
    while queue.nonEmpty do
      val proc = queue.dequeue()
      if skip.exists(name => proc.name.startsWith(name)) then
        DSALogger.info(s"skipped ${proc.name} due to scc")
        visited += proc
      else if !proc.calls.filter(proc => !proc.isExternal.getOrElse(false)).forall(visited.contains) then
        DSALogger.info(s"BU procedure ${proc.name} was readded")
        queue.enqueue(proc)
      else
        DSALogger.info(s"performing BU for ${proc.name}")
        bus(proc).contextTransfer(BU, bus)
        visited += proc
    bus.view.mapValues(_.clone).toMap
  }

  def solveTDs(bus: Map[Procedure, IntervalGraph]): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing DSA TD phase")
    val tds = bus.view.mapValues(_.clone).toMap
    val visited: mutable.Set[Procedure] = mutable.Set.empty
    val queue = mutable.Queue[Procedure]().enqueueAll(tds.keys.toSeq.sortBy(p => p.name))

    // TODO instead of skipping merge the scc and use it directly
    var skip = Seq("croak", "myexit")
    while queue.nonEmpty do
      val proc = queue.dequeue()
      if skip.exists(name => proc.name.startsWith(name)) then
        DSALogger.info(s"skipped ${proc.name} due to scc")
        visited += proc
      else if !proc.callers().filter(f => tds.keySet.contains(f)).forall(f => visited.contains(f)) then
        DSALogger.info(s"TD procedure ${proc.name} was readded")
        queue.enqueue(proc)
      else
        DSALogger.info(s"performing TD for ${proc.name}")
        tds(proc).contextTransfer(TD, tds)
        visited += proc

    tds.view.mapValues(_.clone).toMap
  }
}
