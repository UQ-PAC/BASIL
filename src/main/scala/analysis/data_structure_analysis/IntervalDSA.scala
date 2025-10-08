package analysis.data_structure_analysis

import analysis.data_structure_analysis.IntervalDSA.checkUniqueGlobals
import analysis.data_structure_analysis.OSet.Top
import analysis.solvers.OffsetUnionFindSolver
import boogie.SpecGlobal
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.*
import ir.eval.BitVectorEval.isNegative
import specification.{ExternalFunction, FuncEntry, SymbolTableEntry}
import util.DSAPhase.*
import util.LogLevel.INFO
import util.assertion.*
import util.{DSALogger, DSAPhase, DSConfig, PerformanceTimer}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private val intervalNodeCounter = util.Counter()

case class NodeTerm(v: IntervalNode) extends analysis.solvers.Var[NodeTerm]

/**
 * Data Structure Graph
 */
class IntervalGraph(
  val proc: Procedure,
  var phase: DSAPhase,
  val irContext: IRContext,
  var sva: SymValues[OSet],
  val constraints: Set[Constraint],
  val glIntervals: Seq[DSInterval],
  val eqCells: Boolean,
  val nodeBuilder: Option[() => Map[SymBase, IntervalNode]]
) {

  val solver = OffsetUnionFindSolver[NodeTerm]()
  val builder: () => Map[SymBase, IntervalNode] = nodeBuilder.getOrElse(buildNodes)
  var nodes: Map[SymBase, IntervalNode] = builder()

  val isCalledBySCC = calledBySCC(proc)

  def calledBySCC(p: Procedure): Boolean = {
    proc.scc.isDefined || {
      CallGraph.pred(p).exists(calledBySCC)
    }
  }

  def exprToSymVal(expr: Expr): SymValSet[OSet] =
    SymValues.exprToSymValSet(sva, i => isGlobal(i, irContext), glIntervals)(expr)

  protected def symValToNodes(
    symVal: SymValSet[OSet],
    current: Map[SymBase, IntervalNode]
  ): Map[SymBase, IntervalNode] = {
    symVal.state.filter((base, _) => base != Constant).foldLeft(current) { case (result, (base, symOffsets)) =>
      val node = find(result.getOrElse(base, init(base, None)))
      base match
        case Heap(call) => node.flags.heap = true
        case Stack(proc) => node.flags.stack = true
        case GlobSym(interval) => node.flags.global = true
        case Constant =>
        case unknown: (Ret | Par | Loaded) =>
          node.flags.unknown = true
          node.flags.incomplete = true
      result + (base -> node)
    }
  }

  def addGlobal(globals: Map[SymBase, IntervalNode], address: Int, size: Int): IntervalCell = {
    val (interval, offset) = getGlobal(glIntervals, address).get
    val base = GlobSym(interval)
    val node = globals(base)
    node.flags.global = true
    node.add(DSInterval(offset, offset + size))
  }

  def buildGlobals(
    globals: Set[SymbolTableEntry],
    globalOffsets: Map[BigInt, BigInt],
    externalFunctions: Set[ExternalFunction]
  ): Map[SymBase, IntervalNode] = {
    val globalNodes = glIntervals.foldLeft(Map[SymBase, IntervalNode]()) { (m, interval) =>
      val base = GlobSym(interval)
      val node = IntervalNode(this, Map(base -> Set(0)))
      m + (base -> node)
    }

    globals.toSeq.sortBy(_.address).foreach {
      case FuncEntry(name, size, address) =>
        addGlobal(globalNodes, address.toInt, size / 8).node.flags.function = true
      case SpecGlobal(name, size, arraySize, address) =>
        addGlobal(globalNodes, address.toInt, 0)
    }

    externalFunctions.foreach(e =>
      val node = addGlobal(globalNodes, e.offset.toInt, 0).node
      node.flags.function = true
      node.flags.foreign = true
    )

    globalOffsets.foreach { case (address, relocated) =>
      val pointer = addGlobal(globalNodes, address.toInt, 8)
      val pointee = addGlobal(globalNodes, relocated.toInt, 0)
      pointer.setPointee(pointee)
    }

    globalNodes
  }

  def buildNodes(): Map[SymBase, IntervalNode] = {
    val globals =
      buildGlobals(irContext.globals ++ irContext.funcEntries, irContext.globalOffsets, irContext.externalFunctions)
    sva.state.foldLeft(globals) { case (m, (variable, valueSet)) =>
      symValToNodes(valueSet, m)
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
    /*val symBase = Global
    val globalNode = nodes(Global)
    val actual = cell.node.get(cell.interval)
    irContext.funcEntries.foldLeft(Set[Procedure]()) { (s, funEntry) =>
      if actual == get(globalNode.get(funEntry.address.toInt)) then
        s + irContext.program.procedures.filter(p => p.procName == funEntry.name).head
      else s
    }*/
    Set.empty
  }

  def addParamCells(): Unit = {
    val unchanged = Set("R29", "R30", "R31")
    (proc.formalInParam ++ proc.formalOutParam)
      .foreach(v =>
        val cells = exprToCells(v)
        if unchanged.forall(n => !v.name.startsWith(n)) then cells.map(_.node).foreach(_.flags.escapes = true)
      )
  }

  // Processes all non call constraints
  def localPhase(): Unit = {
    addParamCells()
    constraints.foreach(processConstraint)
    if eqCells then constraints.foreach(widenEqClasses)
  }

  // returns the cells corresponding to the
  def symValToCells(symVal: SymValSet[OSet], newEqv: Boolean = false): Set[IntervalCell] = {
    val pairs = symVal.state.filter((base, _) => base != Constant)
    pairs.foldLeft(Set[IntervalCell]()) { case (results, (base: SymBase, offsets: OSet)) =>
      val (node, adjustment) = findNode(nodes(base))
      if offsets == Top then results + node.collapse()
      else {
        val current = offsets.toIntervals
          .filter(i =>
            !base.isInstanceOf[GlobSym] ||
              isGlobal(i.start.get + base.asInstanceOf[GlobSym].interval.start.get, irContext)
          )
          .map(_.move(i => i + adjustment))
          .map(node.add)
        val eq =
          if base.isInstanceOf[GlobSym] || !newEqv || !eqCells then Set.empty
          else
            offsets.toOffsets.flatMap(o =>
              cellToEq(node.get(adjustment)).map(_.interval.move(i => i + o)).map(node.add)
            )
        results ++ current ++ eq
      }
    }
  }

  def contextTransfer(phase: DSAPhase, graphs: Map[Procedure, IntervalGraph]): Unit = {
    require(phase == TD || phase == BU)
    this.phase = phase
    constraints.toSeq.sortBy(c => c.label).foreach {
      case dcc: DirectCallConstraint if graphs.contains(dcc.target) && !dcc.target.isExternal.getOrElse(false) =>
        val (source, target) = if phase == TD then (this, graphs(dcc.target)) else (graphs(dcc.target), this)
        IntervalDSA.callTransfer(phase, dcc, source, target)
      case dcc: DirectCallConstraint if dcc.target.name == "indirect_call_launchpad" =>
        resolveIndirectCall(dcc)
          .filterNot(proc =>
            dcc.call.parent.parent == proc || proc.isExternal.getOrElse(false) || proc.name.startsWith("_")
          )
          .foreach(proc =>

            val (source, target) = if phase == TD then (this, graphs(proc)) else (graphs(proc), this)
            IntervalDSA.callTransfer(phase, dcc, source, target)
          )
      case _ =>
    }
  }

  private def isIndirectCall(dcc: DirectCallConstraint): Boolean = {
    dcc.target.name == "indirect_call_launchpad"
  }

  def cellToEq(cell: IntervalCell): Set[IntervalCell] = {
    cell.node.eqClasses.collectFirst { case eqc if eqc.contains(cell) => eqc } match {
      case Some(value) => value
      case None => Set(cell)
    }
  }

  // find the corresponding cells for a expr from this graph's procedure
  def exprToCells(expr: Expr, newEqv: Boolean = false): Set[IntervalCell] = {
    symValToCells(exprToSymVal(expr), newEqv).map(find).flatMap(cellToEq)
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
        debugAssert(IntervalDSA.equiv(valueCells), s"value cells should be unified instead got $valueCells")

        val indexCells = constraintArgToCells(constraint.arg1, ignoreContents = true).map(get)
        if indexCells.nonEmpty then
          if indexCells.nonEmpty && valueCells.nonEmpty then
            indexCells.foreach(indexCell =>
              valueCells.foreach(valueCell =>
                debugAssert(indexCell.node.isUptoDate, "outdated cell in local correctness check")
                debugAssert(indexCell.getPointee.node.isUptoDate, "outdated cell in local correctness check")
                debugAssert(valueCell.node.isUptoDate, "outdated cell in local correctness check")
                debugAssert(
                  indexCell.hasPointee && indexCell.getPointee.equiv(valueCell),
                  s"$constraint, $indexCell doesn't point to ${valueCell} instead ${indexCell.getPointee}"
                )
              )
            )
      case _ =>
    }
  }

  /**
   * Clones a graph
   * all middle nodes in the union-find are lost
   * @return cloned graph
   */
  override def clone: IntervalGraph = {
    val oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map()
    val copy = IntervalGraph(
      proc,
      phase,
      irContext,
      sva,
      constraints,
      glIntervals,
      eqCells,
      Some(() => Map[SymBase, IntervalNode]())
    )
    this.nodes.foreach { // in addition to current nodes
      case (base, node) => // clone old nodes in base to node map to carry offset info
        val (current, offset) = this.findNode(node)
        if current != node then
          val oldCopy = node.clone(copy, false, oldToNew)
          val curCopy = current.clone(copy, true, oldToNew)
          copy.unify(oldCopy, curCopy, offset)
        else current.clone(copy, true, oldToNew)
    }

    debugAssert(
      copy.solver.size <= this.solver.size,
      s"size of copy's solver ${copy.solver.size}," +
        s"size of this's solver ${this.solver.size}"
    )
    copy.nodes = this.nodes.view.mapValues(oldToNew.apply).toMap
    debugAssert(copy.nodes.keys == this.nodes.keys)
    copy
  }

  def toDot: String = {

    val (nodes, pointsTo) = collect()
    val toRemove = Set('$', '#', '%')

    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    nodes.foreach { n =>
      val eqClasses =
        if n.eqClasses.nonEmpty then s"\\nEq: ${n.eqClasses.map(_.map(_.interval)).mkString("\\n")}" else ""
      val bases = s"\\n${n.bases.mkString("\\n").replace("->", ":")}"
      structs.append(
        DotStruct(
          n.id.toString,
          s"Node ${n.id}$eqClasses$bases",
          Some(n.cells.map(o => s"<${o.interval.start.getOrElse(0)}> ${o.interval}")),
          true
        )
      )
    }

    pointsTo.foreach { (pointer, pointee) =>
      val pointerID = pointer.node.id.toString
      val pointerOffset = pointer.interval.start.getOrElse(0).toString
      arrows.append(
        StructArrow(
          DotStructElement(pointerID, Some(pointerOffset)),
          DotStructElement(pointee.node.id.toString, Some(pointee.interval.start.getOrElse(0).toString))
        )
      )
    }
    StructDotGraph(proc.name, structs, arrows).toDotString
  }

  def collect(): (Set[IntervalNode], Set[(IntervalCell, IntervalCell)]) = {
    val nodes: mutable.Set[IntervalNode] = mutable.Set(this.nodes.values.map(find).toSeq: _*)
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

  def init(symBase: SymBase, size: Option[Int]): IntervalNode = IntervalNode(this, Map(symBase -> Set(0)), size)
  def constraintArgToCells(
    constraintArg: ConstraintArg,
    newEqv: Boolean = false,
    ignoreContents: Boolean = false
  ): Set[IntervalCell] = {
    val cells = exprToCells(constraintArg.value, newEqv)
    val exprCells = cells.map(find)

    if constraintArg.contents && !ignoreContents then exprCells.map(_.getPointee)
    else exprCells

  }

  def markEscapes(
    constraint: MemoryAccessConstraint[_],
    indices: Set[IntervalCell],
    pointees: Set[IntervalCell]
  ): Unit = {
    val indexFlag = joinFlags(indices)
    constraint match
      case MemoryReadConstraint(pos) =>
        indices.map(_.node).foreach(_.flags.read = true)
        if indexFlag.heap || indexFlag.escapes then pointees.map(_.node).foreach(_.flags.escapes = true)
      case MemoryWriteConstraint(pos) =>
        indices.map(_.node).foreach(_.flags.modified = true)
        if indexFlag.heap || indexFlag.escapes then pointees.map(_.node).foreach(_.flags.escapes = true)
  }

  def processConstraint(constraint: Constraint): Unit = {
    constraint match
      case cons: MemoryAccessConstraint[_] =>
        DSALogger.debug(s"Processing constraint $cons")
        val indices = constraintArgToCells(cons.arg1, true, ignoreContents = true)
        indices.foreach(cell => cell.node.add(cell.interval.growTo(cons.size)))
        val pointees = constraintArgToCells(cons.arg1, true)
        markEscapes(cons, indices, pointees)
        val values = constraintArgToCells(cons.arg2, true)
        if pointees.nonEmpty || values.nonEmpty then mergeCells(pointees ++ values)
        else DSALogger.warn(s"$cons had an empty argument")
        (indices ++ values).map(_.node).map(find).filterNot(_.eqClassProperty()).toSet.foreach(_.maintainEqClasses())
      case _ => // ignore
  }

  /**
   * widens any eq classes related to this constraint that aren't fixed
   * the constraint must have been processed using processConstraint
   */
  def widenEqClasses(con: Constraint): Unit = {
    con match
      // only concerned with constraints occuring in a cycle
      case cons: MemoryAccessConstraint[_] =>
        var pointees = constraintArgToCells(cons.arg1)
        var values = constraintArgToCells(cons.arg2)
        if pointees.nonEmpty || values.nonEmpty then {
          assert(IntervalDSA.equiv(pointees ++ values))
          val eqs = cellToEq((pointees ++ values).head) // compute eq cells involved in constraint
          pointees = constraintArgToCells(cons.arg1, true)
          values = constraintArgToCells(cons.arg2, true)
          val newEqs = cellToEq(mergeCells(pointees ++ values))
          // if reprocessing added another cell collapse the node
          if newEqs.size > eqs.size && (cons.source.parent.isLoopParticipant() || isCalledBySCC) then
            find(eqs.head).node.collapse()
        }
      case _ =>
  }

  /**
   * if the cells is not in a scc returns an empty seq
   * else return a seq of cells corresponding to a circle of cells pointing to each other
   */
  def isInSCC(cell: IntervalCell): Seq[IntervalCell] = {
    var seen = Seq[IntervalCell]()
    var cur = cell
    while cur.hasPointee && !seen.contains(cell) && !seen.contains(cur) do {
      cur = get(cur.getPointee)
      seen = seen.appended(cur)
    }
    if seen.contains(cell) then seen else Seq.empty
  }

  def disconnectSCC(scc: Seq[IntervalCell]): Unit = {
    scc.foreach(_.removePointee)
  }

  def connectSCC(scc: Seq[IntervalCell]): Unit = {
    if scc.nonEmpty then
      val head = scc.head
      var cur = head
      var tail = scc.tail
      while tail.nonEmpty do {
        find(cur).setPointee(find(tail.head))
        cur = tail.head
        tail = tail.tail
      }
      get(scc.last).setPointee(get(head))
  }

  def getSelfEdges(node: IntervalNode): Map[IntervalCell, IntervalCell] = {
    node.cells.filter(_.hasPointee).filter(_.getPointee.node == node).map(c => (c, c.getPointee)).toMap
  }

  /**
   * disconnect edges from a node to itself (could be different cells in the same node)
   */
  def disconnectSelfEdges(node: IntervalNode): Map[IntervalCell, IntervalCell] = {
    val selfEdges = getSelfEdges(node)
    selfEdges.keys.foreach(_.removePointee)
    selfEdges
  }

  def getOutEdges(node: IntervalNode): Map[IntervalCell, IntervalCell] = {
    node.cells.filter(_.hasPointee).map(c => (c, c.getPointee)).toMap
  }

  def checkEdgesAreMaintained(edges: Map[IntervalCell, IntervalCell]): Unit = {
    edges.foreach((pointer, pointee) =>
      debugAssert(pointee.equiv(get(pointer).getPointee), s"$pointer doesn't point to updated version of its pointee")
    )
  }

  protected def mergeCellsHelper(cell1: IntervalCell, cell2: IntervalCell): IntervalCell = {
    debugAssert(cell1.node.isUptoDate)
    debugAssert(cell2.node.isUptoDate)

    // determine which node is to be moved and by how much
    val (stableCell, toBeMoved) =
      if cell1.node.isCollapsed then (cell1, cell2)
      else if cell2.node.isCollapsed then (cell2, cell1)
      else if cell1.interval.start.get > cell2.interval.start.get then (cell1, cell2)
      else (cell2, cell1)
    val delta: Option[Int] =
      if !stableCell.node.isCollapsed then Some(stableCell.interval.start.get - toBeMoved.interval.start.get) else None

    val stableNode = stableCell.node
    val nodeToBeMoved = toBeMoved.node

    // bookkeeping
    if delta.nonEmpty then
      stableNode.eqClasses ++= nodeToBeMoved.eqClasses.map(eq =>
        eq.map(cell => stableNode.add(cell.interval.move(i => i + delta.get)))
      )
    stableNode.flags.join(nodeToBeMoved.flags)
    stableNode.bases ++= nodeToBeMoved.bases // compute new region alignments
      .map((base, set) =>
        (
          base,
          stableNode.bases.getOrElse(base, Set.empty) ++ // don't overwrite regions common with stableNode
            set.map(_ + delta.getOrElse(0)) // move regions in notToBeMoved by delta
        )
      )

    // start unification
    val scc1 = isInSCC(cell1)
    disconnectSCC(scc1)
    val scc2 = isInSCC(cell2)
    disconnectSCC(scc2)
    val selfEdges = disconnectSelfEdges(stableNode)

    // unify so that find(nodeToBeMoved, Interval) will give (stableNode, Interval + delta)
    unify(nodeToBeMoved, stableNode, delta.getOrElse(0))

    nodeToBeMoved.cells.foreach(c =>
      val (node, offset) = findNode(stableNode)
      val cell = node.add(c.interval.move(i => i + offset + delta.getOrElse(0)))
      if c.hasPointee then cell.setPointee(c.getPointee)
    )

    selfEdges.foreach((pointer, pointee) => find(pointer).setPointee(find(pointee)))
    connectSCC(scc1)
    connectSCC(scc2)

    find(stableNode).maintainEqClasses()
    find(stableCell)
  }

  def mergeCells(c1: IntervalCell, c2: IntervalCell): IntervalCell = {

    require(c1.node.graph == c2.node.graph && c1.node.graph == this)
    val cell1 = find(c1)
    val cell2 = find(c2)

    val node1Pointees = getOutEdges(cell1.node)
    val node2Pointees = getOutEdges(cell2.node)

    val result =
      if get(cell1) == get(cell2) then cell1
      else if cell1.node.equals(cell2.node) then {
        if eqCells then {
          cell1.node.eqClasses += Set(cell1, cell2)
          cell1.node.maintainEqClasses()
          assert(find(cell1).node.eqClassProperty())
          find(cell1)
        } else cell1.node.collapse()
      } else if cell1.node.isCollapsed || cell2.node.isCollapsed then {
        cell1.node.collapse()
        find(cell2).node.collapse()
        mergeCellsHelper(find(cell1), find(cell2))
      } else mergeCellsHelper(cell1, cell2)

    debugAssert(result.equiv(get(cell1)))
    debugAssert(result.equiv(get(cell2)))
    checkEdgesAreMaintained(node1Pointees)
    checkEdgesAreMaintained(node2Pointees)

    debugAssert(result.node.isUptoDate)
    result
  }

  def mergeCells(cells: Iterable[IntervalCell]): IntervalCell = {
    require(cells.nonEmpty, "can't merge empty set of cells")
    cells.tail.foldLeft(cells.head) { (result, cell) =>
      mergeCells(result, cell)
    }
  }

  def findExact(cell: IntervalCell): (IntervalNode, DSInterval) = {
    val node = cell.node
    val (newNode, offset) = findNode(node)
    (newNode, cell.interval.move(i => i + offset))
  }

  def findNode(node: IntervalNode): (IntervalNode, Int) = {
    val (term, offset) = solver.findWithOffset(node.term)
    debugAssert(solver.findWithOffset(term)._1 == term, "find doesnt' get the most up-to-date node")
    (term.asInstanceOf[NodeTerm].v, offset)
  }

  def find(node: IntervalNode): IntervalNode = {
    findNode(node)._1
  }

  def find(base: SymBase): IntervalNode = {
    require(nodes.contains(base))
    find(nodes(base))
  }

  // find the most uptodate version of cell
  // if the cell was unified changing it's size
  // creates a dummy cell with cell's interval size
  def find(cell: IntervalCell): IntervalCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.add(newInterval)
    debugAssert(findNode(res.node)._1 == res.node)
    res
  }

  // find the most uptodate version of cell
  // if cell was unified with others, returns a cell with
  // unified interval
  def get(cell: IntervalCell): IntervalCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.get(newInterval)
    debugAssert(findNode(res.node)._1 == res.node)
    res
  }

  def unify(a: IntervalNode, b: IntervalNode, offset: Int = 0): Unit = {
    DSALogger.debug(s"unifying ${b.id} with ${a.id} at offset ${offset}")
    solver.unify(a.term, b.term, offset)
  }
}

class IntervalNode(
  val graph: IntervalGraph,
  var bases: Map[SymBase, Set[Int]] = Map.empty,
  val size: Option[Int] = None,
  val id: Int = intervalNodeCounter.next().toInt
) {
  DSALogger.debug(s"created node with id $id")

  val term: NodeTerm = NodeTerm(this)
  var eqClasses: Set[Set[IntervalCell]] = Set.empty
  val children = mutable.Set[Int]()

  val flags: DSFlag = DSFlag()
  protected var _cells: Seq[IntervalCell] = Seq(IntervalCell(this, DSInterval(0, 0)))
  def cells: Seq[IntervalCell] = _cells

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval).sorted
      !intervals.exists(interval1 =>
        intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2))
      )
  }

  def isCollapsed: Boolean = _cells.nonEmpty && _cells.head.interval == DSInterval.Top
  def add(offset: Int): IntervalCell = {
    add(DSInterval(offset, offset))
  }

  def get(offset: Int): IntervalCell = {
    get(DSInterval(offset, offset))
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
    if recurse then debugAssert(isUptoDate)
    val node = this
    val newNode =
      if !oldToNew.contains(node) then
        val v = IntervalNode(newGraph, node.bases, node.size)
        v.flags.join(node.flags)
        node.cells.foreach(cell => v.add(cell.interval))
        v.eqClasses = node.eqClasses.map(eq => eq.map(cell => IntervalCell(v, cell.interval)))
        oldToNew.update(node, v)
        v
      else oldToNew(node)

    if recurse then
      val queue = mutable.Queue[IntervalNode](node)
      while queue.nonEmpty do
        val old = queue.dequeue()
        debugAssert(old.isUptoDate)
        debugAssert(oldToNew.contains(old))
        old.cells.foreach {
          case cell: IntervalCell if cell.hasPointee =>
            val (newNode, off) = newGraph.findNode(oldToNew(old))
            debugAssert(
              newNode.cells.exists(c => c.interval.contains(cell.interval.move(i => i + off))),
              s"expected cloned cell to include same intervals"
            )
            debugAssert(newGraph.find(newNode) == newNode)
            val pointee = cell.getPointee
            debugAssert(pointee.node.isUptoDate, s"expected updated pointee")
            if !oldToNew.contains(pointee.node) then queue.enqueue(pointee.node)
            val (clonedPointee, pointeeOff) =
              if !oldToNew.contains(pointee.node) then (pointee.node.clone(newGraph, false, oldToNew), 0)
              else newGraph.findNode(oldToNew(pointee.node))

            debugAssert(newGraph.find(clonedPointee) == clonedPointee, s"expected cloned pointee to remain uptodate")
            val newPointee = clonedPointee.add(pointee.interval.move(i => i + pointeeOff))
            debugAssert(
              pointee.interval.move(i => i + pointeeOff) == newPointee.interval,
              s"pointee interval: ${pointee.interval}, moved by $pointeeOff, cloned Pointee interval: ${newPointee.interval}"
            )
            val pointer = newNode.get(cell.interval.move(i => i + off))
            debugAssert(pointer.interval.contains(cell.interval.move(i => i + off)))
            debugAssert(!pointer.hasPointee || pointer.getPointee.equiv(newPointee))
            if !pointer.hasPointee then pointer.setPointee(newPointee)
          case _ =>
        }

    newNode
  }

  override def hashCode(): Int = id

  def pretty(verbose: Boolean = false): String = {
    val baseString =
      if verbose then bases.mkString("\n")
      else bases.filterNot(i => isPlaceHolder(i._1)).mkString("\n")

    val intervals =
      if isCollapsed then DSInterval.Top.toString
      else cells.map(_.interval).map(_.toString).mkString("\n")

    s"Node $id\n$baseString\n$intervals"
  }
  override def toString: String =
    s"Node($id, ${bases.keys}, ${if isCollapsed then "C" else cells.map(_.interval).sorted})"

  def add(cell: IntervalCell): Unit = {
    require(cell.node == this, "added cell must have a reference to this node")
    _cells = _cells.appended(cell).sorted
    debugAssert(nonOverlappingProperty, "expected non overlapping cells")
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

  def init(interval: DSInterval): IntervalCell = IntervalCell(this, interval)

  override def equals(obj: Any): Boolean = {
    obj match
      case node: IntervalNode => id == node.id
      case _ => false
  }

  def collapse(): IntervalCell = {
    debugAssert(isUptoDate)
    flags.collapsed = true
    bases = bases.view.mapValues(_ => Set(0)).toMap
    eqClasses = Set.empty
    graph.get(add(DSInterval.Top))
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
  def add(interval: DSInterval): IntervalCell = {
    debugAssert(isUptoDate)
    val overlapping: Seq[IntervalCell] = cells.filter(_.interval.isOverlapping(interval))
    val newCell = if overlapping.isEmpty then
      val res = init(interval)
      _cells = _cells.appended(res).sorted
      res
    else if overlapping.size == 1 && overlapping.head.interval == (interval) then this.get(interval)
    else if overlapping.size == 1 && overlapping.head.interval.contains(interval) then init(interval)
    else
      val unifiedInterval = overlapping.map(_.interval).fold(interval)(DSInterval.join)
      val res = init(unifiedInterval)
      val pointees = overlapping.filter(_.hasPointee).map(_.getPointee)

      _cells = cells.diff(overlapping).appended(res).sorted
      val pointee = if pointees.nonEmpty then Some(graph.mergeCells(pointees)) else None
      if pointees.nonEmpty then graph.find(res).setPointee(pointee.get)
      init(interval)

    debugAssert(nonOverlappingProperty, "expected non overlapping cells")
    newCell
  }

  // get the cell which contains this interval in the node
  // expects exactly 1 corresponding cell since they are non-overlapping
  def get(interval: DSInterval): IntervalCell = {
    val exactMatches = cells.filter(_.interval.contains(interval))
    debugAssert(
      exactMatches.size == 1,
      s"Expected exactly one overlapping interval instead got ${exactMatches.size}, ${interval}, with ${cells.map(_.interval)}"
    )
    exactMatches.head
  }

  def maintainEqClasses(): Unit = {
    assert(this.isUptoDate)
    eqClasses = eqClasses.map( // map to most updated intervals
      eqClass => eqClass.map(c => this.get(c.interval))
    )

    cells.foreach(c =>
      val common = eqClasses.filter(_.contains(c))
      eqClasses = (eqClasses -- common)
      if common.flatten.nonEmpty then eqClasses = eqClasses + common.flatten
    )

    eqClasses.foreach(eqClass =>
      assert(eqClass.nonEmpty)
      graph.mergeCells(eqClass.map(_.getPointee))
    )

    /*if !eqClassProperty() then {
        eqClasses.foreach(
        eqClass =>
          val size = eqClass.map(_.interval.size.getOrElse(0)).max
          eqClass.foreach(c => this.add(c.interval.growTo(size)))
       )
      }*/

    if !graph.find(this).eqClassProperty() then graph.find(this).maintainEqClasses()
  }

  def eqClassProperty(): Boolean = {
    assert(this.isUptoDate)
    var seen: Set[IntervalCell] = Set.empty
    !eqClasses.exists(eqClass =>
      val cond = (!(IntervalDSA.equiv(eqClass.map(_.getPointee))) ||
        eqClass.exists(seen.contains) ||
        !(eqClass.forall(c => cells.contains(c))))
      seen ++= eqClass
      cond
    )
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
class IntervalCell(val node: IntervalNode, val interval: DSInterval) {
  var _pointee: Option[IntervalCell] = None
  private val graph: IntervalGraph = node.graph

  override def toString: String = s"Cell($node, $interval)"

  def moved(f: Int => Int): IntervalCell = {
    val newCell = IntervalCell(node, interval.move(f))
    newCell._pointee = _pointee
    newCell
  }

  def grown(interval: DSInterval): IntervalCell = {
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
    val updatedthis = graph.get(this)
    val updatedOther = graph.get(other)
    updatedOther.equals(updatedthis) ||
    updatedthis.node.eqClasses.exists(s => s.contains(updatedthis) && s.contains(updatedOther))
  }

  def removePointee: Option[IntervalCell] = {
    assert(this.node.isUptoDate)
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
      debugAssert(this.node.isUptoDate)
      _pointee = Some(IntervalNode(graph, Map.empty).add(0))
      graph.find(_pointee.get)
    else graph.find(_pointee.get)
  }

  def hasPointee: Boolean = node.get(this.interval)._pointee.nonEmpty

  def setPointee(cell: IntervalCell): IntervalCell = {
    debugAssert(this.node.isUptoDate)
    debugAssert(cell.node.isUptoDate)
    if node.get(this.interval) ne this then node.get(this.interval).setPointee(cell)
    else if _pointee.isEmpty then
      debugAssert(node.cells.contains(this))
      _pointee = Some(cell)
      cell
    else graph.mergeCells(cell, this.getPointee)
  }
}

class IntervalDSA(irContext: IRContext, config: DSConfig) {
  val globals = {
    val glIntervals = globalIntervals(irContext)
    if config.splitGlobals then glIntervals
    else if glIntervals.isEmpty then glIntervals
    else Seq(glIntervals.head.join(glIntervals.last))
  }

  def pre(): (Map[Procedure, SymValues[OSet]], Map[Procedure, Set[Constraint]]) = {
    var sva: Map[Procedure, SymValues[OSet]] = Map.empty
    var cons: Map[Procedure, Set[Constraint]] = Map.empty
    computeDSADomain(irContext.program.mainProcedure, irContext).toSeq
      .sortBy(_.name)
      .foreach(proc =>
        val SVAResults = getSymbolicValues[OSet](irContext, proc, globals)
        val constraints = generateConstraints(proc)
        sva += (proc -> SVAResults)
        cons += (proc -> constraints)
      )
    (sva, cons)
  }

  def dsa(): DSAContext = {
    val DSATimer = PerformanceTimer("DSA Timer", INFO)
    val (sva, cons) = pre()
    DSATimer.checkPoint("Finished SVA")
    DSALogger.info("Finished Computing Constraints")

    val checks = config.checks
    var dsaContext = DSAContext(sva, cons, Map.empty, Map.empty, Map.empty, Map.empty)
    if (config.phase != DSAPhase.Pre) then {
      val DSA = IntervalDSA.getLocals(irContext, sva, cons, globals, config.globalAsserts, config.eqClasses)
      DSATimer.checkPoint("Finished DSA Local Phase")
      dsaContext = dsaContext.copy(local = DSA)
      if checks then {
        DSA.values.foreach(checkUniqueGlobals)
        DSA.values.foreach(_.localCorrectness())
        IntervalDSA.checkMemoryAccesses(irContext, DSA)
        DSA.values.foreach(IntervalDSA.checkUniqueNodesPerRegion)
        DSALogger.info("Performed correctness checks")
      }
    }

    var rpo: Seq[Set[Procedure]] = Seq() // reverse postorder over call graph scc
    if config.phase == DSAPhase.BU || config.phase == DSAPhase.TD then {
      val (dsaBU, order) = IntervalDSA.solveBUs(irContext.program.mainProcedure, dsaContext.local)
      DSATimer.checkPoint("Finished DSA BU Phase")
      dsaContext = dsaContext.copy(bottomUp = dsaBU)
      rpo = order.reverse
      if checks then {
        dsaBU.values.foreach(checkUniqueGlobals)
        dsaBU.values.foreach(_.localCorrectness())
        DSALogger.info("Performed correctness check")
      }
    }

    if config.phase == DSAPhase.TD then {
      val DSATD = IntervalDSA.solveTDs(dsaContext.bottomUp, rpo)
      DSATimer.checkPoint("Finished DSA TD Phase")
      if checks then
        DSATD.values.foreach(checkUniqueGlobals)
        DSATD.values.foreach(_.localCorrectness())
        DSALogger.info("Performed correctness check")

      val globalGraph =
        IntervalDSA.getLocal(
          irContext.program.mainProcedure,
          irContext,
          SymValues[OSet](Map.empty),
          Set[Constraint](),
          globals,
          config.globalAsserts,
          config.eqClasses
        )
      DSATD.values.foreach(g => IntervalDSA.globalTransfer(g, globalGraph))
      if config.splitGlobals then IntervalDSA.resolveGlobalOverlapping(globalGraph)
      val globalMapping = DSATD.values.foldLeft(Map[IntervalNode, IntervalNode]()) { (m, g) =>
        val oldToNew = IntervalDSA.globalTransfer(globalGraph, g)
        m ++ oldToNew.map((common, spec) => (spec, common))

      }
      dsaContext = dsaContext.copy(topDown = DSATD, globals = globalMapping)
      DSATimer.checkPoint("Finished DSA global graph")
      if checks then
        DSATD.values.foreach(checkUniqueGlobals)
        DSATD.values.foreach(_.localCorrectness())
        IntervalDSA.checkConsistantGlobals(DSATD, globalGraph)
        IntervalDSA.checkMemoryAccesses(irContext, DSATD)
        DSALogger.info("Performed correctness check")
        DSATimer.checkPoint("Finished DSA Invariant Check")
    }

    if config.globalAsserts then {
      val current = config.phase match {
        case DSAPhase.Pre => Map.empty
        case DSAPhase.Local => dsaContext.local
        case DSAPhase.BU => dsaContext.bottomUp
        case DSAPhase.TD => dsaContext.topDown
      }
      current.values.foreach(IntervalDSA.insertGlobalAssertion)
    }
    dsaContext
  }

}

object IntervalDSA {

  def unifyGraphs(source: IntervalGraph, target: IntervalGraph)(using svDomain: SymValSetDomain[OSet]) = {
    val oldToNew = mutable.Map[IntervalNode, IntervalNode]()
    source.proc.formalInParam.foreach(p =>
      val base = Par(target.proc, p)
      if !target.nodes.contains(base) then {
        target.nodes += (base -> IntervalNode(target, Map(base -> Set(0))))
        target.sva = SymValues(target.sva.state + (p -> svDomain.init(base)))
      }
      exprTransfer(p, p, source, target, oldToNew)
    )

    /*source.proc.formalOutParam.foreach(
      p =>
        val base = Par(target.proc, p)
        if !target.nodes.contains(base) then {
          target.nodes += (base -> IntervalNode(target, Map(base -> Set(0))))
          target.sva = SymValues(target.sva.state + (p -> svDomain.init(base)))
        }
        exprTransfer(p, p, source, target, oldToNew)
    )*/
  }

  def equiv(cells: Set[IntervalCell]): Boolean = {
    if cells.size > 1 then {
      val head = cells.head
      val tail = cells.tail
      tail.forall(head.equiv)
    } else true
  }

  def resolveGlobalOverlapping(graph: IntervalGraph): Unit = {
    graph.glIntervals.zipWithIndex.foreach {
      case (interval, i) if i < graph.glIntervals.size =>
        val base = GlobSym(interval)
        val node = graph.find(base)
        val offsets = node.bases(base)
        assert(offsets.size == 1)
        val offset = offsets.head
        if node.cells.last.interval.end.getOrElse(0) > interval.size.get + offset then {
          val next = graph.glIntervals(i + 1)
          val nextOffset = next.start.get - interval.start.get + offset
          if nextOffset < node.cells.last.interval.end.get then {
            val nextCell = graph.find(graph.nodes(GlobSym(next)).get(0))
            val cell = node.add(nextOffset)
            graph.mergeCells(nextCell, cell)
          }
        }
      case _ =>
    }
  }

  def checksGlobalsMaintained(graph: IntervalGraph): Boolean = {
    graph.glIntervals.forall(i => !graph.find(GlobSym(i)).isCollapsed)
  }

  def checksStackMaintained(graph: IntervalGraph): Boolean = {
    if graph.nodes.contains(Stack(graph.proc)) then !graph.find(Stack(graph.proc)).isCollapsed
    else true
  }

  def globalTransfer(
    source: IntervalGraph,
    target: IntervalGraph,
    oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map.empty
  ): Map[IntervalNode, IntervalNode] = {
    DSALogger.info(s"cloning globalNode from ${source.proc.procName}")
    source.glIntervals.foreach { interval =>
      val targetGlobal = target.find(target.nodes(GlobSym(interval)).get(0))
      var sourceGlobal = source.find(source.nodes(GlobSym(interval)).get(0))
      val globalNode = sourceGlobal.node.clone(target, true, oldToNew)

      sourceGlobal = IntervalCell(globalNode, sourceGlobal.interval)
      target.mergeCells(sourceGlobal, targetGlobal)
    }

    oldToNew.map((old, outdatedNew) => (old, target.find(outdatedNew))).toMap
  }

  def callTransfer(phase: DSAPhase, cons: DirectCallConstraint, source: IntervalGraph, target: IntervalGraph): Unit = {
    require(phase == TD || phase == BU)
    val oldToNew = mutable.Map[IntervalNode, IntervalNode]()
    globalTransfer(source, target, oldToNew)
    val unchanged = Set("R29", "R30", "R31")
    DSALogger.info(s"cloning ${source.proc.procName} into ${target.proc.procName}, $phase")
    cons.inParams
      .filterNot(f => unchanged.exists(i => f._1.name.startsWith(i)))
      .filter(f => cons.target.formalInParam.contains(f._1))
      .foreach { case (formal, actual) =>
        val (sourceExpr, targetExpr) = if phase == TD then (actual, formal) else (formal, actual)
        exprTransfer(sourceExpr, targetExpr, source, target, oldToNew)
      }

    cons.outParams
      .filterNot(f => unchanged.exists(i => f._1.name.startsWith(i)))
      .filter(f => cons.target.formalOutParam.contains(f._1))
      .foreach { case (out, actual) =>
        val (sourceExpr, targetExpr) = if phase == TD then (actual, out) else (out, actual)
        exprTransfer(sourceExpr, targetExpr, source, target, oldToNew)
      }
    // TODO add unification between unused indirect call out params and their corresponding input version
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
        debugAssert(cell.node.isUptoDate)
        val (node, offset) =
          target.findNode(cell.node.clone(target, true, oldToNew))
        node.get(cell.interval.move(i => i + offset))
      )
      .map(target.find)
    val targetCells = target.exprToCells(targetExpr).map(target.find)

    if (targetCells ++ sourceCells).nonEmpty then target.mergeCells(targetCells ++ sourceCells)

    sourceCells.foreach(cell =>
      val node = target.find(cell.node)
      val sourceBases = node.bases
      sourceBases.foreach {
        case (base, off) if target.nodes.contains(base) =>
          val t = target.find(target.nodes(base).get(0))
          if t.node.isCollapsed || cell.node.isCollapsed then target.mergeCells(cell, t)
          else if off.size == 1 then target.mergeCells(node.get(off.head), t)
          else {
            DSALogger.warn(
              s"Duplicate offsets for base, $base, $off, source: ${source.proc.procName}, Source Expr: $sourceExpr,  target: ${target.proc.procName}, targetExpr: $targetExpr "
            )
            target.mergeCells(node.collapse(), t)
          }
        case _ =>
      }
    )
  }

  def getPointers(graph: IntervalGraph): Map[IntervalCell, Set[IntervalCell]] = {
    val (nodes, edges) = graph.collect()
    edges.groupMap((_, pointee) => pointee)((pointer, _) => pointer)
  }

  /**
   *  checks that (non Heap/Ret) regions only belongs to a single node
   *  holds after local phase
   *
   *
   *  Heap and Ret may be duplicated due to multiple calls to the same function
   *
   *  potentially violated by BU and TD phases due to cloning duplicate regions
   */
  def checkUniqueNodesPerRegion(graph: IntervalGraph): Unit = {
    val found = mutable.Map[SymBase, IntervalNode]()
    val seen = mutable.Set[IntervalNode]()
    val entry = graph.nodes.values.map(graph.find)
    val queue = mutable.Queue[IntervalNode]().enqueueAll(entry)
    while queue.nonEmpty do {
      val node = queue.dequeue()
      node.bases.keys.foreach(base =>
        debugAssert(!found.contains(base) || found(base) == node, s"$base was in $node and ${found(base)}")
      )
      node.bases.keys.foreach(found.update(_, node))
      seen.add(node)
      val toDo = node.cells.filter(_.hasPointee).map(_.getPointee).map(_.node).filterNot(seen.contains)
      queue.enqueueAll(toDo)
    }
  }

  def checkUniqueGlobals(graph: IntervalGraph): Unit = {
    var found: Map[SymBase, Option[IntervalNode]] = graph.glIntervals.map(i => (GlobSym(i), None)).toMap
    val seen = mutable.Set[IntervalNode]()
    val entry = graph.nodes.values.map(graph.find)
    val queue = mutable.Queue[IntervalNode]().enqueueAll(entry)
    while queue.nonEmpty do {
      val node = queue.dequeue()
      node.bases
        .filter(_._1.isInstanceOf[GlobSym])
        .foreach((base, offsets) =>
          debugAssert(offsets.size <= 1, s"$base had more than one offset in $node")
          debugAssert(found(base).isEmpty || found(base).get == node, s"$base was in at least two nodes")
          found += (base -> Some(node))
        )
      seen.add(node)
      val toDo = node.cells.filter(_.hasPointee).map(_.getPointee).map(_.node).filterNot(seen.contains)
      queue.enqueueAll(toDo)
    }
  }

  private def isNonGlobalConstant(expr: Expr, isGlobal: Int => Boolean): Boolean = {
    expr match
      case literal: BitVecLiteral =>
        !isGlobal(literal.value.toInt)
      case _ => false
  }

  /**
   *  checks that all reachable memory load and stores in DSA's domain have dsa cell(s)
   *  which corresponds to their index expr
   *
   *  additionally checks the index cell(s) have unified pointee or don't have any pointees (skipped constraint)
   */
  def checkMemoryAccesses(ctx: IRContext, DSA: Map[Procedure, IntervalGraph]): Unit = {
    val isglb = (i: Int) => isGlobal(i, ctx)
    ctx.program.foreach {
      case access @ MemoryAccess(index) if DSA.contains(IRWalk.procedure(access)) =>
        val dsg = DSA(IRWalk.procedure(access))
        debugAssert(
          dsg.constraints.exists(_.source == access),
          s"Memory Access $access from ${dsg.proc.procName} didn't have a corresponding DSA constraint"
        )
        val pointers = dsg.exprToCells(index).map(dsg.find)
        debugAssert(
          pointers.nonEmpty || isNonGlobalConstant(index, isglb),
          "Expected cells for indices used in reachable memory access to have corresponding DSA cells"
        )
        debugAssert(pointers.forall(_.hasPointee), "expected all of the pointers to have pointee")
        val distinctPointees = pointers.map(_.getPointee).map(dsg.get).toSet

        debugAssert(
          distinctPointees.size <= 1,
          s"Expected index cells to have unified pointee ${distinctPointees.size}"
        )
      case _ =>
    }
  }

  /**
   * Checks that the same regions are unified with global across all procedures
   * that is if (A and Global) are unified at offset C in one procedure they are unified at offset C
   * across all procedures
   * Should hold at the end of DSA
   */
  def checkConsistantGlobals(DSA: Map[Procedure, IntervalGraph], global: IntervalGraph): Unit = {
    // collect all the regions  from all the resulting graphs
    val unifiedRegions = global.glIntervals.map(GlobSym.apply).map(base => global.find(global.nodes(base)).bases)
    DSA
      .filterNot((proc, _) => proc.procName == "indirect_call_launchpad")
      .foreach((p, graph) =>
        val graphRegions = graph.glIntervals.map(GlobSym.apply).map(base => graph.find(graph.nodes(base)).bases)
        debugAssert(
          unifiedRegions == graphRegions,
          s"Procedure ${p.procName} had a differing unified global sets than compared to the global graph"
        )
      )
  }

  def getLocal(
    proc: Procedure,
    context: IRContext,
    symValues: SymValues[OSet],
    cons: Set[Constraint],
    globals: Seq[DSInterval],
    insertAsserts: Boolean,
    eqCells: Boolean
  ): IntervalGraph = {
    val graph = IntervalGraph(proc, Local, context, symValues, cons, globals, eqCells, None)
    graph.localPhase()
    graph
  }

  def getLocals(
    ctx: IRContext,
    svas: Map[Procedure, SymValues[OSet]],
    cons: Map[Procedure, Set[Constraint]],
    globals: Seq[DSInterval],
    insertAsserts: Boolean = false,
    eqClasses: Boolean = false
  ): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing local DSA")
    computeDSADomain(ctx.program.mainProcedure, ctx).toSeq
      .sortBy(_.name)
      .foldLeft(Map[Procedure, IntervalGraph]())((m, proc) =>
        m + (proc -> IntervalDSA.getLocal(proc, ctx, svas(proc), cons(proc), globals, insertAsserts, eqClasses))
      )
  }

  /**
   * computes the graph for a scc by merging their context
   * all in/out parameters of all the graphs are unified
   * @param graphs mapping from each procedure in the scc to their DSG
   * @param scc the call graph scc
   */
  def computeSCCGraph(graphs: mutable.Map[Procedure, IntervalGraph], scc: Set[Procedure]): Unit = {
    require(scc.forall(graphs.keySet.contains))
    require(scc.size > 1)
    val sscc = scc.toSeq.sortBy(_.formalInParam.size).reverse
    val head = graphs(scc.head)
    sscc.tail.foreach(p => IntervalDSA.unifyGraphs(graphs(p), head))
    scc.foreach(p => graphs.update(p, head))
  }

  /**
   * resolves bottom up constraints
   * @param init root node in call graph (main)
   * @param locals mapping from procedures to their DSG after the local phase
   * @return procedures
   */
  def solveBUs(
    init: Procedure,
    locals: Map[Procedure, IntervalGraph]
  ): (Map[Procedure, IntervalGraph], Seq[Set[Procedure]]) = {
    DSALogger.info("Performing DSA BU phase")
    val bus: mutable.Map[Procedure, IntervalGraph] = mutable.Map(locals.view.mapValues(_.clone).toSeq: _*)
    DSALogger.info("performed cloning")
    var order: Seq[Set[Procedure]] = Seq()
    var visited: Set[Set[Procedure]] = Set()
    def visitPostOrder(scc: Set[Procedure], bus: mutable.Map[Procedure, IntervalGraph]): Unit = {
      visited += scc
      CallSCCWalker.succ(scc).diff(visited).foreach(i => visitPostOrder(i, bus))
      if scc.size == 1 then bus(scc.head).contextTransfer(BU, bus.toMap)
      else computeSCCGraph(bus, scc)
      order = order.appended(scc)
    }
    visitPostOrder(init.scc.getOrElse(Set(init)), bus)
    (bus.toMap, order)
  }

  /**
   * resolves top down constraints
   * @param bus mapping from procedures to their DSG after the bottom up phase
   * @param rpo reverse of order in which bottom up phase visited (reverse postorder)
   * @return DSG graphs after applying top down phase
   */
  def solveTDs(bus: Map[Procedure, IntervalGraph], rpo: Seq[Set[Procedure]]): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing DSA TD phase")
    val oldToNew = mutable.Map[IntervalGraph, IntervalGraph]()
    val tds = bus.map {
      case (proc, graph) if oldToNew.contains(graph) => (proc, oldToNew(graph))
      case (proc, graph) =>
        val clone = graph.clone()
        oldToNew.update(graph, clone)
        (proc, clone)
    }
    rpo.foreach(scc => // all procedures in a scc have the same graph
      val proc = scc.head
      tds(proc).contextTransfer(TD, tds)
    )
    tds
  }

  def insertGlobalAssertion(graph: IntervalGraph): Unit = {
    graph.constraints.foreach {
      case cons: MemoryAccessConstraint[_] =>
        val pos = cons.source
        val indices = graph.constraintArgToCells(cons.arg1, true, ignoreContents = true)
        val collapsedGlobals =
          indices.filter(_.node.isCollapsed).flatMap(_.node.bases.keys.collect { case g: GlobSym => g })
        if collapsedGlobals.nonEmpty then {
          val assertExpr = collapsedGlobals
            .map(g =>
              BinaryExpr(
                BoolAND,
                BinaryExpr(BVSGE, cons.arg1.value, BitVecLiteral(g.interval.start.get, 64)),
                BinaryExpr(BVSLT, cons.arg1.value, BitVecLiteral(g.interval.end.get, 64))
              )
            )
            .reduce((e1, e2) => BinaryExpr(BoolOR, e1, e2))
          val assertSt = Assert(assertExpr)
          pos.parent.statements.insertBefore(pos, assertSt)
        }

      case _ => // ignore
    }
  }
}

object MemoryAccess {
  def unapply(m: MemoryLoad | MemoryStore): Some[Expr] = m match {
    case m: MemoryLoad => Some(m.index)
    case m: MemoryStore => Some(m.index)
  }
}

class DSFlag {
  var collapsed = false
  var function = false
  var stack = false
  var heap = false
  var global = false
  var unknown = false
  var escapes = false
  var read = false
  var modified = false
  var incomplete = false
  var foreign = false
  var merged = false

  def join(other: DSFlag): Unit =
    collapsed = collapsed || other.collapsed
    stack = other.stack || stack
    heap = other.heap || heap
    global = other.global || global
    unknown = other.unknown || unknown
    read = other.read || read
    escapes = escapes || other.escapes
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    foreign = other.foreign && foreign
    merged = true
    function = function || other.function
}

def joinFlags(pointers: Iterable[IntervalCell]): DSFlag = {
  val flag = DSFlag()
  pointers.foreach(c => flag.join(c.node.flags))
  flag
}

def estimateStackSize(program: Program): Map[Procedure, Option[Int]] = {
  program.procedures.foldLeft(Map[Procedure, Option[Int]]()) { (m, proc) =>
    val size = proc.collectFirst {
      case LocalAssign(_, BinaryExpr(BVADD, Register("R31", 64), arg2: BitVecLiteral), _) if isNegative(arg2) =>
        ir.eval.BitVectorEval.bv2SignedInt(arg2).toInt * -1
    }
    m + (proc -> size)
  }
}

def computeDSADomain(proc: Procedure, context: IRContext): Set[Procedure] = {
  var domain: Set[Procedure] =
    Set(proc) ++ (context.program.procedures.filter(f => context.funcEntries.map(_.name).contains(f.procName)))

  val stack: mutable.Stack[Procedure] = mutable.Stack()
  stack.pushAll(domain.flatMap(_.calls))

  // calculate the procedures used in the program
  while (stack.nonEmpty) {
    val current = stack.pop()
    domain += current
    stack.pushAll(current.calls.diff(domain))
  }

  domain
}

def globalIntervals(ctx: IRContext): Seq[DSInterval] = {
  val globals = ctx.globals ++ ctx.funcEntries
  val intervals = mutable.Set[DSInterval]()

  globals.toSeq.sortBy(_.address).foreach {
    case FuncEntry(name, size, address) =>
      intervals += DSInterval(address.toInt, address.toInt + size / 8)
    case SpecGlobal(name, size, arraySize, address) =>
      intervals += DSInterval(address.toInt, address.toInt + size / 8) // ignore size, could be a composite type
  }

  ctx.globalOffsets.foreach { case (address, relocated) =>
    if !intervals.exists(_.contains(address.toInt)) then intervals += DSInterval.Value(address.toInt, address.toInt + 8)
    if !intervals.exists(_.contains(relocated.toInt)) then
      intervals += DSInterval.Value(relocated.toInt, relocated.toInt)
  }

  ctx.externalFunctions.foreach(e =>
    if !intervals.exists(_.contains(e.offset.toInt)) then intervals += DSInterval.Value(e.offset.toInt, e.offset.toInt)
  )

  val seq = intervals.toSeq.sorted // sorted for easier overlapping check
  if seq.size > 1 then {
    seq.sliding(2).foreach(v => assert(!v(0).isOverlapping(v(1))))
  }

  seq
}

def getGlobal(globals: Seq[DSInterval], offset: Int, base: Option[DSInterval] = None): Option[(DSInterval, Int)] = {
  require(base.isEmpty || base.get.isInstanceOf[DSInterval.Value])
  if base.nonEmpty && offset < base.get.size.get then Some(base.get, offset)
  else {
    val literal = base match {
      case Some(value) => value.start.get + offset
      case None => offset
    }
    val newBase = globals.filter(_.contains(literal))
    if newBase.size > 1 then newBase.foreach(_.contains(literal))
    assert(newBase.size <= 1, s"expected to match at most one global but got, $literal,  $newBase")
    if newBase.isEmpty then None
    else {
      val globalInterval = newBase.head
      val newOffset = literal - globalInterval.start.get
      Some(globalInterval, newOffset)
    }
  }
}

def isGlobal(address: Int, irContext: IRContext): Boolean = {
  (irContext.globals ++ irContext.funcEntries).exists(g =>
    DSInterval(g.address.toInt, g.address.toInt + g.size / 8).contains(address)
  ) || irContext.globalOffsets.exists((g1, g2) => g1 == address || g2 == address) || irContext.externalFunctions
    .exists(g => address == g.offset)
}

// --- new metrics ---

case class DensityMetrics(
  procToNodes: Map[Procedure, Set[IntervalNode]],
  allNodes: Set[IntervalNode],
  nodeCount: Int,
  nodeToAccesses: Map[IntervalNode, Set[MemoryAccessConstraint[_]]],
  nodeToCells: Map[IntervalNode, Set[IntervalCell]],
  cellToAccesses: Map[IntervalCell, Set[MemoryAccessConstraint[_]]],
  maxNodeAccesses: Int,
  maxCellAccesses: Int,
  maxNodeDensity: Double,
  maxCellDensity: Double,
  numberOfAccesses: Int,
  memoryAccesses: Set[MemoryAccessConstraint[_]]
) {
  override def toString(): String =
    "=== NODE INFORMATION ===========================================================" +
    "\n\n--- Node Count -----------------------------------------------------------------\n" +
    nodeCount.toString +
    "\n\n--- All Nodes ------------------------------------------------------------------\n" +
    allNodes.map(_.toString).mkString("\n") +
    "\n\n--- Nodes by Procedure ---------------------------------------------------------\n" +
    procToNodes.map((proc, nodes) => s"${proc.procName} -> ${nodes.map(_.id.toString).mkString(", ")}").mkString("\n") +
    "\n\n--- Cells by Node --------------------------------------------------------------\n" +
    nodeToCells.map((n, c) => s"${n.id.toString} -> ${c.map(_.toString).mkString("\n       ")}").mkString("\n") +
    "\n\n=== ACCESS INFORMATION =========================================================" +
    "\n\n--- Number of Accesses ---------------------------------------------------------\n" +
    numberOfAccesses.toString +
    "\n\n--- All Accesses ---------------------------------------------------------------\n" +
    memoryAccesses.map(_.toString).mkString("\n") +
    "\n\n--- Accesses by Node -----------------------------------------------------------\n" +
    nodeToAccesses.map((n, a) => s"${n.id.toString} -> ${a.map(_.toString).mkString(", ")}").mkString("\n") +
    "\n\n--- Accesses by Cell -----------------------------------------------------------\n" +
    cellToAccesses.map((c, a) => s"${c.toString} -> ${a.map(_.toString).mkString(", ")}").mkString("\n") +
    "\n\n--- Maximum Accesses to a Single Node ------------------------------------------\n" +
    maxNodeAccesses.toString +
    "\n\n--- Maximum Accesses to a Single Cell ------------------------------------------\n" +
    maxCellAccesses.toString +
    "\n\n=== DENSITY INFORMATION ========================================================" +
    "\n\n--- Maximum Node Density -------------------------------------------------------\n" +
    maxNodeDensity.toString +
    "\n\n--- Maximum Cell Density -------------------------------------------------------\n" +
    maxCellDensity.toString
  }

def getDensityMetrics(dsaCtx: DSAContext, ignoredProcs: Set[Procedure] = Set.empty): DensityMetrics = DensityMetrics(
  getProcToNodes(dsaCtx, ignoredProcs),
  getAllNodes(dsaCtx, ignoredProcs),
  getNodeCount(dsaCtx, ignoredProcs),
  getNodeToAccesses(dsaCtx, ignoredProcs),
  getNodeToCells(dsaCtx, ignoredProcs),
  getCellToAccesses(dsaCtx, ignoredProcs),
  getMaxNodeAccesses(dsaCtx, ignoredProcs),
  getMaxCellAccesses(dsaCtx, ignoredProcs),
  getMaxNodeDensity(dsaCtx, ignoredProcs),
  getMaxCellDensity(dsaCtx, ignoredProcs),
  getNumberOfAccesses(dsaCtx, ignoredProcs),
  getMemoryAccesses(dsaCtx, ignoredProcs)
)

def relevantDsgs(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[Procedure, IntervalGraph] =
  dsaCtx.topDown.filter{ case (proc, _) => !ignoredProcs.contains(proc) }

def relevantConstraints(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[Procedure, Set[Constraint]] =
  dsaCtx.constraints.filter{ case (proc, _) => !ignoredProcs.contains(proc) }

def getProcToNodes(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[Procedure, Set[IntervalNode]] =
  relevantDsgs(dsaCtx, ignoredProcs).mapValues(_.collect()(0)).toMap

def getAllNodes(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Set[IntervalNode] =
  relevantDsgs(dsaCtx, ignoredProcs).values.flatMap(_.collect()(0)).toSet

def getNodeCount(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Int =
  getAllNodes(dsaCtx, ignoredProcs).size

def getMemoryAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Set[MemoryAccessConstraint[_]] =
  relevantConstraints(dsaCtx, ignoredProcs).values.flatten.toSet.collect { case m: MemoryAccessConstraint[_] => m }

def getAccessesToNode(dsaCtx: DSAContext, dsg: IntervalGraph, node: IntervalNode, ignoredProcs: Set[Procedure]): Set[MemoryAccessConstraint[_]] =
  getMemoryAccesses(dsaCtx, ignoredProcs).filter(m => !(dsg.exprToCells(m.index).map(dsg.find) & node.cells.toSet).isEmpty)

def getAccessesToCell(dsaCtx: DSAContext, dsg: IntervalGraph, cell: IntervalCell, ignoredProcs: Set[Procedure]): Set[MemoryAccessConstraint[_]] =
  getMemoryAccesses(dsaCtx, ignoredProcs).filter(m => dsg.exprToCells(m.index).map(dsg.find).contains(cell))

def zipMaps[A, B](map1: Map[A, Set[B]], map2: Map[A, Set[B]]) =
  map2.foldLeft(map1) { case (acc, (k, v)) => acc + (k -> acc.get(k).fold(v)(_ ++ v)) }

def getNodeToAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[IntervalNode, Set[MemoryAccessConstraint[_]]] =
  relevantDsgs(dsaCtx, ignoredProcs).values.map(dsg =>
    dsg.collect()(0).map(node => node -> getAccessesToNode(dsaCtx, dsg, node, ignoredProcs)).toMap
  ).foldLeft(Map.empty[IntervalNode, Set[MemoryAccessConstraint[_]]]) {
    (map1, map2) => zipMaps(map1, map2)
  }

def getNodeToCells(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[IntervalNode, Set[IntervalCell]] =
  getAllNodes(dsaCtx, ignoredProcs).map(node => node -> node.cells.toSet).toMap

def getCellToAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Map[IntervalCell, Set[MemoryAccessConstraint[_]]] =
  relevantDsgs(dsaCtx, ignoredProcs).values.map(dsg =>
    dsg.collect()(0).flatMap(_.cells).map(cell => cell -> getAccessesToCell(dsaCtx, dsg, cell, ignoredProcs)).toMap
  ).foldLeft(Map.empty[IntervalCell, Set[MemoryAccessConstraint[_]]]) {
    (map1, map2) => zipMaps(map1, map2)
  }

def getMaxNodeAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Int =
  getNodeToAccesses(dsaCtx, ignoredProcs).values.map(_.size).foldLeft(0)(math.max)

def getMaxCellAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Int =
  getCellToAccesses(dsaCtx, ignoredProcs).values.map(_.size).foldLeft(0)(math.max)

def getMaxNodeDensity(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Double =
  getMaxNodeAccesses(dsaCtx, ignoredProcs).toDouble / getNumberOfAccesses(dsaCtx, ignoredProcs)

def getMaxCellDensity(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Double =
  getMaxCellAccesses(dsaCtx, ignoredProcs).toDouble / getNumberOfAccesses(dsaCtx, ignoredProcs)

def getNumberOfAccesses(dsaCtx: DSAContext, ignoredProcs: Set[Procedure]): Int =
  getMemoryAccesses(dsaCtx, ignoredProcs).size

// --- old metrics ---

def getBroadDensities(dsaCtx: DSAContext): (Double, Double) = {
  var maxNodeAccesses = 0
  var maxCellAccesses = 0
  
  // get all memory access constraints in the program
  val memoryAccesses: Set[MemoryAccessConstraint[_]] = dsaCtx.constraints.values.flatten.toSet.collect {
    case m: MemoryAccessConstraint[_] => m
  }
  if memoryAccesses.size == 0 then return (-1, -1)
  
  for (dsg <- dsaCtx.topDown.values) {
    for (node <- dsg.collect()(0)) {
      var nodeAccesses = 0
      for (cell <- node.cells) {
        var cellAccesses = 0
        for (m <- memoryAccesses) {
          for (cellAccessed <- dsg.exprToCells(m.index).map(dsg.find)) {
            if cellAccessed == cell then cellAccesses += 1
          }
        }
        nodeAccesses += cellAccesses
        if cellAccesses > maxCellAccesses then maxCellAccesses = cellAccesses
      }
      if nodeAccesses > maxNodeAccesses then maxNodeAccesses = nodeAccesses
    }
  }
  
  return (maxNodeAccesses.toDouble / memoryAccesses.size, maxCellAccesses.toDouble / memoryAccesses.size)
}

/**
  * Returns a map from each procedure p to a tuple (n, c) containing the maximum node density n and the maximum cell
  * density c in p's DSG. If a procedure p has no accesses, p maps to (-1, -1).
  */
def getDensities(dsaCtx: DSAContext): Map[Procedure, (Double, Double)] = {
  var ret = Map.empty[Procedure, (Double, Double)]
  // for each procedure 'proc' with DSG 'dsg', find the max node and cell density w.r.t. accesses in this procedure
  for ((proc, dsg) <- dsaCtx.topDown) {
    // these three values will be used to derive the density metrics
    var maxNodeAccesses = 0 // maximum number of accesses to a particular node
    var maxCellAccesses = 0 // maximum number of accesses to a particular cell
    // get the memory accesses in this procedure
    val memoryAccesses: Set[MemoryAccessConstraint[_]] = dsaCtx.constraints(proc).collect {
      case m: MemoryAccessConstraint[_] => m
    }
    // for each node in 'dsg', compute its number of accesses and the accesses for each of its cells
    for (node <- dsg.collect()(0)) {
      var nodeAccesses = 0 // we will compute this by summing the accesses to each of its cells
      // for each cell, sum its accesses
      for (cell <- node.cells) {
        var cellAccesses = 0
        // iterate through each access in the procedure and count up the ones that correspond to this cell
        // ??? unsure if we should be iterating through the memory accesses of all procedures
        for (m <- memoryAccesses) {
          for (cellAccessed <- dsg.exprToCells(m.index).map(dsg.find)) { // note: the size of this set should be 0 or 1 here
            if cellAccessed == cell then cellAccesses += 1 // ??? unsure about using '==' for equality check
          }
        }
        nodeAccesses += cellAccesses
        if cellAccesses > maxCellAccesses then maxCellAccesses = cellAccesses
      }
      if nodeAccesses > maxNodeAccesses then maxNodeAccesses = nodeAccesses
    }
    if (memoryAccesses.size == 0) {
      ret += (proc -> (-1.0, -1.0))
    } else {
      ret += (proc -> (maxNodeAccesses.toDouble / memoryAccesses.size, maxCellAccesses.toDouble / memoryAccesses.size))
    }
  }
  ret
}

def densitiesToCsv(densities: Map[Procedure, (Double, Double)]): String = {
  val header = "Procedure Name,Max Node Density,Max Cell Density"
  val rows = densities.map {
    case (proc, (nodeDensity, cellDensity)) => s"${proc.procName},$nodeDensity,$cellDensity"
  }.mkString("\n")

  s"$header\n$rows"
}
