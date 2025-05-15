package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.{BU, Local, TD}
import analysis.data_structure_analysis.OSet.Top
import analysis.data_structure_analysis.Global
import analysis.data_structure_analysis.IntervalDSA.checkUniqueGlobals
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import boogie.SpecGlobal
import specification.FuncEntry
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.*
import ir.eval.BitVectorEval.{bv2SignedInt, isNegative}
import specification.{ExternalFunction, SymbolTableEntry}
import util.LogLevel.INFO
import util.{DSAContext, DSALogger, DSConfig, IRContext, PerformanceTimer}

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
  val sva: SymValues[OSet],
  val constraints: Set[Constraint],
  val glIntervals: Seq[DSInterval],
  val nodeBuilder: Option[() => Map[SymBase, IntervalNode]]
) {

  val solver = OffsetUnionFindSolver[NodeTerm]()
  val builder: () => Map[SymBase, IntervalNode] = nodeBuilder.getOrElse(buildNodes)
  var nodes: Map[SymBase, IntervalNode] = builder()

  def exprToSymVal(expr: Expr): SymValSet[OSet] = SymValues.exprToSymValSet(sva, glIntervals)(expr)

  protected def symValToNodes(
    symVal: SymValSet[OSet],
    current: Map[SymBase, IntervalNode]
  ): Map[SymBase, IntervalNode] = {
    symVal.state.filter((base, _) => base != Constant).foldLeft(current) { case (result, (base, symOffsets)) =>
      val node = find(result.getOrElse(base, init(base, None)))
      base match
        case Heap(call) => node.flags.heap = true
        case Stack(proc) => node.flags.stack = true
        case Global(interval) => node.flags.global = true
        case Constant =>
        case unknown: (Ret | Par | Loaded) =>
          node.flags.unknown = true
          node.flags.incomplete = true
      result + (base -> node)
    }
  }

  def addGlobal(globals: Map[SymBase, IntervalNode], address: Int, size: Int): IntervalCell = {
    val (interval, offset) = getGlobal(glIntervals, address).get
    val base = Global(interval)
    val node = globals(base)
    node.flags.global = true
    node.add(DSInterval(offset, offset + size))
  }
  
  def buildGlobals(
    globals: Set[SymbolTableEntry],
    globalOffsets: Map[BigInt, BigInt],
    externalFunctions: Set[ExternalFunction]
  ): Map[SymBase, IntervalNode] = {
    val globalNodes = glIntervals.foldLeft(Map[SymBase, IntervalNode]()) {
      (m, interval) => 
        val base = Global(interval)
        val node = IntervalNode(this, Map(base -> Set(0)))
        m + (base -> node)
    }
    
    globals.toSeq.sortBy(_.address).foreach {
      case FuncEntry(name, size, address) => 
        addGlobal(globalNodes, address.toInt, size/8).node.flags.function = true
      case SpecGlobal(name, size, arraySize, address) =>
        addGlobal(globalNodes, address.toInt, size/8)
    }

    externalFunctions.foreach (
      e =>
        val node = addGlobal(globalNodes, e.offset.toInt, 0).node
        node.flags.function = true
        node.flags.foreign = true
    )

    globalOffsets.foreach {
      case (address, relocated) =>
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

  def isGlobal(address: Int): Boolean = {
    (irContext.globals ++ irContext.funcEntries).exists(g =>
      DSInterval(g.address.toInt, g.address.toInt + g.size / 8).contains(address)
    ) || irContext.globalOffsets.exists((g1, g2) => g1 == address || g2 == address) || irContext.externalFunctions
      .exists(g => address == g.offset)
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
    var processed = Set[Constraint]()
    constraints.toSeq
      .sortBy(f => f.label)
      .foreach(c =>
        processed += c
        processConstraint(c)
      )
  }

  // returns the cells corresponding to the
  def symValToCells(symVal: SymValSet[OSet]): Set[IntervalCell] = {
    val pairs = symVal.state.filter((base, _) => base != Constant)
    pairs.foldLeft(Set[IntervalCell]()) { case (results, (base: SymBase, offsets: OSet)) =>
      val (node, adjustment) = findNode(nodes(base))
      if offsets == Top then results + node.collapse()
      else {
        results ++ offsets.toIntervals
          .filter(i => !base.isInstanceOf[Global] || isGlobal(i.start.get + base.asInstanceOf[Global].interval.start.get))
          .map(_.move(i => i + adjustment))
          .map(node.add)
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

  /**
   * Clones a graph
   * all middle nodes in the union-find are lost
   * @return cloned graph
   */
  override def clone: IntervalGraph = {
    val oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map()
    val copy = IntervalGraph(proc, phase, irContext, sva, constraints, glIntervals, Some(() => Map[SymBase, IntervalNode]()))
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
    copy
  }

  def toDot: String = {

    val (nodes, pointsTo) = collect()
    val toRemove = Set('$', '#', '%')

    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    nodes.foreach { n =>
      structs.append(
        DotStruct(
          n.id.toString,
          s"Node ${n.id}\\n${n.bases.mkString("\\n").replace("->", ":")}",
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

  def init(symBase: SymBase, size: Option[Int]): IntervalNode = IntervalNode(this, Map(symBase -> Set(0)), size)
  def constraintArgToCells(constraintArg: ConstraintArg, ignoreContents: Boolean = false): Set[IntervalCell] = {
    val cells = symValToCells(exprToSymVal(constraintArg.value))
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
        val indices = constraintArgToCells(cons.arg1, ignoreContents = true)
        indices.foreach(cell => cell.node.add(cell.interval.growTo(cons.size)))
        val pointees = constraintArgToCells(cons.arg1)

        markEscapes(cons, indices, pointees)
        val values = constraintArgToCells(cons.arg2)
        if pointees.nonEmpty || values.nonEmpty then mergeCells(pointees ++ values)
        else DSALogger.warn(s"$cons had an empty argument")
      case _ => // ignore
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
      assert(pointee.equiv(get(pointer).getPointee), s"$pointer doesn't point to updated version of its pointee")
    )
  }

  protected def mergeCellsHelper(cell1: IntervalCell, cell2: IntervalCell): IntervalCell = {
    assert(cell1.node.isUptoDate)
    assert(cell2.node.isUptoDate)

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
      else if cell1.node.equals(cell2.node) then cell1.node.collapse()
      else if cell1.node.isCollapsed || cell2.node.isCollapsed then
        cell1.node.collapse()
        find(cell2).node.collapse()
        mergeCellsHelper(find(cell1), find(cell2))
      else mergeCellsHelper(cell1, cell2)

    assert(result.equiv(get(cell1)))
    assert(result.equiv(get(cell2)))
    checkEdgesAreMaintained(node1Pointees)
    checkEdgesAreMaintained(node2Pointees)

    assert(result.node.isUptoDate)
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
    assert(findNode(res.node)._1 == res.node)
    res
  }

  // find the most uptodate version of cell
  // if cell was unified with others, returns a cell with
  // unified interval
  def get(cell: IntervalCell): IntervalCell = {
    val (newNode, newInterval) = findExact(cell)
    val res = newNode.get(newInterval)
    assert(findNode(res.node)._1 == res.node)
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
    if recurse then assert(isUptoDate)
    val node = this
    val newNode =
      if !oldToNew.contains(node) then
        val v = IntervalNode(newGraph, node.bases, node.size)
        v.flags.join(node.flags)
        node.cells.foreach(cell => v.add(cell.interval))
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
              newNode.cells.exists(c => c.interval.contains(cell.interval.move(i => i + off))),
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
              pointee.interval.move(i => i + pointeeOff) == newPointee.interval,
              s"pointee interval: ${pointee.interval}, moved by $pointeeOff, cloned Pointee interval: ${newPointee.interval}"
            )
            val pointer = newNode.get(cell.interval.move(i => i + off))
            assert(pointer.interval.contains(cell.interval.move(i => i + off)))
            assert(!pointer.hasPointee || pointer.getPointee.equiv(newPointee))
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

  def init(interval: DSInterval): IntervalCell = IntervalCell(this, interval)

  override def equals(obj: Any): Boolean = {
    obj match
      case node: IntervalNode => id == node.id
      case _ => false
  }

  def collapse(): IntervalCell = {
    assert(isUptoDate)
    flags.collapsed = true
    bases = bases.view.mapValues(_ => Set(0)).toMap
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
    assert(isUptoDate)
    val overlapping: Seq[IntervalCell] = cells.filter(_.interval.isOverlapping(interval))
    val newCell = if overlapping.isEmpty then
      val res = init(interval)
      _cells = _cells.appended(res)
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

    assert(nonOverlappingProperty, "expected non overlapping cells")
    newCell
  }

  // get the cell which contains this interval in the node
  // expects exactly 1 corresponding cell since they are non-overlapping
  def get(interval: DSInterval): IntervalCell = {
    val exactMatches = cells.filter(_.interval.contains(interval))
    assert(
      exactMatches.size == 1,
      s"Expected exactly one overlapping interval instead got ${exactMatches.size}, ${interval}, with ${cells.map(_.interval)}"
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
    graph.get(this).equals(graph.get(other))
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
      _pointee = Some(IntervalNode(graph, Map.empty).add(0))
      graph.find(_pointee.get)
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
        val SVAResults = getSymbolicValues[OSet](proc, globals)
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
      val DSA = IntervalDSA.getLocals(irContext, sva, cons, globals)
      DSATimer.checkPoint("Finished DSA Local Phase")
      dsaContext = dsaContext.copy(local = DSA)
      if checks then {
        DSA.values.foreach(checkUniqueGlobals)
        IntervalDSA.checkReachable(irContext.program, DSA)
        DSA.values.foreach(IntervalDSA.checkUniqueNodesPerRegion)
        DSA.values.foreach(_.localCorrectness())
        DSALogger.info("Performed correctness checks")  
      }
    }
    
    if config.phase == DSAPhase.BU || config.phase == DSAPhase.TD then {
      val DSABU = IntervalDSA.solveBUs(dsaContext.local)
      DSATimer.checkPoint("Finished DSA BU Phase")
      dsaContext = dsaContext.copy(bottomUp = DSABU)
      if checks then {
        DSABU.values.foreach(checkUniqueGlobals)
        DSABU.values.foreach(_.localCorrectness())
        DSALogger.info("Performed correctness check") 
      }
    }

    if config.phase == DSAPhase.TD then {
     val DSATD = IntervalDSA.solveTDs(dsaContext.bottomUp)
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
          globals
        )
      DSATD.values.foreach(g => IntervalDSA.globalTransfer(g, globalGraph))
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
        IntervalDSA.checkReachable(irContext.program, DSATD)
        DSALogger.info("Performed correctness check")
        DSATimer.checkPoint("Finished DSA Invariant Check")
    }

    dsaContext
  }

}

object IntervalDSA {




  def checksGlobalsMaintained(graph: IntervalGraph): Boolean = {
    graph.glIntervals.forall(i => !graph.find(graph.nodes(Global(i))).isCollapsed)
  }

  def checksStackMaintained(graph: IntervalGraph): Boolean = {
    if graph.nodes.contains(Stack(graph.proc)) then
      !graph.find(graph.nodes(Stack(graph.proc))).isCollapsed
    else true
  }

  def globalTransfer(
    source: IntervalGraph,
    target: IntervalGraph,
    oldToNew: mutable.Map[IntervalNode, IntervalNode] = mutable.Map.empty
  ): Map[IntervalNode, IntervalNode] = {
    DSALogger.info(s"cloning globalNode from ${source.proc.procName}")
    source.glIntervals.foreach {
      interval =>
        val targetGlobal = target.find(target.nodes(Global(interval)).get(0))
        var sourceGlobal = source.find(source.nodes(Global(interval)).get(0))
        val globalNode = sourceGlobal.node.clone(target, true, oldToNew)

        sourceGlobal = globalNode.get(sourceGlobal.interval)
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
        assert(cell.node.isUptoDate)
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
              s"hit this case, $base, $off, source: ${source.proc.procName}, Source Expr: $sourceExpr,  target: ${target.proc.procName}, targetExpr: $targetExpr "
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
        assert(!found.contains(base) || found(base) == node, s"$base was in $node and ${found(base)}")
      )
      node.bases.keys.foreach(found.update(_, node))
      seen.add(node)
      val toDo = node.cells.filter(_.hasPointee).map(_.getPointee).map(_.node).filterNot(seen.contains)
      queue.enqueueAll(toDo)
    }
  }

  def checkUniqueGlobals(graph: IntervalGraph): Unit = {
    var found: Map[SymBase, Option[IntervalNode]] = graph.glIntervals.map(i => (Global(i), None)).toMap
    val seen = mutable.Set[IntervalNode]()
    val entry = graph.nodes.values.map(graph.find)
    val queue = mutable.Queue[IntervalNode]().enqueueAll(entry)
    while queue.nonEmpty do {
      val node = queue.dequeue()
      node.bases.filter(_._1.isInstanceOf[Global]).foreach(
        (base, offsets) =>
          assert(found(base).isEmpty || found(base).get == node)
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
  def checkReachable(program: Program, DSA: Map[Procedure, IntervalGraph]): Unit = {
    val reachable = computeDomain(IntraProcIRCursor, program.procedures)
    for (pos <- reachable) {
      val proc = IRWalk.procedure(pos)
      if DSA.contains(proc) then
        val dsg = DSA(proc)
        pos match
          case load: MemoryLoad =>
            val pointers = dsg.exprToCells(load.index).map(dsg.find)
            assert(
              pointers.nonEmpty || isNonGlobalConstant(load.index, dsg.isGlobal),
              "Expected cells for indices used in reachable memory access to have corresponding DSA cells"
            )
            assert(
              pointers.filter(_.hasPointee).map(_.getPointee).map(dsg.get).size <= 1,
              s"Expected index cells to have unified pointer}"
            )
            assert(
              !pointers.exists(_.hasPointee) || pointers.forall(_.hasPointee),
              "expected all/none of the pointers to have pointer"
            )
          case store: MemoryStore =>
            val pointers = dsg.exprToCells(store.index).map(dsg.find)
            assert(
              pointers.nonEmpty,
              s"Expected cells for indices used in reachable memory access to have corresponding DSA cells"
            )
            assert(
              pointers.filter(_.hasPointee).map(_.getPointee).map(dsg.get).size <= 1,
              s"Expected index cells to have unified pointer"
            )
            assert(
              !pointers.exists(_.hasPointee) || pointers.forall(_.hasPointee),
              "expected all/none of the pointers to have pointer"
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
    val unifiedRegions = global.glIntervals.map(Global.apply).map(base => global.find(global.nodes(base)).bases)
    DSA
      .filterNot((proc, _) => proc.procName == "indirect_call_launchpad")
      .foreach((p, graph) =>
        val graphRegions = graph.glIntervals.map(Global.apply).map(base => graph.find(graph.nodes(base)).bases)
        assert(
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
    globals: Seq[DSInterval]
  ): IntervalGraph = {
    val graph = IntervalGraph(proc, Local, context, symValues, cons, globals, None)
    graph.localPhase()
    graph
  }

  def getLocals(
    ctx: IRContext,
    svas: Map[Procedure, SymValues[OSet]],
    cons: Map[Procedure, Set[Constraint]],
    globals: Seq[DSInterval]
  ): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing local DSA")
    computeDSADomain(ctx.program.mainProcedure, ctx).toSeq
      .sortBy(_.name)
      .foldLeft(Map[Procedure, IntervalGraph]())((m, proc) =>
        m + (proc -> IntervalDSA.getLocal(proc, ctx, svas(proc), cons(proc), globals))
      )
  }

  def solveBUs(locals: Map[Procedure, IntervalGraph]): Map[Procedure, IntervalGraph] = {

    DSALogger.info("Performing DSA BU phase")
    val bus = locals.view.mapValues(_.clone).toMap
    DSALogger.info("performed cloning")
    val visited: mutable.Set[Procedure] = mutable.Set.empty
    val queue = mutable.Queue[Procedure]().enqueueAll(bus.keys.toSeq.sortBy(p => p.name))

    // TODO instead of skipping merge the scc and use it directly
    var skip = Seq.empty
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
    bus
  }

  def solveTDs(bus: Map[Procedure, IntervalGraph]): Map[Procedure, IntervalGraph] = {
    DSALogger.info("Performing DSA TD phase")
    val tds = bus.view.mapValues(_.clone).toMap
    val visited: mutable.Set[Procedure] = mutable.Set.empty
    val queue = mutable.Queue[Procedure]().enqueueAll(tds.keys.toSeq.sortBy(p => p.name))

    // TODO instead of skipping merge the scc and use it directly
    var skip = Seq.empty
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
    tds
  }
}

enum DSAPhase {
  case Pre, Local, BU, TD
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
        bv2SignedInt(arg2).toInt * -1
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
    if !intervals.exists(_.contains(address.toInt)) then
      intervals += DSInterval.Value(address.toInt, address.toInt + 8)
    if !intervals.exists(_.contains(relocated.toInt)) then
      intervals += DSInterval.Value(relocated.toInt, relocated.toInt)
  }

  ctx.externalFunctions.foreach(e =>
    if !intervals.exists(_.contains(e.offset.toInt)) then
      intervals += DSInterval.Value(e.offset.toInt, e.offset.toInt)
  )

  val seq = intervals.toSeq.sorted // sorted for easier overlapping check
  if seq.size > 1 then
    {
       seq.sliding(2).foreach(
        v =>
          if (v(0).isOverlapping(v(1))) then
            v(0).isOverlapping(v(1))
            print("")
          assert(!v(0).isOverlapping(v(1)))
      )
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
    if newBase.size > 1 then
      newBase.foreach(_.contains(literal))
    assert(newBase.size <= 1, s"expected to match at most one global but got, $literal,  $newBase")
    if newBase.isEmpty then None
    else {
      val globalInterval = newBase.head
      val newOffset = literal - globalInterval.start.get
      Some(globalInterval, newOffset)
    }
  }
}
