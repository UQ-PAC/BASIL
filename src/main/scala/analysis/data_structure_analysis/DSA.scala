package analysis.data_structure_analysis

import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure}

import scala.collection.{SortedSet, mutable}

trait Counter(val init: Int = 0) {
  private var counter = init
  def increment(by: Int = 1): Int = {
    counter += by
    counter
  }

  def decrement(by: Int = 1): Int = {
    counter -= by
    counter
  }

  def get: Int = counter

  def reset(): Unit = counter = init
}

enum DSAPhase {
  case Pre, Local, BU, TD
}


case class Interval(start: Int, end: Int) {
  require(start <= end)

  override def toString: String = s"$start-$end" 
  def size: Int = end - start
  def move(func: Int => Int): Interval = Interval(func(start), func(end))
  def isEmpty: Boolean = this.size == 0
  def contains(offset: Int): Boolean = start <= offset && end >= offset
  def contains(interval: Interval): Boolean =
    start <= interval.start && end >= interval.end
  def isOverlapping(other: Interval): Boolean = !(start > other.end || other.start > end)
  def join(other: Interval): Interval = {
    require(isOverlapping(other), "Expected overlapping Interval for a join")
    Interval(math.min(start, other.start), math.max(end, other.end))
  }
}

object Interval {
  def join(interval1: Interval, interval2: Interval): Interval = interval1.join(interval2)

  implicit def orderingByTuple[T <: Interval]: Ordering[T] =
    Ordering.by(i => (i.start, i.end))
}

trait DSAGraph[Solver, Merged, Cell <: NodeCell & DSACell, CCell <: DSACell, Node <: DSANode[Cell]](val proc: Procedure, val phase: DSAPhase, val solver: Solver) {
  val sva: SymbolicValues = getSymbolicValues(proc)
  val constraints: Set[Constraint] = generateConstraints(proc)
  val nodes: Map[SymBase, Node] = buildNodes
  def exprToSymVal(expr: Expr): SymValueSet = sva.exprToSymValSet(expr)
  def init(symBase: SymBase, size: Option[Int]): Node
  def constraintArgToCells(constraintArg: ConstraintArg): Set[CCell]

  def localPhase(): Unit = {
    constraints.foreach(processConstraint)
  }

  def symValToCells(symVal: SymValueSet): Set[Cell] = {
    val pairs = symVal.state
    pairs.foldLeft(Set[Cell]()) {
      case (results, (base: SymBase, offsets: SymOffsets)) =>
        val node = nodes(base)
        if offsets.isTop then
          results + node.collapse()
        else
          results ++ offsets.getOffsets.map(node.get)
    }
  }

  protected def processConstraint(constraint: Constraint): Unit

  // takes a map from symbolic bases to nodes and updates it based on symVal
  protected def symValToNodes(symVal: SymValueSet, current: Map[SymBase, Node]): Map[SymBase, Node] = {
    symVal.state.foldLeft(current) {
      case (result, (base, symOffsets)) =>
        val node = result.getOrElse(base, init(base, None))
        if symOffsets.isTop then node.collapse()
        else
          symOffsets.getOffsets.map(node.add)
        result + (base -> node)
    }
  }


    // takes a map from symbolic bases to nodes and updates it based on constraint
  protected def binaryConstraintToNodes(constraint: BinaryConstraint, nodes: Map[SymBase, Node]): Map[SymBase, Node] = {
    val arg1 = exprToSymVal(constraint.arg1.value)
    val arg2 = exprToSymVal(constraint.arg2.value)
    val res = symValToNodes(arg1, nodes)
    symValToNodes(arg2, res)
  }

  def buildNodes: Map[SymBase, Node] = {
    constraints.foldLeft(Map[SymBase, Node]()) {
      case (resultMap, constraint) => constraint match
        case constraint: BinaryConstraint => binaryConstraintToNodes(constraint, resultMap)
        case dcc @ DirectCallConstraint(call) =>
          (dcc.inConstraints ++ dcc.outConstraints).foldLeft(resultMap){case (updated, constraint) => binaryConstraintToNodes(constraint, updated)}
        case _ => resultMap
    }
  }

  def mergeCells(cell1: CCell, cell2: CCell): Merged
  def mergeCells(cells: Iterable[Cell]): Merged
  def find(cell: CCell): Merged
}

trait DSANode[Cell <: NodeCell & DSACell](val size: Option[Int]) {

  def init(interval: Interval): Cell
  def graph: DSAGraph[_, _, Cell, _, _]
  protected var _cells: Seq[Cell] = Seq.empty
  def cells: Seq[Cell] = _cells
  protected var _collapsed: Option[Cell] = None
  def collapsed: Option[Cell] = _collapsed

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval)
      val overlapping = false
      intervals.exists(interval1 =>
        intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2)))
  }


  def isCollapsed: Boolean = collapsed.nonEmpty
  def add(offset: Int): Cell = {
    if !isCollapsed then
      add(Interval(offset, offset))
    else
      collapsed.get
  }

  def get(offset: Int): Cell = {
    if isCollapsed then collapsed.get else
      val exactMatch = cells.filter(_.interval.contains(offset))
      assert(exactMatch.size == 1, "Expected  exactly one interval to contain the offset")
      exactMatch.head
  }

  def get(interval: Interval): Cell = {
    if isCollapsed then collapsed.get else
      val exactMatches = cells.filter(_.interval.contains(interval))
      assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
      assert(exactMatches.head.interval == interval, "")
      exactMatches.head
  }

  def growCell(interval: Interval): Cell = {
    add(interval)
  }

  def add(interval: Interval): Cell = {
    if !isCollapsed then
      val overlapping: Seq[Cell] = cells.filter(_.interval.isOverlapping(interval))
//      _cells = cells.diff(overlapping)

      val newCell = if overlapping.isEmpty then
        init(interval)
      else
        val unifiedInterval = overlapping.map(_.interval).reduce(Interval.join)
        val res = init(unifiedInterval)
        graph.mergeCells(overlapping.appended(res))
        res

      _cells = cells.diff(overlapping).appended(newCell).sorted
      newCell
    else
      collapsed.get
  }

  def collapse(): Cell = {
    if !isCollapsed then

      val collapsedCell = add(Interval(0, 0))
      graph.mergeCells(cells.appended(collapsedCell))
      _collapsed = Some(collapsedCell)
    collapsed.get
  }

}


trait DSACell