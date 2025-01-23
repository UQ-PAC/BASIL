package analysis.data_structure_analysis

import ir.{Expr, Procedure}

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
  case Local, BU, TD
}


case class Interval(start: Int, end: Int) {
  require(start <= end)

  def size: Int = end - start
  def isEmpty: Boolean = this.size == 0
  def contains(offset: Int): Boolean = start <= offset && end > offset
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

trait DSA {}

trait DSAGraph[Merged, Cell <: DSACell, Node <: DSANode[Merged, _, Cell, Node]](val proc: Procedure, val phase: DSAPhase) {
  val sva: SymbolicValues = getSymbolicValues(proc)
  val constraints: Set[Constraint] = generateConstraints(proc)
  val nodes: Map[SymBase, Node]
  def exprToSymVal(expr: Expr): SymValueSet = sva.exprToSymValSet(expr)
  def constraintArgToCells(constraintArg: ConstraintArg): Set[Cell]
  def symValToCells(symVal: SymValueSet): Seq[Cell]
  protected def processConstraint(constraint: Constraint): Unit

  // takes a map from symbolic bases to nodes and updates it based on symVal
  protected def symValToNodes(symVal: SymValueSet, current: Map[SymBase, Node]): Map[SymBase, Node]

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

  def mergeCells(cell1: Cell, cell2: Cell): Merged
  def mergeCells[T <: Cell](cells: Iterable[T]): Merged
  def find(cell: Cell): Merged
}

trait DSANode[Merged, CCell <: NodeCell, Cell <: DSACell, Node <: DSANode[Merged, CCell, Cell, Node]]() {

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval)
      val overlapping = false
      intervals.exists(interval1 =>
        intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2)))
  }
  
  def collapsed: Option[CCell]
  def cells: Seq[CCell]
  def graph: DSAGraph[Merged, Cell, Node]
  def isCollapsed: Boolean = collapsed.nonEmpty
  def collapse(): CCell
  def add(offset: Int): CCell = {
    if !isCollapsed then
      add(Interval(offset, offset))
    else
      collapsed.get
  }
  def add(interval: Interval): CCell 
  
  def get(offset: Int): CCell = {
    val exactMatch = cells.filter(_.interval.contains(offset))
    assert(exactMatch.size == 1, "Expected  exactly one interval to contain the offset")
    exactMatch.head
  }

  def get(interval: Interval): CCell = {
    val exactMatches = cells.filter(_.interval.isOverlapping(interval))
    assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
    assert(exactMatches.head.interval == interval, "")
    exactMatches.head
  }

  def growCell(interval: Interval): CCell = {
    add(interval)
  }

}

trait DSACell