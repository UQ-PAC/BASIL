package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.UnionFindSolver
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{Expr, InterProcIRCursor, LocalVar, Procedure, Program, computeDomain}

import scala.collection.mutable.ArrayBuffer

enum DSAPhase {
  case Local, BU, TD
}


trait DSA {

}

trait DSAGraph(val proc: Procedure, val phase: DSAPhase) {
  val sva: Map[LocalVar, SymValueSet] = getSymbolicValues(proc)
  val constraints: Set[Constraint] = generateConstraints(proc)
  val nodes: Map[SymBase, DSANode]
  def exprToSymVal(expr: Expr): SymValueSet = SymbolicValueDomain.exprToSymValSet(expr, sva)
  def constraintArgToCells(constraintArg: ConstraintArg): Seq[DSACell]
  def symValToCells(symVal: SymValueSet): Seq[DSACell]
  
}
trait DSANode {

}

trait DSACell {

}

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

object SuperCellCounter extends Counter

case class FieldTerm(v: SuperCell) extends analysis.solvers.Var[FieldTerm]

class FieldGraph(proc: Procedure, phase: DSAPhase) extends DSAGraph(proc, phase) {

  val solver: UnionFindSolver[FieldTerm] = UnionFindSolver[FieldTerm]()
  override val nodes: Map[SymBase, FieldNode] = buildNodes

  def localPhase(): Unit = {
    constraints.foreach(processConstraint)
  }

  def symValToCells(symVal: SymValueSet): Seq[FieldCell] = {
    val pairs = symVal.map
    pairs.foldLeft(Seq[FieldCell]()) {
      case (results, (base: SymBase, offsets: SymOffsets)) =>
        val node = nodes(base)
        if offsets.isTop then
          node.collapse()
          results.appended(node.get(0))
        else
          results ++ offsets.getOffsets.map(node.get)
    }.sorted
  }

  def constraintArgToCells(constraintArg: ConstraintArg): Seq[ConstraintCell] = {
    val exprCells = symValToCells(exprToSymVal(constraintArg.value))
    if constraintArg.contents then
      exprCells.map(_.content)
    else
      exprCells
  }

  // takes a map from symbolic bases to nodes and updates it based on symVal
  private def symValToNodes(symVal: SymValueSet, current: Map[SymBase, FieldNode]): Map[SymBase, FieldNode]  = {
    symVal.map.foldLeft(current) {
      case (result, (base, symOffsets)) =>
        val node = result.getOrElse(base, FieldNode(this, base, None))
        if symOffsets.isTop then node.collapse()
        else
          symOffsets.getOffsets.map(node.add)
        result + (base -> node)
    }
  }

 // takes a map from symbolic bases to nodes and updates it based on constraint
  private def binaryConstraintToNodes(constraint: BinaryConstraint, nodes: Map[SymBase, FieldNode]): Map[SymBase, FieldNode] = {
    val arg1 = exprToSymVal(constraint.arg1.value)
    val arg2 = exprToSymVal(constraint.arg2.value)
    val res = symValToNodes(arg1, nodes)
    symValToNodes(arg2, res)
  }

  def buildNodes: Map[SymBase, FieldNode] = {
    constraints.foldLeft(Map[SymBase, FieldNode]()) {
      case (resultMap, constraint) => constraint match
        case constraint: BinaryConstraint => binaryConstraintToNodes(constraint, resultMap)
        case dcc @ DirectCallConstraint(call) =>
          (dcc.inConstraints ++ dcc.outConstraints).foldLeft(resultMap){case (updated, constraint) => binaryConstraintToNodes(constraint, updated)}
        case _ => resultMap
    }
  }

  private def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case cons: BinaryConstraint =>
        val first = mergeCells(constraintArgToCells(cons.arg1))
        val sec = mergeCells(constraintArgToCells(cons.arg2))
        mergeCells(first, sec)
      case dcc: DirectCallConstraint =>
        (dcc.inConstraints ++ dcc.outConstraints).foreach(processConstraint)
      case idcc: IndirectCallConstraint => // ignore
  }

  private def find(cell: SuperCell): SuperCell = {
    solver.find(cell.term).asInstanceOf[FieldTerm].v
  }

  def find(cell: ConstraintCell): SuperCell = {
    find(cell.sc)
  }

  def mergeCells[T <: ConstraintCell](cells: Iterable[T]): SuperCell = {
    require(cells.nonEmpty, "can't merge no cells")
    mergeCells(cells.map(_.sc))
  }

  def mergeCells(cell1: ConstraintCell, cell2: ConstraintCell): SuperCell = {
    mergeCells(cell1.sc, cell2.sc)
  }

  private def mergeCells(cell1: SuperCell, cell2: SuperCell): SuperCell = {
    val newCell1 = find(cell1)
    val newCell2 = find(cell2)

    if newCell1 != newCell2 then //
      val res = SuperCell(newCell1.members.union(newCell2.members))
      solver.unify(newCell1.term, res.term)
      solver.unify(newCell2.term, res.term)

      val pointees: Set[ConstraintCell] = res.pointers.map(f => f.content)
      if pointees.nonEmpty then
        mergeCells(pointees)
      res
    else // already unified
      newCell1
  }

  private def mergeCells(cells: Iterable[SuperCell])(implicit i1: DummyImplicit): SuperCell = {
    require(cells.nonEmpty, "can't merge no cells")
    if cells.size > 1 then
      cells.tail.foldLeft(cells.head) {
        (res, cell) =>
          mergeCells(res, cell)
      }
    else
      cells.head
  }

  def toDot: String = {
    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    var seenCells: Set[Int] = Set.empty
    var pointTos: Set[(Int, Int)] = Set.empty

    val cells = nodes.values.flatMap(_.cells)
    val superCells = cells.flatMap(f => Set(f.sc, f.content.sc)).map(find)

    superCells.foreach(
      f =>
        if !seenCells.contains(f.id) then
          seenCells += f.id
          structs.append(DotStruct(f.id.toString, f.toString, None))
    )

    cells.foreach(
      f =>
        val pointer = find(f.sc)
        val pointee = find(f.content.sc)
        if !pointTos.contains((pointer.id, pointee.id)) then
          pointTos += (pointer.id, pointee.id)
          arrows.append(StructArrow(DotStructElement(pointer.id.toString, None), DotStructElement(pointee.id.toString, None)))
    )

    StructDotGraph(proc.name, structs, arrows).toDotString
  }
}


class FieldNode(val graph: FieldGraph, val base: SymBase, val size: Option[Int]) extends DSANode {

  var cells: Set[FieldCell] = Set.empty
  private var collapsed: Option[FieldCell] = None
  add(0)

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval)
      val overlapping = false
      intervals.exists(interval1 =>
        intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2)))
  }

  def add(interval: Interval): FieldCell = {
    val overlapping: Set[FieldCell] = cells.filter(_.interval.isOverlapping(interval))
    cells = cells -- overlapping

    val newCell = if overlapping.isEmpty then
      FieldCell(this, interval)
    else
      val unifiedInterval = overlapping.map(_.interval).reduce(Interval.join)
      val res = FieldCell(this, unifiedInterval)
      graph.mergeCells(overlapping + res)
      res

    cells += newCell
    newCell

  }

  def add(offset: Int): FieldCell = {
    if !isCollapsed then
      add(Interval(offset, offset))
    else
      collapsed.get
  }

  def get(offset: Int): FieldCell = {
    val exactMatch = cells.filter(_.interval.contains(offset))
    assert(exactMatch.size == 1, "Expected  exactly one interval to contain the offset")
    exactMatch.head
  }

  def get(interval: Interval): FieldCell = {
    val exactMatches = cells.filter(_.interval.isOverlapping(interval))
    assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
    assert(exactMatches.head.interval == interval, "")
    exactMatches.head
  }

  def collapse(): FieldCell = {
    if !isCollapsed then
      val collapsedCell = FieldCell(this, Interval(0,0))
      graph.mergeCells(cells + collapsedCell)
      collapsed = Some(collapsedCell)
    collapsed.get
  }

  def isCollapsed: Boolean = collapsed.nonEmpty
  def growCell(interval: Interval): FieldCell = {
    add(interval)
  }
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
}

sealed trait ConstraintCell extends DSACell {
  val sc = SuperCell(Set(this))
}

case class FieldCell(node: FieldNode, interval: Interval) extends ConstraintCell  {
  val content: ContentCell = ContentCell(this)
  override def toString: String = s"Cell($node, $interval)"
}

object FieldCell {
  implicit def orderingByInterval[A <: FieldCell]: Ordering[A] =
    Ordering.by(e => (e.interval.start, e.interval.end))
}

case class ContentCell(cell: FieldCell) extends ConstraintCell {
  override def toString: String = s"[|${cell.toString}|]"
}


case class SuperCell(members: Set[ConstraintCell], id: Int = SuperCellCounter.increment()) {
  def pointers: Set[FieldCell] = members.collect {case cell: FieldCell => cell}

  val term: FieldTerm = FieldTerm(this)

  override def toString: String = s"SuperCell($id, $members)"
}




class FieldDSA(program: Program) {
  val domain: Set[Procedure] = computeDomain(InterProcIRCursor, Set(program.mainProcedure))
    .collect {case proc: Procedure => proc}.toSet


}

object FieldDSA {
  def getLocal(proc: Procedure): FieldGraph = {
    val graph = FieldGraph(proc, Local)
    graph.localPhase()
    graph
  }
}

/*
trait DSAGraph(val proc: Procedure, val phase: DSAPhase) {
  val sva: Map[LocalVar, SymValueSet] = getSymbolicValues(proc)
  val constraints: Set[Constraint] = generateConstraints(proc)
  def exprToSymVal(expr: Expr): SymValueSet = SymbolicValueDomain.exprToSymValSet(expr, sva)
  val solver: UnionFindSolver[DSATerm] = UnionFindSolver[DSATerm]()
  def constraintArgToCell(constraintArg: ConstraintArg)
  def buildNodes: Map[SymBase, DSANode]
  def mergeCells(cell1: DSACell, cell2: DSACell): DSACell
  def mergeCells(cells: Set[DSACell]): DSACell = {
    require(cells.nonEmpty, "can't merge no cells")
    if cells.size > 1 then
      cells.tail.foldLeft(cells.head) {
        (res, cell) =>
          mergeCells(res, cell)
      }
    else
      cells.head
  }
  def collect: (Set[DSANode], Map[DSACell, DSACell])
  def toDot: String
}

trait DSANode(val graph: DSAGraph, val base: SymBase, val size: Option[Int]) {

  var cells: Set[DSACell]

  def nonOverlappingProperty: Boolean = {
    if cells.size <= 1 then true
    else
      val intervals = cells.map(_.interval)
      val overlapping = false
      intervals.exists(interval1 => intervals.exists(interval2 => interval1 != interval2 && interval1.isOverlapping(interval2)))
  }

  def add(interval: Interval): DSACell
  def add(offset: Int): DSACell

  def get(offset: Int): DSACell = {
    val exactMatch = cells.filter(_.interval.contains(offset))
    assert(exactMatch.size == 1, "Expected  exactly one interval to contain the offset")
    exactMatch.head
  }

  def get(interval: Interval): DSACell = {
    val exactMatches = cells.filter(_.interval.isOverlapping(interval))
    assert(exactMatches.size == 1, "Expected exactly one overlapping interval")
    assert(exactMatches.head.interval == interval, "")
    exactMatches.head
  }

  def growCell(interval: Interval): DSACell = {
    add(interval)
  }
}

trait DSACell(val DSANode: DSANode, val interval: Interval) {
  def getPointee: DSACell
  def getSize: Int = interval.size
}

class FieldGraph(proc: Procedure, phase: DSAPhase = Local) extends DSAGraph(proc, phase) {


  override def buildNodes: Map[SymBase, FieldNode] =  constraints.foldLeft(Map[SymBase, FieldNode]()) {
    case (map, constraint) =>
      constraint match
        case AssignmentConstraint(pos, ar1, ar2) => ???

        case MemoryReadConstraint(pos) => ???
        case MemoryWriteConstraint(pos) => ???
        case DirectCallConstraint(call) => ???
        case IndirectCallConstraint(call) => ???
        case _ => ???
  }


  override def mergeCells(cell1: DSACell, cell2: DSACell): DSACell = ???

  override def collect: (Set[DSANode], Map[DSACell, DSACell]) = ???

  override def toDot: String = ???

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

class FieldNode(graph: FieldGraph, base: SymBase, size: Option[Int]) extends DSANode(graph, base, size) {

  var cells: Set[DSACell] = ???

  override def add(interval: Interval): DSACell = ???

  override def add(offset: Int): DSACell = ???
}

sealed trait ConstraintCell {
  val sc = SuperCell(Set(this))
}

case class FieldCell(node: FieldNode, override val interval: Interval) extends DSACell(node, interval), ConstraintCell  {
  val content: ContentCell = ContentCell(this)
  override def toString: String = s"Cell($node, $interval)"

  override def getPointee: DSACell = ???
}

case class ContentCell(cell: FieldCell) extends ConstraintCell, DSACell(cell.node, cell.interval) {
  override def toString: String = s"[|${cell.toString}|]"

  override def getPointee: DSACell = ???
}
case class SuperCell(members: Set[ConstraintCell])

*/
