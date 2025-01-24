package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.UnionFindSolver
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{Expr, InterProcIRCursor, LocalVar, Procedure, Program, computeDomain}

import scala.collection.mutable.ArrayBuffer


object SuperCellCounter extends Counter

case class FieldTerm(v: SuperCell) extends analysis.solvers.Var[FieldTerm]

class FieldGraph(proc: Procedure, phase: DSAPhase) extends DSAGraph[SuperCell, FieldCell, ConstraintCell, FieldNode](proc, phase) {
  
  override def init(symBase: SymBase, size: Option[Int]): FieldNode = FieldNode(this, symBase, size)
  
  val solver: UnionFindSolver[FieldTerm] = UnionFindSolver[FieldTerm]()
  override val nodes: Map[SymBase, FieldNode] = buildNodes

  def localPhase(): Unit = {
    constraints.foreach(processConstraint)
  }

  override def constraintArgToCells(constraintArg: ConstraintArg): Set[ConstraintCell] = {
    val exprCells = symValToCells(exprToSymVal(constraintArg.value))
    if constraintArg.contents then
      exprCells.map(_.content).toSet
    else
      exprCells.toSet
  }

  override def processConstraint(constraint: Constraint): Unit =
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

  override def mergeCells[T <: ConstraintCell](cells: Iterable[T]): SuperCell = {
    require(cells.nonEmpty, "can't merge no cells")
    mergeCells(cells.map(_.sc))
  }

  override def mergeCells(cell1: ConstraintCell, cell2: ConstraintCell): SuperCell = {
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

    val cells = nodes.values.flatMap(f => f.cells)
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


class FieldNode(val graph: FieldGraph, val base: SymBase, size: Option[Int]) extends DSANode[FieldCell](size) {
//  override def graph: DSAGraph[SuperCell, FieldCell, ConstraintCell, FieldNode] = parent
  override def init(interval: Interval): FieldCell = FieldCell(this, interval)
}

sealed trait ConstraintCell extends DSACell {
  val sc = SuperCell(Set(this))
}

trait NodeCell(val interval: Interval) extends DSACell, ConstraintCell

object NodeCell {
  implicit def orderingByInterval[T <: NodeCell]: Ordering[T] =
    Ordering.by(sc => sc.interval)
}

case class FieldCell(node: FieldNode, override val interval: Interval) extends NodeCell(interval)  {
  val content: ContentCell = ContentCell(this)
  override def toString: String = s"Cell($node, $interval)"
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
