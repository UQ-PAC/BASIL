package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.{Analysis, FlatElement, Loop}
import analysis.solvers.{Term, UnionFindSolver, Var}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.{BitVecLiteral, CFGPosition, IntraProcIRCursor, MemoryLoad, MemoryStore, Procedure, Program, Variable, computeDomain, toDot}
import util.writeToFile

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.breakable

object SuperCellCounter extends Counter

class WinterGraph(val proc: Procedure, val phase: DSAPhase = Local, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) {

  val sva = SVA(proc, constProp, loops)
  sva.analyze()
  val solver = UnionFindSolver[UnifTerm]()
  val constraints: Set[Constraint] = computeConstraints()
  val baseToNode: Map[SymBase, WinterNode] = constraints.flatMap(c => List(c.arg1.SSAVar, c.arg2.SSAVar)).foldLeft(Map[SymBase, WinterNode]()) {
    (m, symValSet) =>
      var res = m
      for ((base: SymBase, valSet: Option[Set[Int]]) <- symValSet) {

        val node: WinterNode = res.getOrElse(base, WinterNode(this, base))
        valSet match
          case Some(vs) =>
            vs.foreach(f => node.getCell(Some(f)))
          case None =>
            node.getCell(None)
        res += (base -> node)
      }
      res
  }

  private def computeConstraints(): Set[Constraint] =
  {
    val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, Set(proc)).toSet
    var constraints: Set[Constraint] = Set.empty
    domain.foreach {
      case load@MemoryLoad(lhs, _, index, _, size, _) =>
        constraints += Constraint(load, lhs, index, EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size / 8)
      case store@MemoryStore(_, index, value, _, size, _) =>
        constraints += Constraint(store, value, index, EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size / 8)
      case _ =>
    }
    constraints
  }


  def growCell(base: SymBase, offset: Option[Int], size: Int): Unit = baseToNode(base).growCell(offset, size)
  def growCells(base: SymBase, offsets: Option[Set[Int]], size: Int): Unit = {
    if offsets.isEmpty then
      getCell(base, None)  // causes collapse if not already collapsed, other wise the size doesn't matter
    else
      offsets.get.foreach {
        case offset: Int => growCell(base, Some(offset), size)
      }
  }
  def growCells(symValSet: Map[SymBase, Option[Set[Int]]], size: Int): Unit = {
    symValSet.foreach(f=> growCells(f._1, f._2, size))
  }

  def getCell(base: SymBase, offset: Option[Int]): WinterCell =
    baseToNode(base).getCell(offset)
  def getCells(base: SymBase, offsets: Option[Set[Int]]): Set[WinterCell] = {
    if offsets.isEmpty then
      Set(getCell(base, None))
    else
      offsets.get.foldLeft(Set[WinterCell]()) {
        (s, offset) => s + getCell(base, Some(offset))
      }
  }
  def getCells(symValSet: Map[SymBase, Option[Set[Int]]]): Set[WinterCell] = {
    symValSet.foldLeft(Set[WinterCell]()) {
      (s, mapping) =>
        val (base, valueSet) = mapping
        s ++ getCells(base, valueSet)
    }
  }

  def toDot: String = {
    val structs = ArrayBuffer[DotStruct]()
    val arrows = ArrayBuffer[StructArrow]()

    var seenCells: Set[Int] = Set.empty
    var pointTos: Set[(Int, Int)] = Set.empty

    val cells =  baseToNode.values.flatMap(_.cells.values.toSet)
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


  def localPhase(): WinterGraph =
  {
    constraints.toSeq.sortBy(f => f.id).foreach(processConstraint) //  sorting only for debugging
    this
  }

  private def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case Constraint(pos, value, index, arg1: EV, arg2: EEV, size: Int, id: Int) =>
        //        if (getCells(arg1.SSAVar).isEmpty) then println(pos)
        //        if (getCells(arg2.SSAVar).isEmpty) then println(pos)
        val valueCells: Set[ConstraintCell] = getCells(arg1.SSAVar).asInstanceOf[Set[ConstraintCell]]
        val pointerCells: Set[WinterCell] = getCells(arg2.SSAVar)
        val pointeeCells: Set[ConstraintCell] = pointerCells.map(f => f.content)

        unify(valueCells)
        unify(pointerCells.asInstanceOf[Set[ConstraintCell]]) // TODO pointee cells should be unified (check)
        unify(valueCells.head, pointeeCells.head)

      case _ => ???
  }

  private def find(cell: SuperCell): SuperCell = {
    solver.find(cell.term).asInstanceOf[DSAUnifTerm].cell
  }

  def find(cell: ConstraintCell): SuperCell = {
    find(cell.sc)
  }

  def unify(cells: Set[ConstraintCell]): SuperCell = {
    require(cells.nonEmpty)
    unify(cells.map(_.sc))
  }

  def unify(cell1: ConstraintCell, cell2: ConstraintCell): SuperCell = {
    unify(cell1.sc, cell2.sc)
  }

  private def unify(cells: Set[SuperCell])(implicit i1: DummyImplicit): SuperCell = {
    require(cells.nonEmpty)

    val head = cells.head
    val tail = cells.tail

    tail.foldLeft(head) {
      (res, cell) => unify(res, cell)
    }
  }

  private def unify(cell1: SuperCell, cell2: SuperCell): SuperCell = {
    val newCell1 = find(cell1)
    val newCell2 = find(cell2)

    if newCell1 != newCell2 then //
      val res =  SuperCell(newCell1.members.union(newCell2.members))
      solver.unify(newCell1.term, res.term)
      solver.unify(newCell2.term, res.term)


      val pointees: Set[ConstraintCell] = res.pointers.map(f => f.content)
      if pointees.nonEmpty then
        unify(pointees)

      res
    else // already unified
      newCell1
  }
}

case class Interval(start: Int, end: Int) {
  require(start <= end)

  def size: Int = end - start
  def isSingleton: Boolean = this.size == 0
  def contains(offset: Int): Boolean = start <= offset && offset < end

  // returns non if not overlapping
  // otherwise returns the join of two intervals
  def overlap(other: Interval): Option[Interval] = {
    if other.start >= this.end || this.start >= other.end then
      None
    else
      Some(Interval(math.min(other.start, this.start), math.max(other.start, this.start)))
  }

  override def toString: String = s"Interval($start:$end)"
}

class WinterNode(val graph: WinterGraph, val symBase: SymBase, var cells: Map[Interval, WinterCell] = Map.empty, val size: Int = 0) {
  nonOverlapping()

  getCell(Some(0)) // create a cell at 0
  private var collapsed = false

  private def nonOverlapping(): Unit = {
    var seen: List[Interval] = List.empty
    breakable {
      cells.keys.foreach(
        interval =>
          assert(!seen.exists(_.overlap(interval).nonEmpty))
          seen = seen.appended(interval)
      )
    }
  }


  def growCell(offset: Option[Int], size: Int): WinterCell = {
    if offset.isEmpty then // no need to grow a cell in a collapsed node
      getCell(offset) // return the cell corresponding to Top (also collapsed if not yet collapsed)
    else
      val (interval, cell) = getOffset(offset.get)
      val newInterval = Interval(interval.start, math.max(offset.get - interval.start + size, interval.end))
      val resultInterval = cells.foldLeft(newInterval) {
        (resultInterval, mapping) =>
          val (interval, intervalCell) = mapping
          val possibleNewInterval = resultInterval.overlap(interval)
          if possibleNewInterval.isEmpty then
           //intervals didn't overlap, therefore, skip
           resultInterval
          else
            graph.unify(cell, intervalCell) // unify cells of overlapping intervals
            possibleNewInterval.get

      }

      cells --= cells.keys.filter(_.overlap(resultInterval).nonEmpty)
      cells += resultInterval -> cell
      cell
  }


  def getOffset(offset: Int): (Interval, WinterCell) = {
    val hits = cells.filter(f => f._1.contains(offset))
    assert(hits.size <= 1)

    if hits.nonEmpty then
      hits.head
    else
      val res = Interval(offset, offset) -> WinterCell(this, offset)
      cells += res
      res
  }

  def getCell(offset: Option[Int]): WinterCell = {
    if offset.isEmpty then
      collapse()

    if collapsed then
      cells.head._2 // return any of the cells in the mapping if collapsed
    else
      val (interval, cell) = getOffset(offset.get)
      cell
  }

  def collapse(): Unit = {
    if !collapsed then
      collapsed = true

      // unify all cells in this
      graph.unify(cells.values.toSet)
  }

  override def toString: String = s"Node($symBase)"

  override def equals(obj: Any): Boolean = {
    obj match
      case node: WinterNode => symBase == node.symBase
      case _ => false
  }

  override def hashCode(): Int = symBase.hashCode()
}

sealed trait ConstraintCell {
  val sc = SuperCell(Set(this))
}

case class Contents(cell: WinterCell) extends ConstraintCell

case class WinterCell(node: WinterNode, offset: Int) extends ConstraintCell {
  val content: Contents = Contents(this)

  override def toString: String = s"Cell($node, $offset)"
}

case class SuperCell(members: Set[ConstraintCell], id : Int = SuperCellCounter.increment()) {
  def pointers: Set[WinterCell] = members.filter(_.isInstanceOf[WinterCell]).map(_.asInstanceOf[WinterCell])
  val term: DSAUnifTerm = DSAUnifTerm(this)

  override def toString: String = s"SuperCell($id, $members)"
}


class WinterDSA(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], loops: Set[Loop]) extends Analysis[Map[Procedure, WinterGraph]] {

  val domain = computeDomain(program)

  private def computeDomain(program: Program): Set[Procedure] =
  {
    var domain: Set[Procedure] = Set(program.mainProcedure)
    val stack: mutable.Stack[Procedure] = mutable.Stack()
    stack.pushAll(program.mainProcedure.calls)

    // calculate the procedures used in the program
    while (stack.nonEmpty) {
      val current = stack.pop()
      domain += current
      stack.pushAll(current.calls.diff(domain))
    }

    domain
  }
  override def analyze(): Map[Procedure, WinterGraph] = {

    val result: mutable.Map[DSAPhase, Map[Procedure, WinterGraph]] = mutable.Map.empty
    result.update(Local, domain.foldLeft(Map[Procedure, WinterGraph]()) {
      (m, proc) => m + (proc -> WinterGraph(proc, Local, constProp, loops).localPhase())
    })

//    writeToFile(result(Local).head._2.toDot, "cooldsa.dot")
//    writeToFile(toDot(program, result(Local).head._2.sva.svaMap.map(f => (f._1, f._2.toString())).toMap), "sva.dot")

    writeToFile(result(Local)(program.mainProcedure).toDot, "eqClasses.dot")

    result(Local)
  }
}


/** Terms used in unification.
 */
sealed trait UnifTerm

/** A term variable in the solver
 */
case class DSAUnifTerm(cell: SuperCell) extends Var[UnifTerm] {

  override def toString: String = s"Term{$cell}"
}


