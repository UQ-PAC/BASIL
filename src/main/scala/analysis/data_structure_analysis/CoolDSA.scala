package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.{CoolDSAUnionFindSolver, DSACoolUniTerm, DSAUnionFindSolver}
import analysis.{Analysis, FlatElement}
import ir.*

import scala.collection.mutable
import scala.language.postfixOps

enum DSAPhase {
  case Local, BU, TD
}

object CoolNodeCounter {
  var counter = 0
  def getCounter: Int = {
    counter += 1
    counter
  }

  def reset(): Unit =
    {
      counter = 0
    }
}

class CoolGraph(val proc: Procedure, val phase: DSAPhase = Local, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])
{

  val solver: CoolDSAUnionFindSolver = CoolDSAUnionFindSolver()

  val sva = SVA(proc, constProp)
  val constraints: Set[Constraint] = computeConstraints()
  var symBases: Map[SymBase, CoolNode] = constraints.flatMap(c => List(c.arg1.SSAVar, c.arg2.SSAVar)).foldLeft(Map[SymBase, CoolNode]()) {
    (m, symValSet) =>
      var res = m
      for ((base: SymBase, valSet: Option[Set[BitVecLiteral]]) <- symValSet) {
        val node = res.getOrElse(base, CoolNode(this, Set(base)))
        valSet match
          case Some(vs) =>
            vs.foreach(f => node.addCell(f.value.toInt))
          case None =>
            node.collapse()
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
        constraints += DereferenceConstraint(EV(sva.exprToSymValSet(load, lhs)), EEV(sva.exprToSymValSet(load, index)), size / 8)
      case store@MemoryStore(_, index, value, _, size, _) =>
        constraints += DereferenceConstraint(EV(sva.exprToSymValSet(store, value)), EEV(sva.exprToSymValSet(store, index)), size / 8)
      case _ =>
    }
    constraints
  }

  private def getCells(symValSet: Map[SymBase, Option[Set[BitVecLiteral]]]): Set[CoolCell] =
  {
    symValSet.foldLeft(Set[CoolCell]()) {
      (s, m) =>
        m match
          case (base: SymBase, valSet: Option[Set[BitVecLiteral]]) =>
            s ++ getCells(base, valSet)
    }
    Set.empty
  }

  private def getCells(base: SymBase, offset: Option[Set[BitVecLiteral]]): Set[CoolCell] =
  {
    val node = symBases(base)
    offset match
      case Some(value) =>
        value.map(_.value.toInt).map(node.getCell)
      case None =>
        assert(node.isCollapsed)
        Set(node.getCell(0))
  }


  private def processConstraint(constraint: Constraint): Unit =
  {
    constraint match
      case DereferenceConstraint(arg1: EV, arg2: EEV, size: Int) =>
        val valueCell: CoolCell = mergeCells(getCells(arg1.SSAVar))
        val pointerCells = getCells(arg2.SSAVar)

      case _ => ???
  }

  def mergePointees(pointerCells: Set[CoolCell]): CoolCell =
    {
      mergeCells(pointerCells.map(_.getPointee))
    }

  def mergeCells(cells: Set[CoolCell]): CoolCell =
  {
    cells.tail.foldLeft(cells.head) {
      (res, cell) =>
        mergeCells(res, cell)
    }
  }

  def mergeCells(cell1: CoolCell, cell2: CoolCell): CoolCell =
  {
    ???
  }

  def find(node: CoolNode): CoolField = {
    val (n, offset) = solver.findWithOffset(node.term)
    val resultNode = n.node
    CoolField(resultNode, offset)
  }

  /**
   * wrapper for find functionality of the union-find
   *
   * @param cell the cell to perform find on
   * @return the input cell's equivalent cell in the parent
   */
  def find(cell: CoolCell): CoolCell = {
    val node = cell.node
    val parent: CoolField = find(node)
    parent.node.getCell(cell.offset + parent.offset)
  }
}

class CoolNode(val graph: CoolGraph, val symBases: Set[SymBase], private var _size: Int = 0, val id: Int = CoolNodeCounter.getCounter) {

  val term: DSACoolUniTerm = DSACoolUniTerm(this)
  private var collapsed = false

  def isCollapsed: Boolean = collapsed

  def collapse(): Unit =
  {
    collapsed = true
  }


  def getCell(offset: Int): CoolCell = {
    if collapsed then cells(0) else cells(offset)
  }

  def addCell(offset: Int): CoolCell = {
    if collapsed then cells(0) else cells.getOrElseUpdate(offset, new CoolCell(this, offset))
  }

  def size(): Int = {
    _size
  }

  private val cells: mutable.Map[Int, CoolCell] = mutable.Map(0 -> CoolCell(this, 0))
}

class CoolCell(val node: CoolNode, val offset: Int)
{
  private val graph: CoolGraph = node.graph
  private var _pointee: Option[CoolCell] = None

  def getPointee: CoolCell =
    {
      if _pointee.isEmpty then _pointee = Some(CoolNode(graph, Set.empty).getCell(0))
      _pointee.get
    }

  def hasPointee: Boolean = _pointee.isEmpty

  def setPointee(cell: CoolCell): CoolCell =
    {
      if _pointee.isEmpty then
        _pointee = Some(cell)
      else if graph.find(cell) != graph.find(_pointee.get) then
        graph.mergeCells(cell, _pointee.get)
      _pointee.get
    }
}

class CoolDSA(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends Analysis[Map[Procedure, Graph]]
{

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

  override def analyze(): Map[Procedure, Graph] =
  {

    val result: mutable.Map[DSAPhase, Map[Procedure, CoolGraph]] = mutable.Map.empty
    result.update(Local, domain.foldLeft(Map[Procedure, CoolGraph]()) {
      (m, proc) => m + (proc -> CoolGraph(proc, Local, constProp))
    })

    Map.empty
  }
}


// a node, offset pair, difference to a cell is that it doesn't represent a DSG construct,
case class CoolField(node: CoolNode, offset: Int)
