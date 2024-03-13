package analysis

import com.sun.org.apache.xalan.internal.xsltc.compiler.util.NodeType
import ir.{Expr, Procedure, Variable}

import scala.collection.mutable

// need a type procedure

type Prexpr = Procedure | Expr

/**
 * DSA Graph
 */
class Graph(val procedure: Procedure) {

  val nodes: mutable.Set[Node] = mutable.Set()
  val pointersToCells: mutable.Map[Expr, Cell] = mutable.Map()
  // TODO refactor the one below
  // If cells change i don't think this will work.
  var pointsToRelations: mutable.Map[Cell, Cell] = mutable.Map()

  /**
   *
   * @param node
   * @return Set[(node, offset_i)_pointer, cell_pointee)
   */
  def getPointees(node: Node): Set[(Cell, Cell)] = {
    node.cells.foldLeft(Set(): Set[(Cell, Cell)]) {
      (s, c) => if c.pointee.isDefined then s.+((c, c.pointee.get)) else s
    }
  }

  def getPointers(node: Node): Set[(Cell, Cell)] = {
    pointsToRelations.foldLeft(Set(): Set[(Cell, Cell)]) {
      (s, m) =>
        m match
          case (key, value) =>
            if node.cells.contains(value) then s.+((key, value)) else s
    }
  }

  def pointTo(pointer: Cell, pointee: Option[Cell]): Unit = {
    pointer.pointTo(pointee)
    pointee match
      case Some(value) =>
        pointsToRelations.put(pointer, value)
      case None => pointsToRelations.remove(pointer)
  }


  def makeNode(memoryRegion2: MemoryRegion2): Node = {
    val node = Node(memoryRegion2, this)
    nodes.add(node)
    node
  }

  def makeCell(memoryRegion2: MemoryRegion2): Cell = {
    val node = makeNode(memoryRegion2)
    node.makeCell()
  }

  def unify(variable: Variable, cell: Cell): Unit = {
    if !pointersToCells.contains(variable) then
      pointersToCells.put(variable, cell)
    else
      pointersToCells(variable).unify(cell)
  }

  def collapsePointer(pointer: Variable, memoryRegion2: MemoryRegion2): Unit = {
    val cell = makeCell(memoryRegion2)
    cell.node.get.collapseNode()
    unify(pointer, cell)
  }
}


/**
 * DSA Node represents a memory object
 */
class Node (val memoryRegion2: MemoryRegion2, val owner: Graph) {
  var cells: mutable.Set[Cell] = mutable.Set()
  private val flags: NodeFlags = NodeFlags()
  var size: BigInt = memoryRegion2 match
    case region: HeapRegion2 => region.size.value
    case _ => 0

  def links: Set[BigInt] =
    cells.foldLeft(Set(): Set[BigInt]){
      (s, c) => s + c.offset
    }

  def offsetHelper(offset1: BigInt, offset2: BigInt): BigInt = {
    if isCollapsed then
      0
    else if isSeq then
      (offset1 + offset2) % size
    else
      offset1 + offset2
  }

  def redirectEdges(node: Node, offset: BigInt): Unit = {
    owner.getPointers(this).foreach(
      (pointer, pointee) =>
        val newCell = node.makeCell(node.offsetHelper(offset, pointee.offset))
        owner.pointTo(pointer, Some(newCell))
        owner.pointersToCells.foreach(
          (key, value) =>
            if value.equals(pointee) then owner.pointersToCells.put(key, newCell)
        )
    )

    owner.getPointees(this).foreach(
      (pointer, pointee) =>
        val newCell = node.makeCell(node.offsetHelper(offset, pointer.offset))
        if owner.pointsToRelations.contains(newCell) then
          pointee.unify(owner.pointsToRelations(newCell))
        else
          owner.pointTo(newCell, Some(pointee))
    )

    owner.nodes.remove(this)
    owner.pointsToRelations =  owner.pointsToRelations.filter(
      (key, value) => key.equals(this) && value.equals(this)
    )
  }
  def collapseNode(): Unit = {
    val cell = owner.makeCell(this.memoryRegion2)
    cells.foreach(
      c =>
        cell.unify(c)
        owner.pointTo(c, None)
    )
    size = 1
    flags.collapsed = true
    cells = mutable.Set(cell)
  }

  def collapse(node: Node, offset: BigInt): Unit = {
    node.collapseNode()
    redirectEdges(node, offset)
  }
  def unify(node: Node, offset: BigInt): Unit = {
    val updatedOffset = offsetHelper(offset, 0)
    if (isCollapsed && !node.isCollapsed) {
      collapse(node, updatedOffset)
    } else if (!isCollapsed && !node.isCollapsed) {
      if (isSeq && !node.isSeq) {
        if updatedOffset == 0 then node.unify(this, 0) else collapse(node, updatedOffset)
      } else if (!isSeq && node.isSeq) {
        if size % node.size == 0 then
          flags.seq = true
          unify(node, offset)
        else if size + updatedOffset > node.size then
          collapse(node, updatedOffset)
      } else if (isSeq && node.isSeq) {
        if size < node.size then node.unify(this, 0)
        else if node.size % size != 0 || offsetHelper(offset, 0) > 0 then collapse(node, updatedOffset)
      }
    }

    if this.equals(node) && updatedOffset > 0 then node.collapseNode()
    redirectEdges(node, updatedOffset)
  }

  def makeCell(offset: BigInt = 0): Cell = {
    val cell = Cell(Some(this), offset)
    cells.add(cell)
    cell
  }

  def updateSize(s: BigInt): Unit = {
    if isSeq && size != s then
      collapseNode()
    else if (!isSeq && s > size) then
      size = s
  }


  def isCollapsed = flags.collapsed
  def isSeq = flags.seq

  def setSeq(value: Boolean = true): Unit = {
    flags.seq = value
  }

}

/**
 * Node flags
 */
class NodeFlags {
  var collapsed = false
  var seq = false
  def join(n: NodeFlags): Unit = {

  }
}

/**
 * A memory cell (or a field). An offset into a memory object.
 */
class Cell(val node: Option[Node] = None, val offset: BigInt = 0) {

  private var pointsTo: Option[Cell] = None
  private def n = node.get

  def this(cell: Cell) = {
    this(cell.node, cell.offset)
    pointsTo = cell.pointsTo
  }

  def this(cell: Cell, offset: BigInt) = {
    this(cell.node, cell.offset + offset)
    pointsTo = cell.pointsTo
  }

  def this(node: Node, offset : BigInt) = {
    this(Some(node), offset)
  }


  override def equals(obj: Any): Boolean = {
    obj match
      case cell: Cell => cell.node.equals(this.node) && cell.offset == this.offset
      case _ => false
  }

  def unify(cell: Cell): Unit = {
    if (offset < cell.offset) then
      n.unify(cell.n, cell.offset - offset)
    else if (cell.offset < offset) then
      cell.n.unify(n, offset-cell.offset)
    else
      n.unify(cell.n, 0)
  }

  def pointTo(cell: Option[Cell] = None): Unit = {
    pointsTo = cell
  }

  def pointee : Option[Cell] = pointsTo
}

/**
 * Simulation relation mapping
 */
class SimulationMap {

}
