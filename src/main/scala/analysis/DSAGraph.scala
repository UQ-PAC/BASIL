package analysis

import com.sun.org.apache.xalan.internal.xsltc.compiler.util.NodeType
import ir.{Expr, Procedure}

import scala.collection.mutable

// need a type procedure

type Value = Procedure | Expr

/**
 * DSA Graph
 */
class Graph(val procedure: Procedure) {

  val nodes: mutable.Set[Node] = mutable.Set()
  val pointersToCells: mutable.Map[Expr, Cell]= mutable.Map()


  def makeNode(): Node = {
    val node = Node(this)
    nodes.add(node)
    node
  }

}


/**
 * DSA Node represents a memory object
 */
class Node (val owner: Graph) {
//  val links =
  private val flags: NodeFlags = NodeFlags()
  var size: Int = 0


  def offsetHelper(offset1: Int, offset2: Int): Int = {
    if isCollapsed then
      0
    else if isSeq then
      (offset1 + offset2) % size
    else
      offset1 + offset2
  }

  def redirectEdges(node: Node, offset: Int): Unit = {

  }
  def collapseNode(): Unit = {

  }

  def collapse(node: Node, offset: Int): Unit = {
    node.collapseNode()
    redirectEdges(node, offset)
  }
  def unify(node: Node, offset: Int): Unit = {
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


  def isCollapsed = flags.collapsed
  def isSeq = flags.seq

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
class Cell(var node: Option[Node] = None, var offset: Int = 0) {

  private var pointsTo: Option[Cell] = None
  private def n = node.get

  def this(cell: Cell) = {
    this(cell.node, cell.offset)
    pointsTo = cell.pointsTo
  }

  def this(cell: Cell, offset: Int) = {
    this(cell.node, cell.offset + offset)
    pointsTo = cell.pointsTo
  }

  def this(node: Node, offset : Int) = {
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

}

/**
 * Simulation relation mapping
 */
class SimulationMap {

}
