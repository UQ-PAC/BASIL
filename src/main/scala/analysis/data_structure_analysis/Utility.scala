package analysis.data_structure_analysis

import analysis.*
import analysis.solvers.DSAUniTerm
import ir.*
import util.Counter

import scala.collection.mutable

class Flags() {
  var collapsed = false
  var function = false
  var stack = false
  var heap = false
  var loaded = false
  var par = false
  var ret = false
  var global = false
  var unknown = false
  var read = false
  var modified = false
  var incomplete = false
  var foreign = false
  var merged = false

  def join(other: Flags): Unit =
    collapsed = collapsed || other.collapsed
    stack = other.stack || stack
    heap = other.heap || heap
    global = other.global || global
    unknown = other.unknown || unknown
    read = other.read || read
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    foreign = other.foreign && foreign
    merged = true
    function = function || other.function
}

/** a Data structure Node
  */
class Node(using counter: Counter)(val graph: Option[Graph], var size: BigInt = 0, val id: Int = counter.next().toInt) {

  val term: DSAUniTerm = DSAUniTerm(this)
  val children: mutable.Map[Node, BigInt] = mutable.Map()
//  var collapsed = false
  var flags: Flags = Flags()
  def collapsed: Boolean = flags.collapsed

  val allocationRegions: mutable.Set[MemoryLocation] = mutable.Set()

  val cells: mutable.Map[BigInt, Cell] = mutable.Map()
  this.addCell(0, 0)

  def getSize: BigInt = {
    val (offset, cell) = cells.toSeq.maxBy((offset, cell) => offset)
    size = offset + cell.largestAccessedSize
    size
  }

  def getCell(offset: BigInt): Cell = {
    if (collapsed) {
      cells(0)
    } else if (!cells.contains(offset)) {
      var result: Option[Cell] = None
      cells.foreach { (start, cell) =>
        if (start <= offset && offset < (start + cell.largestAccessedSize)) {
          result = Some(cell)
        }
      }
      result match {
        case Some(value) => value
        case None => ???
        //          Logger.warn(s"$this didn't have a cell at offset: $offset. An empty cell was added in")
        //          addCell(offset, 0)
      }
    } else {
      cells(offset)
    }
  }

  def addCell(offset: BigInt, size: Int): Cell = {
//    this.updateSize(offset + size)
    if collapsed then cells(0)
    else if !cells.contains(offset) then
      val cell = Cell(Some(this), offset)
      cells.update(offset, cell)
      cell.growSize(size)
      cell
    else
      cells(offset).growSize(size)
      cells(offset)
  }

  def cloneSelf(graph: Graph): Node = {
    val node = Node(Some(graph), this.size)
    node.allocationRegions.addAll(this.allocationRegions)
    node.flags.join(this.flags)
    cells.foreach { (offset, cell) =>
      node.addCell(offset, cell.largestAccessedSize)
    }
    node
  }

  def cloneNode(from: Graph, to: Graph): Unit = {
//    debugAssert(from.nodes.contains(this)) TODO update nodes after each phase for to check this assertion
    if (!to.nodes.contains(this)) {
      to.nodes.add(this)

      from.varToCell.foreach { (pos, varMap) =>
        varMap.foreach { (variable, slice) =>
          if (from.find(slice).node.equals(this)) {
            if (to.varToCell.contains(pos)) {
              if to.varToCell(pos).contains(variable) then {
                // ensures corresponding cell in the caller (cloned from the callee during the BU phase)
                // is unified with itself during the top-down phase
                to.mergeCells(to.adjust(to.varToCell(pos)(variable)), from.adjust(slice))
              } else to.varToCell(pos)(variable) = from.find(slice)
            } else {
              to.varToCell(pos) = mutable.Map(variable -> from.find(slice))
            }
          }
        }
      }
      from.formals.foreach { (variable, slice) =>
        if (from.find(slice).node.equals(this)) {
          if (to.varToCell.contains(from.proc)) {
            if to.varToCell(from.proc).contains(variable) then {
              to.mergeCells(to.adjust(to.varToCell(from.proc)(variable)), from.adjust(slice))
            } else to.varToCell(from.proc)(variable) = from.find(slice)
          } else {
            to.varToCell(from.proc) = mutable.Map(variable -> from.find(slice))
          }
        }
      }
      cells.values.foreach { cell =>
        if (cell.pointee.isDefined) {
          val pointee = cell.getPointee
          pointee.node.cloneNode(from, to)
          //          to.pointTo.update(cell, pointee) TODO check this is not necessary
        }
      }
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match
      case node: Node =>
        this.id == node.id
      case _ => false
  }

  override def hashCode(): Int = id

  override def toString: String = s"Node($id, $allocationRegions ${if collapsed then ", collapsed" else ""})"

}

/** a cell in DSA
  * @param node
  *   the node this cell belongs to
  * @param offset
  *   the offset of the cell
  */
class Cell(using Counter)(val node: Option[Node], val offset: BigInt) {
  var largestAccessedSize: Int = 0

  // the cell's pointee
  var pointee: Option[Slice] = None

  // returns the cell's pointee if it has one.
  // if not it will create a placeholder, set it as the pointee of this cell and return it
  def getPointee: Slice =
    if pointee.isEmpty then
      val node = Node(Some(this.node.get.graph.get))
      pointee = Some(Slice(node.cells(0), 0))
    else
      val graph = pointee.get.node.graph.get
      val resolvedPointee = graph.find(graph.adjust(pointee.get))

      pointee = Some(graph.deadjust(resolvedPointee))
    pointee.get

  def growSize(size: Int): Boolean =
    if size > largestAccessedSize then
      largestAccessedSize = size
      true
    else false

  override def equals(obj: Any): Boolean =
    obj match
      case cell: Cell => this.node.equals(cell.node) && this.offset.equals(cell.offset)
      case _ => false

  override def toString: String = s"Cell(${if node.isDefined then node.get.toString else "NONE"}, $offset)"
}

/** a slice made from a cell and an internal offset
  */
case class Slice(cell: Cell, internalOffset: BigInt) {
  def node: Node = cell.node.get
  def offset: BigInt = cell.offset
}

/** represents a direct call in DSA
  * @param call
  *   instance of the call
  * @param graph
  *   caller's DSG
  */
class CallSite(using Counter)(val call: DirectCall, val graph: Graph) {
  val proc: Procedure = call.target
  val paramCells: mutable.Map[Variable, Slice] =
    graph.params(proc).foldLeft(mutable.Map[Variable, Slice]()) { (m, reg) =>
      val node = Node(Some(graph))
      node.flags.incomplete = true
      m += (reg -> Slice(node.cells(0), 0))
    }
  val returnCells: mutable.Map[Variable, Slice] =
    graph.writesTo(proc).foldLeft(mutable.Map[Variable, Slice]()) { (m, reg) =>
      val node = Node(Some(graph))
      node.flags.incomplete = true
      m += (reg -> Slice(node.cells(0), 0))
    }
}

case class DSAGlobal(addressRange: AddressRange, field: Field) {
  lazy val start: BigInt = addressRange.start
  lazy val end: BigInt = addressRange.end
  lazy val node: Node = field.node
  lazy val offset: BigInt = field.offset
}

// global address range
case class AddressRange(start: BigInt, end: BigInt)

// a node, offset pair, difference to a cell is that it doesn't represent a DSG construct,
case class Field(node: Node, offset: BigInt)

// unwraps internal padding and slicing and returns the expression
def unwrapPaddingAndSlicing(expr: Expr): Expr = {
  // TODO: if we really want we can implement a rewriter to coerce
  // an expression to a 64 bit precision expression in a similar way to
  // the rewriter that converts bv1 to bool exprs:
  // Expand inner expressions, wrap in an extract(32, 0) expr to maintain
  // internal type safety, and push the extract up the until until it is at the
  // outermost expression.
  val r = ir.eval.simplifyPaddingAndSlicingExprFixpoint(expr)(0) match {
    case Extract(_, 0, x) => x
    case ZeroExtend(_, x) => x
    case BinaryExpr(BVADD, Extract(hi, 0, x), y: Literal) =>
      ir.eval.partialEvaluateExpr(BinaryExpr(BVADD, x, SignExtend(size(x).get - size(y).get, y)))
    case o => o
  }
  r
}
