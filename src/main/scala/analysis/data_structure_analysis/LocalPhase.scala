package analysis.data_structure_analysis

import analysis.BitVectorEval.{bv2SignedInt, isNegative}
import analysis.*
import ir.*
import specification.{ExternalFunction, SpecGlobal, SymbolTableEntry}
import util.writeToFile

import java.math.BigInteger
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

/**
 * The local phase of Data Structure Analysis
 * @param proc procedure to be analysed
 * @param symResults result of symbolic access analysis
 * @param constProp
 * @param globals
 * @param globalOffsets
 * @param externalFunctions
 * @param reachingDefs
 * @param writesTo mapping from procedures to registers they change
 * @param params mapping from procedures to their parameters
 */
class LocalPhase(proc: Procedure,
                 symResults: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
                 constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                 globals: Set[SymbolTableEntry], globalOffsets: Map[BigInt, BigInt],
                 externalFunctions: Set[ExternalFunction],
                 reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
                 writesTo: Map[Procedure, Set[Register]],
                 params: Map[Procedure, Set[Variable]]
                ) extends Analysis[Any] {

  private val mallocRegister = Register("R0", 64)
  private val stackPointer = Register("R31", 64)

  // set of cfg positions already processed by the analysis local phase
  private val visited: mutable.Set[CFGPosition] = mutable.Set()

  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAddress]]] = symResults.map { (position, innerMap) =>
    val newMap = innerMap.keys.foldLeft(Map[Variable, Set[SymbolicAddress]]()) { (m, access) =>
      if (m.contains(access.accessor)) {
        m + (access.accessor -> (m(access.accessor) + access))
      } else {
        m + (access.accessor -> Set(access))
      }
    }
    position -> newMap
  }

  private def getStack(offset: BigInt, size: Int): Cell = {
    var last: BigInt = 0
    var headNodeOffset: BigInt = -1

    val head: Cell =
      if graph.stackMapping.contains(offset) then
        headNodeOffset = 0
        graph.stackMapping(offset).cells(0)
      else
        breakable {
          graph.stackMapping.keys.toSeq.sorted.foreach(
            elementOffset =>
              if offset < elementOffset then
                break
              else
                last = elementOffset
          )
        }
        val diff = offset - last
        headNodeOffset = offset
        assert(graph.stackMapping.contains(last))
        graph.stackMapping(last).getCell(diff)

    graph.find(head).growSize(size)
    val headOffset = head.offset
    graph.stackMapping.keys.toSeq.filter(off => off > headOffset && off < headOffset + size).sorted.foreach {
      off =>
        val stackDiff = off - headOffset
        val updatedHead = graph.find(head)
        val newHeadOffset = updatedHead.offset
        val headNode = updatedHead.node.get
        graph.mergeCells(headNode.addCell(newHeadOffset + stackDiff, 0), graph.find(graph.stackMapping(off).cells(0)))
    }
    head
  }


  /**
   * if an expr is the address of a stack location return its corresponding cell
   * @param pos IL position where the expression is used
   */
  private def isStack(expr: Expr, pos: CFGPosition, size : Int = 0): Option[Cell] = {
    expr match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        varToSym(pos)(arg1).exists(s => s.symbolicBase.isInstanceOf[StackLocation]) =>
        evaluateExpression(arg2, constProp(pos)) match
          case Some(v) =>
            val stackRegions = varToSym(pos)(arg1).filter(s => s.symbolicBase.isInstanceOf[StackLocation])
            val res = stackRegions.tail.foldLeft(getStack(v.value + stackRegions.head.offset, size)) {
              (res, sym) =>
                graph.mergeCells(res, getStack(v.value + sym.offset, size))
            }
            Some(res)
          case None => None
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) &&
        varToSym(pos)(arg).exists(s => s.symbolicBase.isInstanceOf[StackLocation]) =>
        val stackRegions = varToSym(pos)(arg).filter(s => s.symbolicBase.isInstanceOf[StackLocation])
        val res = stackRegions.tail.foldLeft(getStack(stackRegions.head.offset, size)) {
          (res, sym) =>
            graph.mergeCells(res, getStack(sym.offset, size))
        }
        Some(res)
      case _ => None
  }

  var mallocCount: Int = 0

  private def nextMallocCount = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  val graph: Graph = Graph(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params)

  /**
   * if an expr is the address of a global location return its corresponding cell
   * @param pos IL position where the expression is used
   */
  def getGlobal(expr: Expr, pos: CFGPosition, size: Int = 0): Option[Cell] = {
    val value = evaluateExpression(expr, constProp(pos))
    if value.isDefined then
      val globals = graph.getGlobal(value.get.value, size)
      val address = value.get.value
      if globals.nonEmpty then
        val head = globals.head
        val DSAGlobal(range: AddressRange, Field(node, internal)) = head
        val headOffset: BigInt = if address > range.start then address - range.start + internal else internal
        val headNode = node
        val headCell: Cell = node.addCell(headOffset, size)
        graph.selfCollapse(headNode)
        val tail = globals.tail
        tail.foreach {
          g =>
            val DSAGlobal(range: AddressRange, Field(node, internal)) = g
            val offset: BigInt = if address > range.start then address - range.start + internal else internal
            node.addCell(offset, 0)
            graph.selfCollapse(node)
            assert(range.start >= address)
            graph.mergeCells(graph.find(headNode.addCell(range.start - address, 0)), graph.find(node.getCell(offset)))
        }

        Some(graph.find(headCell))
      else
        None
    else
      None
  }

  /**
   * Handles unification for instructions of the form R_x = R_y [+ offset] where R_y is a pointer and [+ offset] is optional
   * @param position the cfg position being visited (note this might be a local assign of the form R_x = R_y [+ offset]
   *                 or it might be memory load/store where the index is of the form R_y [+ offset]
   * @param lhs Ev(R_x) if position is local assign or a cell from an empty node if R_y [+ offset] is the index of a memoryStore
   * @param rhs R_y, reachingDefs(position)(R_y) can be used to find the set of SSA variables that may define R_x
   * @param pointee if false, the position is local pointer arithmetic therefore Ev(R_y [+ offset]) is merged with lhs
   *                else, the position is a memory read/write therefore E(Ev(R_y [+ offset])) is merged with lhs
   * @param offset offset if [+ offset] is present
   * @return the cell resulting from the unification
   */
  private def visitPointerArithmeticOperation(position: CFGPosition, lhs: Cell, rhs: Variable, size: Int, pointee: Boolean = false, offset: BigInt = 0, collapse: Boolean = false): Cell =
    // visit all the defining pointer operation on rhs variable first
    reachingDefs(position)(rhs).foreach(visit)
    // get the cells of all the SSA variables in the set
    val cells: Set[Slice] = graph.getCells(position, rhs).foldLeft(Set[Slice]()) {
      (col, slice) =>
        col + Slice(graph.find(slice.cell), slice.internalOffset)
    }
    // merge the cells or their pointees with lhs
    var result = cells.foldLeft(lhs) {
      (c, t) =>
        val cell = t.cell
        val internalOffset = t.internalOffset
        if !collapse then  // offset != 0 then // it's R_x = R_y + offset
          val node = cell.node.get // get the node of R_y
          var field = offset + cell.offset + internalOffset // calculate the total offset
          node.addCell(field, size) // add cell there if doesn't already exists
          if node.collapsed then
            field = 0
          graph.mergeCells(c,
            if pointee then
              graph.adjust(node.getCell(field).getPointee)
            else
              node.getCell(field)
          )
        else
          var node = cell.node.get
          node = graph.collapseNode(node)
          graph.mergeCells(c, if pointee then graph.adjust(node.cells(0).getPointee) else node.cells(0))

    }
    if pointee then
      cells.foreach { t =>
        val offset = t.cell.offset
        val internalOffset = t.internalOffset
        val node = t.cell.node.get
        val cell = graph.find(node.getCell(offset + internalOffset))
        if cell.pointee.isDefined && graph.find(cell.getPointee.cell).equals(result) then
          graph.selfCollapse(node)
          //            assert(graph.pointTo.contains(node.getCell(offset))) TODO
          result = graph.find(graph.find(node.getCell(offset)).getPointee.cell)
        else
          graph.selfCollapse(node)
      }
    val resultOffset = result.offset
    graph.selfCollapse(result.node.get)
    result.node.get.getCell(result.offset)

  /**
   * handles unsupported pointer arithmetic by collapsing all the nodes invloved
   */
  private def unsupportedPointerArithmeticOperation(n: CFGPosition, expr: Expr, lhsCell: Cell): Cell = {
    val cell = expr.variables.foldLeft(lhsCell) {
      (c, v) =>
        val cells: Set[Slice] = graph.getCells(n, v)

        cells.foldLeft(c) {
          (c, p) =>
            graph.mergeCells(c, p.cell)
        }
    }

    val node = cell.node.get
    node.flags.unknown = true
    graph.collapseNode(node)

    node.cells(0)
  }

  def visit(n: CFGPosition): Unit = {
    if visited.contains(n) then
      return
    else
      visited.add(n)
    n match
      case DirectCall(target, _) if target.name == "malloc" => // R0 = Malloc()
        val size: BigInt = evaluateExpression(mallocRegister, constProp(n)) match
          case Some(value) => value.value
          case None => 0
        val node = Node(Some(graph), size)
        node.allocationRegions.add(HeapLocation(nextMallocCount, target, size))
        node.flags.heap = true
        graph.mergeCells(graph.varToCell(n)(mallocRegister).cell, node.cells(0))
      case call: DirectCall if params.contains(call.target) => // Rx, Ry, ... Rn = FunctionCall()
        // create call sites for the callees
        val cs = CallSite(call, graph)
        graph.callsites.add(cs)
        cs.paramCells.foreach { (variable, slice) =>
          visitPointerArithmeticOperation(call, graph.adjust(slice), variable, 0)
        }
        cs.returnCells.foreach { (variable, slice) =>
          val returnArgument = graph.varToCell(n)(variable)
          graph.mergeCells(graph.adjust(returnArgument), graph.adjust(slice))
        }
      case LocalAssign(variable, rhs, _) =>
        val expr: Expr = unwrapPaddingAndSlicing(rhs)
        val lhsCell = graph.adjust(graph.varToCell(n)(variable))
        val global = isGlobal(rhs, n)
        val stack = isStack(rhs, n)
        if global.isDefined then // Rx = global address
          graph.mergeCells(lhsCell, global.get)
        else if stack.isDefined then // Rx = stack address
          graph.mergeCells(lhsCell, stack.get)
        else
          expr match
            case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) => // Rx = Rx + c
              val arg2Offset = evaluateExpression(arg2, constProp(n))
              if op.equals(BVADD) && arg1.equals(stackPointer)
              && arg2Offset.isDefined && isNegative(arg2Offset.get) then
                () // the stack is handled prior to this
              else if /*varToSym.contains(n) &&  varToSym(n).contains(arg1) && */  arg2Offset.isDefined then
                // merge lhs with cell(s) corresponding to (arg1 + arg2) where arg1 is cell and arg2 is an offset
                val offset = evaluateExpression(arg2, constProp(n)).get.value
                visitPointerArithmeticOperation(n, lhsCell, arg1, 0, false, offset)
              else // c can't be statically evaluated
                unsupportedPointerArithmeticOperation(n, expr, lhsCell)

            // Rx = Ry merge corresponding cells to Rx and Ry
            case arg: Variable /*if varToSym.contains(n) && varToSym(n).contains(arg)*/ =>
             visitPointerArithmeticOperation(n, lhsCell, arg, 0)
            case _ =>
              unsupportedPointerArithmeticOperation(n, expr, lhsCell)

      case MemoryLoad(lhs, _, index, _, size, _) => // Rx = Mem[Ry], merge Rx and pointee of Ry (E(Ry))
        val indexUnwrapped = unwrapPaddingAndSlicing(index)
        val lhsCell = graph.adjust(graph.varToCell(n)(lhs))
        assert(size % 8 == 0)
        val byteSize = size / 8
        lhsCell.node.get.flags.read = true
        val global = getGlobal(indexUnwrapped, n, byteSize)
        val stack = isStack(indexUnwrapped, n, byteSize)
        if global.isDefined then
          graph.mergeCells(lhsCell, graph.adjust(graph.find(global.get).getPointee))
        else if stack.isDefined then
          graph.mergeCells(lhsCell, graph.adjust(graph.find(stack.get).getPointee))
        else
          indexUnwrapped match
            case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) =>
              evaluateExpression(arg2, constProp(n)) match
                case Some(v) =>
                  //                        assert(varToSym(n).contains(arg1))
                  val offset = v.value
                  visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, offset)
                case None =>
                  //                        assert(varToSym(n).contains(arg1))
                  // collapse the result
                  //                        visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, 0, true)
                  unsupportedPointerArithmeticOperation(n, indexUnwrapped, Node(Some(graph)).cells(0))
            case arg: Variable =>
              //                    assert(varToSym(n).contains(arg))
              visitPointerArithmeticOperation(n, lhsCell, arg, byteSize, true)
            case _ => ???
      case MemoryStore(_, ind, expr, _, size, _) =>
        val unwrapped = unwrapPaddingAndSlicing(expr)
        unwrapped match {
          // Mem[Ry] = Rx
          case value: Variable =>
            val index: Expr = unwrapPaddingAndSlicing(ind)
            reachingDefs(n)(value).foreach(visit)
            assert(size % 8 == 0)
            val byteSize = size / 8
            val global = getGlobal(index, n, byteSize)
            val stack = isStack(index, n, byteSize)
            val addressPointee: Cell =
              if global.isDefined then
                graph.adjust(graph.find(global.get).getPointee)
              else if stack.isDefined then
                graph.adjust(graph.find(stack.get).getPointee)
              else
                index match
                  case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) =>
                    evaluateExpression(arg2, constProp(n)) match
                      case Some(v) =>
                        //                    assert(varToSym(n).contains(arg1))
                        val offset = v.value
                        visitPointerArithmeticOperation(n, Node(Some(graph)).cells(0), arg1, byteSize, true, offset)
                      case None =>
                        //                    assert(varToSym(n).contains(arg1))
                        // collapse the results
                        // visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg1, byteSize, true, 0, true)
                        unsupportedPointerArithmeticOperation(n, index, Node(Some(graph)).cells(0))
                  case arg: Variable =>
                    //                assert(varToSym(n).contains(arg))
                    visitPointerArithmeticOperation(n, Node(Some(graph)).cells(0), arg, byteSize, true)
                  case _ =>
                    ???

            addressPointee.node.get.flags.modified = true
            val valueCells = graph.getCells(n, value)
            val result = valueCells.foldLeft(addressPointee) { (c, slice) =>
              graph.mergeCells(graph.adjust(slice), c)
            }
          case _ => // if value is a literal ignore it
        }
      case _ =>

  }
  def analyze(): Graph = {
    val domain = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString)

    domain.foreach(visit)

    val b = graph.solver.solution()
    graph.collectNodes()
    //    graph.nodes.foreach(node =>
    //      node.children.foreach(
    //        child =>
    //          assert(graph.solver.find(child._1.term).equals(graph.solver.find(node.term)))
    //          assert(graph.solver.find(child._1.term)._2.equals(child._2))
    //
    //      )
    //    )
    graph
  }
}
