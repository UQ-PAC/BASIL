package analysis

import analysis.BitVectorEval.{bv2SignedInt, isNegative}
import ir.{Assign, BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Endian, Expr, Extract, IntraProcIRCursor, MemoryAssign, MemoryLoad, Procedure, Register, Variable, ZeroExtend, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal, SymbolTableEntry}

import scala.util.control.Breaks.{break, breakable}
import java.math.BigInteger
import scala.collection.mutable

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
class Local(
             proc: Procedure,
             symResults: Map[CFGPosition, Map[SymbolicAccess, TwoElement]],
             constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
             globals: Set[SymbolTableEntry], globalOffsets: Map[BigInt, BigInt],
             externalFunctions: Set[ExternalFunction],
             reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
             writesTo: Map[Procedure, Set[Register]],
             params:  Map[Procedure, Set[Variable]]
           ) extends Analysis[Any]{

  private val mallocRegister = Register("R0", 64)
  private val stackPointer = Register("R31", 64)

  // set of cfg positions already processed by the analysis local phase
  private val visited: mutable.Set[CFGPosition] = mutable.Set()


  // variables to symbolic access map for each cfg position
  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]] = symResults.foldLeft(Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]()) {
    (outerMap, syms) =>
      val position = syms._1
      val innerMap = syms._2.foldLeft(Map[Variable, Set[SymbolicAccess]]()) {
        (m, access) =>
          if m.contains(access._1.accessor) then
            // every variable pointing to a stack region ONLY has one symbolic access associated with it.
            m(access._1.accessor).foreach(
              sym => assert(!sym.symbolicBase.isInstanceOf[StackLocation])
            )
            assert(!access._1.symbolicBase.isInstanceOf[StackLocation])
            m + (access._1.accessor -> (m(access._1.accessor) + access._1))
          else
            m + (access._1.accessor -> Set(access._1))
      }
      outerMap + (position -> innerMap)
  }


  /**
   * if an expr is the address of a stack location return its corresponding cell
   * @param pos IL position where the expression is used
   */
  def isStack(expr: Expr, pos: CFGPosition): Option[DSC] =
    expr match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        varToSym(pos)(arg1).size == 1 && varToSym(pos)(arg1).head.symbolicBase.isInstanceOf[StackLocation] =>
        evaluateExpression(arg2, constProp(pos)) match
          case Some(v) =>
            val offset = v.value + varToSym(pos)(arg1).head.offset
            if graph.stackMapping.contains(offset) then
              Some(graph.stackMapping(offset).cells(0))
            else
              None
          case None => None
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) &&
        varToSym(pos)(arg).size == 1 && varToSym(pos)(arg).head.symbolicBase.isInstanceOf[StackLocation] =>
        val offset = varToSym(pos)(arg).head.offset
        if graph.stackMapping.contains(offset) then
          Some(graph.stackMapping(offset).cells(0))
        else
          None
      case _ => None


  var mallocCount: Int = 0

  private def nextMallocCount = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  val graph: DSG = DSG(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params)


  /**
   * if an expr is the address of a global location return its corresponding cell
   * @param pos IL position where the expression is used
   */
  def isGlobal(expr: Expr, pos: CFGPosition, size: Int = 0): Option[DSC] =
    val value = evaluateExpression(expr, constProp(pos))
    if value.isDefined  then
      val global = graph.isGlobal(value.get.value)
      if global.isDefined then
        val address = value.get.value
        val (range: AddressRange, Field(node, internal)) = global.get
        val offset = address - range.start
        node.addCell(internal + offset, size)
        graph.selfCollapse(node)
        if node.collapsed then
          Some(node.cells(0))
        else
          Some(node.getCell(internal + offset))
      else
        None
    else
      None


  /**
   * Handles unification for instructions of the form R_x = R_y [+ offset] where R_y is a pointer and [+ offset] is optional
   * @param position the cfg position being visited (note this might be a local assign of the form R_x = R_y [+ offset]
   *                 or it might be memory load/store where the index is of the form R_y [+ offset]
   * @param lhs Ev(R_x) if position is local assign or a cell from an empty node if R_y [+ offset] is the index of a memoryAssign
   * @param rhs R_y, reachingDefs(position)(R_y) can be used to find the set of SSA variables that may define R_x
   * @param pointee if false, the position is local pointer arithmetic therefore Ev(R_y [+ offset]) is merged with lhs
   *                else, the position is a memory read/write therefore E(Ev(R_y [+ offset])) is merged with lhs
   * @param offset offset if [+ offset] is present
   * @return the cell resulting from the unification
   */
  private def visitPointerArithmeticOperation(position: CFGPosition, lhs: DSC, rhs: Variable, size: Int, pointee: Boolean = false,  offset: BigInt = 0, collapse: Boolean = false) : DSC =
    // visit all the defining pointer operation on rhs variable first
    reachingDefs(position)(rhs).foreach(visit)
    // get the cells of all the SSA variables in the set
    val cells: Set[Slice] = graph.getCells(position, rhs)
    // merge the cells or their pointees with lhs
    var result = cells.foldLeft(lhs) {
      (c, t) =>
        val cell = t._1
        val internalOffset = t._2
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
          val node = cell.node.get
          graph.collapseNode(node)
          graph.mergeCells(c, if pointee then graph.adjust(node.cells(0).getPointee) else node.cells(0))

    }
    if pointee then
      cells.foreach(
        t =>
          val offset = t._1.offset
          val internalOffset = t._2
          val node = t._1.node.get
          val cell = graph.find(node.getCell(offset + internalOffset))
          if cell._pointee.isDefined && graph.find(cell.getPointee._1).equals(result) then
            graph.selfCollapse(node)
//            assert(graph.pointTo.contains(node.getCell(offset))) TODO
            result = graph.find(graph.find(node.getCell(offset)).getPointee._1)
          else
            graph.selfCollapse(node)
      )
    val resultOffset = result.offset
    graph.selfCollapse(result.node.get)
    result.node.get.getCell(result.offset)

  /**
   * handles unsupported pointer arithmetic by collapsing all the nodes invloved
   */
  def unsupportedPointerArithmeticOperation(n: CFGPosition, expr: Expr, lhsCell: DSC): Unit = {
    var containsPointer = false
    breakable {
      for (v <- expr.variables) {
        if varToSym.contains(n) && varToSym(n).contains(v) then
          containsPointer = true
          break
      }
    }
    if containsPointer then
      val cell = expr.variables.foldLeft(lhsCell) {
        (c, v) =>
          val cells: Set[Slice] = graph.getCells(n, v)

          cells.foldLeft(c) {
            (c, p) =>
              graph.mergeCells(c, p._1)
          }
      }
      val node = cell.node.get
      node.flags.unknown = true
      graph.collapseNode(node)
  }

  def visit(n: CFGPosition): Unit = {
    if visited.contains(n) then
      return
    else
      visited.add(n)
    n match
      case DirectCall(proc, target, label) if proc.name == "malloc" => // R0 = Malloc()
        val size: BigInt = evaluateExpression(mallocRegister, constProp(n)) match
          case Some(value) => value.value
          case None => 0
        val node = DSN(Some(graph), size)
        node.allocationRegions.add(HeapLocation(nextMallocCount, proc, size))
        node.flags.heap = true
        graph.mergeCells(graph.varToCell(n)(mallocRegister)._1, node.cells(0))
      case call: DirectCall if params.contains(call.target) => // Rx, Ry, ... Rn = FunctionCall()
        // create call sites for the callees
        val cs = CallSite(call, graph)
        graph.callsites.add(cs)
        cs.paramCells.foreach{
          case (variable: Variable, slice: Slice) =>
            visitPointerArithmeticOperation(call, graph.adjust(slice), variable, 0)
        }
        cs.returnCells.foreach{
          case (variable: Variable, slice: Slice) =>
            val returnArgument  = graph.varToCell(n)(variable)
            graph.mergeCells(graph.adjust(returnArgument), graph.adjust(slice))
        }
      case Assign(variable: Variable, rhs: Expr, maybeString) =>
        val expr: Expr = unwrapPaddingAndSlicing(rhs)
        val lhsCell = graph.adjust(graph.varToCell(n)(variable))
        var global = isGlobal(rhs, n)
        var stack = isStack(rhs, n)
        if global.isDefined then // Rx = global address
          graph.mergeCells(lhsCell, global.get)
        else if stack.isDefined then // Rx = stack address
          graph.mergeCells(lhsCell, stack.get)
        else
          expr match
            case BinaryExpr(op, arg1: Variable, arg2) => // Rx = Rx + c
              val arg2Offset = evaluateExpression(arg2, constProp(n))
              if op.equals(BVADD) && arg1.equals(stackPointer)
              && arg2Offset.isDefined && isNegative(arg2Offset.get) then
                () // the stack is handled prior to this
//                val size = bv2SignedInt(arg2Offset.get)
//                val node = DSN(Some(graph))
//                node.allocationRegions.add(StackLocation("Stack_"+proc.name, proc, -size))
//                node.flags.stack = true
//                graph.mergeCells(lhsCell, node.cells(0))
              else if /*varToSym.contains(n) &&  varToSym(n).contains(arg1) && */ arg2Offset.isDefined then
                // merge lhs with cell(s) corresponding to (arg1 + arg2) where arg1 is cell and arg2 is an offset
                val offset = evaluateExpression(arg2, constProp(n)).get.value
                visitPointerArithmeticOperation(n, lhsCell, arg1, 0, false, offset)
              else // c can't be statically evaluated
                unsupportedPointerArithmeticOperation(n, expr, lhsCell)

            // Rx = Ry merge corresponding cells to Rx and Ry
            case arg: Variable /*if varToSym.contains(n) && varToSym(n).contains(arg)*/ =>
             visitPointerArithmeticOperation(n, lhsCell, arg, 0)

            case MemoryLoad(mem, index, endian, size) => // Rx = Mem[Ry], merge Rx and pointee of Ry (E(Ry))
              val byteSize = (size.toDouble/8).ceil.toInt
              lhsCell.node.get.flags.read  = true
              global = isGlobal(index, n, byteSize)
              stack = isStack(index, n)
              if global.isDefined then
                graph.mergeCells(lhsCell,graph.adjust(graph.find(global.get).getPointee))
              else if stack.isDefined then
                graph.mergeCells(lhsCell, graph.adjust(graph.find(stack.get).getPointee))
              else
                index match
                  case BinaryExpr(op, arg1: Variable, arg2) =>
                    evaluateExpression(arg2, constProp(n)) match
                      case Some(v) =>
//                        assert(varToSym(n).contains(arg1))
                        val offset = v.value
                        visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, offset)
                      case None =>
//                        assert(varToSym(n).contains(arg1))
                        visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, 0, true)
                  case arg: Variable =>
//                    assert(varToSym(n).contains(arg))
                    visitPointerArithmeticOperation(n, lhsCell, arg, byteSize, true)
                  case _ => ???
            case _ =>
              unsupportedPointerArithmeticOperation(n, expr, lhsCell)

      case MemoryAssign(memory,  ind: Expr, expr: Expr, endian: Endian, size: Int, label) if unwrapPaddingAndSlicing(expr).isInstanceOf[Variable] => // if value is a literal ignore it
        // Mem[Ry] = Rx
        val value: Variable = unwrapPaddingAndSlicing(expr).asInstanceOf[Variable]
        val index: Expr = unwrapPaddingAndSlicing(ind)
        reachingDefs(n)(value).foreach(visit)
        val byteSize = (size.toDouble/8).ceil.toInt
        val global = isGlobal(index, n, byteSize)
        val stack = isStack(index, n)
        val addressPointee: DSC =
          if global.isDefined then
            graph.adjust(graph.find(global.get).getPointee)
          else if stack.isDefined then
            graph.adjust(graph.find(stack.get).getPointee)
          else
            index match
              case BinaryExpr(op, arg1: Variable, arg2) =>
                evaluateExpression(arg2, constProp(n)) match
                  case Some(v) =>
//                    assert(varToSym(n).contains(arg1))
                    val offset = v.value
                    visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg1, byteSize, true, offset)
                  case None =>
//                    assert(varToSym(n).contains(arg1))
                    visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg1, byteSize, true, 0, true)
              case arg: Variable =>
//                assert(varToSym(n).contains(arg))
                visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg, byteSize, true)
              case _ =>
                ???

        addressPointee.node.get.flags.modified = true
        val valueCells = graph.getCells(n, value)
        val result = valueCells.foldLeft(addressPointee) {
          (c, slice) =>
            graph.mergeCells(graph.adjust(slice), c)
        }

        print("")

      case _ =>
  }
  def analyze(): DSG =
    val domain = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString)

    domain.foreach(visit)

    val b = graph.solver.solution()
    graph.collectNodes
    graph.nodes.foreach(node =>
      node.children.foreach(
        child =>
          assert(graph.solver.find(child._1.term)._1.equals(node.term))
          assert(graph.solver.find(child._1.term)._2.equals(child._2))

      )
    )
    graph
}
