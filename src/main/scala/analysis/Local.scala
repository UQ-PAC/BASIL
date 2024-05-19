package analysis

import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor,  LocalAssign, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register,  Variable, ZeroExtend, computeDomain, toShortString}
import specification.{ExternalFunction, SpecGlobal}

import scala.util.control.Breaks.{break, breakable}
import java.math.BigInteger
import scala.collection.mutable

class Local(
             proc: Procedure,
             symResults: Map[CFGPosition, Map[SymbolicAccess, TwoElement]],
             constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
             globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
             externalFunctions: Set[ExternalFunction],
             reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
             writesTo: Map[Procedure, Set[Register]],
             params:  Map[Procedure, Set[Variable]]
           ) extends Analysis[Any]{

  private val mallocRegister = Register("R0", BitVecType(64))
  private val stackPointer = Register("R31", BitVecType(64))

  private val visited: mutable.Set[CFGPosition] = mutable.Set()


  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]] = symResults.foldLeft(Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]()) {
    (outerMap, syms) =>
      val position = syms._1
      val innerMap = syms._2.foldLeft(Map[Variable, Set[SymbolicAccess]]()) {
        (m, access) =>
          if m.contains(access._1.accessor) then
            // every variable pointing to a stack region ONLY has one symbolic access associated with it.
            m(access._1.accessor).foreach(
              sym => assert(!sym.symbolicBase.isInstanceOf[StackRegion2])
            )
            assert(!access._1.symbolicBase.isInstanceOf[StackRegion2])
            m + (access._1.accessor -> (m(access._1.accessor) + access._1))
          else
            m + (access._1.accessor -> Set(access._1))
      }
      outerMap + (position -> innerMap)
  }
  

  def isStack(expr: Expr, pos: CFGPosition): Option[DSC] =
    expr match
      case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
        varToSym(pos)(arg1).size == 1 && varToSym(pos)(arg1).head.symbolicBase.isInstanceOf[StackRegion2] &&
        evaluateExpression(arg2, constProp(pos)).isDefined =>
        val offset = evaluateExpression(arg2, constProp(pos)).get.value + varToSym(pos)(arg1).head.offset
        if graph.stackMapping.contains(offset) then
          Some(graph.stackMapping(offset).cells(0))
        else
          None
      case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) &&
        varToSym(pos)(arg).size == 1 && varToSym(pos)(arg).head.symbolicBase.isInstanceOf[StackRegion2] =>
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


  def isGlobal(expr: Expr, pos: CFGPosition, size: Int = 0): Option[DSC] =
    if evaluateExpression(expr, constProp(pos)).isDefined && graph.isGlobal(evaluateExpression(expr, constProp(pos)).get.value).isDefined then
      val address = evaluateExpression(expr, constProp(pos)).get.value
      val ((baseAddress: BigInt, end: BigInt), (node: DSN, internal: BigInt)) = graph.isGlobal(evaluateExpression(expr, constProp(pos)).get.value).get
      val offset = address - baseAddress
      node.addCell(internal + offset, size)
      graph.optionalCollapse(node)
      if node.collapsed then
        Some(node.cells(0))
      else
        Some(node.getCell(internal + offset))
    else
      None

//  def getCells(pos: CFGPosition, arg: Variable): Set[(DSC, BigInt)] =
//    if reachingDefs(pos).contains(arg) then
//      reachingDefs(pos)(arg).foldLeft(Set[(DSC, BigInt)]()) {
//        (s, defintion) =>
//          s + graph.varToCell(defintion)(arg)
//      }
//    else
//      Set(graph.formals(arg))




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
    val cells: Set[(DSC, BigInt)] = graph.getCells(position, rhs)
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
              graph.getPointeeAdjusted(node.getCell(field))
            else
              node.getCell(field)
          )
        else
          val node = cell.node.get
          graph.collapseNode(node)
          graph.mergeCells(c, if pointee then graph.getPointeeAdjusted(node.cells(0)) else node.cells(0))

    }
    if pointee then
      cells.foreach(
        t =>
          val offset = t._1.offset
          val internalOffset = t._2
          val node = t._1.node.get
          val cell = node.getCell(offset + internalOffset)
          if graph.pointTo.contains(cell) && graph.pointTo(cell)._1.equals(result) then
            graph.optionalCollapse(node)
            assert(graph.pointTo.contains(node.getCell(offset)))
            result = graph.getPointee(node.getCell(offset))._1
          else
            graph.optionalCollapse(node)
      )
    val resultOffset = result.offset
    graph.optionalCollapse(result.node.get)
    result.node.get.getCell(result.offset)



  def visit(n: CFGPosition): Unit = {
    if visited.contains(n) then
      return
    else
      visited.add(n)
    n match
      case DirectCall(proc, target, label) if proc.name == "malloc" =>
        val size: BigInt = evaluateExpression(mallocRegister, constProp(n)) match
          case Some(value) => value.value
          case None => 0
        val node = DSN(Some(graph), size)
        node.allocationRegions.add(HeapRegion2(nextMallocCount, proc, size))
        node.flags.heap = true
        graph.mergeCells(graph.varToCell(n)(mallocRegister)._1, node.cells(0))
      case call: DirectCall if params.contains(call.target) =>
        val cs = CallSite(call, graph)
        graph.callsites.add(cs)
        cs.paramCells.foreach{
          case (variable: Variable, cell: DSC) =>
            visitPointerArithmeticOperation(call, cell, variable, 0)
        }
        cs.returnCells.foreach{
          case (variable: Variable, cell: DSC) =>
            val returnArgument  = graph.varToCell(n)(variable)._1
            graph.mergeCells(returnArgument, cell)
        }
      case LocalAssign(variable, rhs, maybeString) =>
        val expr: Expr = unwrapPaddingAndSlicing(rhs)
        val lhsCell = graph.varToCell(n)(variable)._1
        if isGlobal(expr, n).isDefined then
          val global = isGlobal(expr, n).get
          graph.mergeCells(lhsCell, global)
        else if isStack(expr, n).isDefined then // just in case stack can't be recognised in after this assignment
          val stack = isStack(expr, n).get
          graph.mergeCells(lhsCell, stack)
        else
          expr match
            case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
              && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= BITVECNEGATIVE =>
              val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
              val node = DSN(Some(graph))
              node.allocationRegions.add(StackRegion2("Stack_"+proc.name, proc, -size))
              node.flags.stack = true
              graph.mergeCells(lhsCell, node.cells(0))

            case BinaryExpr(op, arg1: Variable, arg2) if /*varToSym.contains(n) &&  varToSym(n).contains(arg1) && */ evaluateExpression(arg2, constProp(n)).isDefined =>
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              visitPointerArithmeticOperation(n, lhsCell, arg1, 0, false, offset)

            case arg: Variable /*if varToSym.contains(n) && varToSym(n).contains(arg)*/ =>
             visitPointerArithmeticOperation(n, lhsCell, arg, 0)

            case MemoryLoad(mem, index, endian, size) =>
              val byteSize = (size.toDouble/8).ceil.toInt
              lhsCell.node.get.flags.read  = true
              if isGlobal(index, n, byteSize).isDefined then
                val global = isGlobal(index, n, byteSize).get
                graph.mergeCells(lhsCell, graph.getPointeeAdjusted(global))
              else if isStack(index, n).isDefined then
                val stack = isStack(index, n).get
                graph.mergeCells(lhsCell, graph.getPointeeAdjusted(stack))
              else
                index match
                  case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//                    assert(varToSym(n).contains(arg1))
                    val offset = evaluateExpression(arg2, constProp(n)).get.value
                    visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, offset)
                  case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isEmpty=>
//                    assert(varToSym(n).contains(arg1))
                    visitPointerArithmeticOperation(n, lhsCell, arg1, byteSize, true, 0, true)
                  case arg: Variable =>
//                    assert(varToSym(n).contains(arg))
                    visitPointerArithmeticOperation(n, lhsCell, arg, byteSize, true)
                  case _ => ???
            case _ =>
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
                    val cells: Set[(DSC, BigInt)] = graph.getCells(n, v)

                    cells.foldLeft(c) {
                      (c, p) =>
                        graph.mergeCells(c, p._1)
                    }
                }
                val node = cell.node.get
                node.flags.unknown = true
                graph.collapseNode(node)

      case MemoryAssign(memory, MemoryStore(mem, index, expr: Expr, endian, size), label) if unwrapPaddingAndSlicing(expr).isInstanceOf[Variable] => // if value is a literal ignore it
        val value: Variable = unwrapPaddingAndSlicing(expr).asInstanceOf[Variable]
        reachingDefs(n)(value).foreach(visit)
        val byteSize = (size.toDouble/8).ceil.toInt
        val addressPointee: DSC =
          if isGlobal(index, n, byteSize).isDefined then
            graph.getPointeeAdjusted(isGlobal(index, n, byteSize).get)
          else if isStack(index, n).isDefined then
            graph.getPointeeAdjusted(isStack(index, n).get)
          else
            index match
            case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//              assert(varToSym(n).contains(arg1))
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg1, byteSize, true, offset)
            case arg: Variable =>
//              assert(varToSym(n).contains(arg))
              visitPointerArithmeticOperation(n, DSN(Some(graph)).cells(0), arg, byteSize, true)
            case _ => ???

        addressPointee.node.get.flags.modified = true
        val valueCells = graph.getCells(n, value)
        val result = valueCells.foldLeft(addressPointee) {
          (c, p) =>
            val node = p._1.node.get
            val cell = node.addCell(p._1.offset + p._2, 0)
            graph.mergeCells(cell, c)
        }

      case _ =>
  }
  def analyze(): DSG =
    val domain = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).reverse

    domain.foreach(visit)

//    println(graph.formals)
//    val results = graph.varToCell.keys.toSeq.sortBy(_.toShortString)
//    results.foreach {
//      pos =>
//        println(pos)
//        val tab = "    "
//        graph.varToCell(pos).foreach {
//          case (variable, cell) =>
//            println(tab + variable.toString + " -> " + cell.toString)
//        }
//    }
//    println(graph.pointTo)
//    // collect the nodes in the dsg
    graph.nodes.addAll(graph.formals.values.map(_._1.node.get))
    graph.varToCell.values.foreach(
      value => graph.nodes.addAll(value.values.map(_._1.node.get))
    )
    graph.nodes.addAll(graph.stackMapping.values)
    graph.nodes.addAll(graph.globalMapping.values.map(_._1))
    graph
}
