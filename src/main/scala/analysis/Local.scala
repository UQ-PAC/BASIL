package analysis

import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, IntraProcIRCursor, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, computeDomain, toShortString}
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
             writesTo: Map[Procedure, Set[Register]]
           ) extends Analysis[Any]{

  val bitvecnegative: BigInt = new BigInt(new BigInteger("9223372036854775808"))
  val mallocRegister = Register("R0", BitVecType(64))
  val stackPointer = Register("R31", BitVecType(64))

  private val visited: mutable.Set[CFGPosition] = mutable.Set()


  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]] = symResults.foldLeft(Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]()) {
    (outerMap, syms) =>
      val position = syms._1
      val innerMap = syms._2.foldLeft(Map[Variable, Set[SymbolicAccess]]()) {
        (m, access) =>
          if (m.contains(access._1.accessor)) then
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



  def decToBinary(n: BigInt): Array[Int] = {
    val binaryNum: Array[Int] = new Array[Int](64)
    var i = 0
    var num = n
    while (num > 0) {
      binaryNum(i) = (num % BigInt(2)).intValue
      num = num / 2
      i += 1
    }
    binaryNum
  }

  def twosComplementToDec(binary: Array[Int]): BigInt = {
    var result: BigInt = BigInt(0)
    var counter: Int = 0
    binary.foreach(
      n =>
        if counter == binary.length - 1 && n == 1 then
          result = result - BigInt(2).pow(counter)
        else if n == 1 then
          result = result + BigInt(2).pow(counter)
        counter += 1
    )
    result
  }

  var mallocCount: Int = 0

  private def nextMallocCount = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  val graph = DSG(proc, constProp, varToSym, globals, globalOffsets, externalFunctions, reachingDefs, writesTo)


  def isGlobal(expr: Expr, pos: CFGPosition, size: Int = 0): Option[DSC] =
    if evaluateExpression(expr, constProp(pos)).isDefined && graph.isGlobal(evaluateExpression(expr, constProp(pos)).get.value).isDefined then
      val address = evaluateExpression(expr, constProp(pos)).get.value
      val node: DSN = graph.isGlobal(evaluateExpression(expr, constProp(pos)).get.value).get
      val baseAddress = node.region.get.asInstanceOf[DataRegion2].start
      val offset = address - baseAddress
      node.addCell(offset, size)
      if node.collapsed then
        Some(node.cells(0))
      else
        Some(node.cells(offset))
    else
      None

  def getCells(pos: CFGPosition, arg: Variable): Set[DSC] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[DSC]()) {
        (s, defintion) =>
          s + graph.varToCell(defintion)(arg)
      }
    else
      Set(graph.formals(arg))

  def getNodes(pos: CFGPosition, arg: Variable): Set[DSN] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[DSN]()){
        (s, definition) =>
          s + graph.varToCell(definition)(arg).node.get
      }
    else
      Set(graph.formals(arg).node.get)

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
  private def visitPointerArithmeticOperation(position: CFGPosition, lhs: DSC, rhs: Variable, pointee: Boolean = false,  offset: BigInt = 0) : DSC =
    // visit all the defining pointer operation on rhs variable first
    reachingDefs(position)(rhs).foreach(visit)
    // get the cells of all the SSA variables in the set
    val cells: Set[DSC] = getCells(position, rhs)
    // merge the cells or their pointees with lhs
    cells.foldLeft(lhs) {
      (c, cell) =>
        if offset != 0 then // it's R_x = R_y + offset
          val node = cell.node.get // get the node of R_y
          var field = offset + cell.offset // calculate the total offset
          node.addCell(field, 0) // add cell there if doesn't already exists
          if node.collapsed then
            field = 0
          graph.mergeCells(c, if pointee then graph.getPointee(node.cells(field)) else node.cells(field))
        else
          graph.mergeCells(c, if pointee then graph.getPointee(cell) else cell)
    }


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
        val node = DSN(Some(graph), Some(HeapRegion2(nextMallocCount, proc, size)))
        graph.nodes.add(node)
        graph.mergeCells(graph.varToCell(n)(mallocRegister), node.cells(0))

      case LocalAssign(variable, expr, maybeString) =>
        val lhsCell = graph.varToCell(n)(variable)
        if isGlobal(expr, n).isDefined then
          val global = isGlobal(expr, n).get
          graph.mergeCells(lhsCell, global)
        else if isStack(expr, n).isDefined then // just in case stack can't be recognised in after this assignment
          val stack = isStack(expr, n).get
          graph.mergeCells(lhsCell, stack)
        else
          expr match
            case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
              && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= bitvecnegative =>
              val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
              val node = DSN(Some(graph), Some(StackRegion2("Stack_"+proc.name, proc, -size)))
              graph.nodes.add(node)
              graph.mergeCells(lhsCell, node.cells(0))

            case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(n) &&  varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              visitPointerArithmeticOperation(n, lhsCell, arg1, false, offset)

            case arg: Variable if varToSym(n).contains(arg) =>
             visitPointerArithmeticOperation(n, lhsCell, arg)

            case MemoryLoad(mem, index, endian, size) =>
              val byteSize = (size.toDouble/8).ceil.toInt
              if isGlobal(index, n, byteSize).isDefined then
                val global = isGlobal(index, n, byteSize).get
                graph.mergeCells(lhsCell, graph.getPointee(global))
              else if isStack(index, n).isDefined then
                val stack = isStack(index, n).get
                graph.mergeCells(lhsCell, graph.getPointee(stack))
              else
                index match
                  case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
                    assert(varToSym(n).contains(arg1))
                    val offset = evaluateExpression(arg2, constProp(n)).get.value
                    visitPointerArithmeticOperation(n, lhsCell, arg1, true, offset)
                  case arg: Variable =>
                    assert(varToSym(n).contains(arg))
                    visitPointerArithmeticOperation(n, lhsCell, arg, true)
                  case _ => ???
            case _ =>
              breakable {
                var containsPointer = false
                for (v <- expr.variables) {
                  if varToSym.contains(n) && varToSym(n).contains(v) then
                    containsPointer = true
                    break
                }
                if containsPointer then
                  val cell = expr.variables.foldLeft(lhsCell) {
                    (c, v) =>
                      val cells: Set[DSC] = getCells(n, v)

                      cells.foldLeft(c) {
                        (c, p) =>
                          graph.mergeCells(c, p) // TODO this causing everything to collapse
                      }
                  }
                  val node = cell.node.get
                  graph.collapseNode(node)
                  graph.varToCell(n).update(variable, node.cells(0))
              }

      case MemoryAssign(memory, MemoryStore(mem, index, value: Variable, endian, size), label) =>
        val byteSize = (size.toDouble/8).ceil.toInt
        val addressPointee: DSC =
          if isGlobal(index, n, byteSize).isDefined then
            graph.getPointee(isGlobal(index, n, byteSize).get)
          else if isStack(index, n).isDefined then
            graph.getPointee(isStack(index, n).get)
          else
            index match
            case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
              assert(varToSym(n).contains(arg1))
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              visitPointerArithmeticOperation(n, DSN(Some(graph), None).cells(0), arg1, true, offset)
            case arg: Variable =>
              assert(varToSym(n).contains(arg))
              visitPointerArithmeticOperation(n, DSN(Some(graph), None).cells(0), arg, true)
            case _ => ???

        val valueCells = getCells(n, value)
        val result = valueCells.foldLeft(addressPointee) {
          (c, p) =>
            graph.mergeCells(p, c)
        }

      case _ =>
  }
  def analyze(): Any =
    val domain = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).reverse

    domain.foreach(visit)

    println(graph.formals)
    val results = graph.varToCell.keys.toSeq.sortBy(_.toShortString)
    results.foreach {
      pos =>
        println(pos)
        val tab = "    "
        graph.varToCell(pos).foreach {
          case (variable, cell) =>
            println(tab + variable.toString + " -> " + cell.toString)
        }
    }
    println(graph.pointTo)
}
