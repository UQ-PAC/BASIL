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


  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]] = symResults.foldLeft(Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]()) {
    (outerMap, syms) =>
      val position = syms._1
      val innerMap = syms._2.foldLeft(Map[Variable, Set[SymbolicAccess]]()) {
        (m, access) =>
          val b = position
          if (m.contains(access._1.accessor)) then
            m + (access._1.accessor -> (m(access._1.accessor) + access._1))
          else
            m + (access._1.accessor -> Set(access._1))
      }

      outerMap + (position -> innerMap)
  }

  val stackMapping: Map[BigInt, DSN] =
    computeDomain(IntraProcIRCursor, Set(proc)).foldLeft(Map[BigInt, DSN]()) {
      (results, pos) => stackBuilder(pos, results)
    }

  def stackBuilder(pos: CFGPosition, m: Map[BigInt, DSN]): Map[BigInt, DSN] = {
    pos match
      case LocalAssign(variable: Variable, expr: Expr, _) =>
        expr match
          case MemoryLoad(mem, index, endian, size) =>
            val byteSize = (size.toDouble/8).ceil.toInt
            index match
              case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) &&  varToSym(pos).contains(arg1) &&
                evaluateExpression(arg2, constProp(pos)).isDefined  =>
                  var offset = evaluateExpression(arg2, constProp(pos)).get.value
                  varToSym(pos)(arg1).foldLeft(m){
                    (m, sym) =>
                      sym match
                        case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), symOffset) =>
                          offset = offset + symOffset
                          if m.contains(offset) then
                            m(offset).addCell(0, byteSize)
                            m
                          else
                            val node = DSN(Some(graph), Some(StackRegion2(pos.toShortString, proc, byteSize)))
                            node.addCell(0, byteSize)
                            m + (offset -> node)
                        case _=> m
                  }
              case arg: Variable if varToSym.contains(pos) &&  varToSym(pos).contains(arg) =>
                varToSym(pos)(arg).foldLeft(m){
                  (m, sym) =>
                    sym match
                      case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), offset) =>
                        if m.contains(offset) then
                          m(offset).addCell(0, byteSize)
                          m
                        else 
                          val node = DSN(Some(graph), Some(StackRegion2(pos.toShortString, proc, byteSize)))
                          node.addCell(0, byteSize)
                          m + (offset -> node)
                      case _=> m
                }
              case _ => m
          case _ => m
      case MemoryAssign(mem, MemoryStore(mem2, index, value, endian, size), label) =>
        val byteSize = (size.toDouble / 8).ceil.toInt
        index match
          case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(pos) && varToSym(pos).contains(arg1) &&
            evaluateExpression(arg2, constProp(pos)).isDefined =>
            var offset = evaluateExpression(arg2, constProp(pos)).get.value
            varToSym(pos)(arg1).foldLeft(m) {
              (m, sym) =>
                sym match
                  case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), symOffset) =>
                    offset = offset + symOffset
                    if m.contains(offset) then
                      m(offset).addCell(0, byteSize)
                      m
                    else
                      val node = DSN(Some(graph), Some(StackRegion2(pos.toShortString, proc, byteSize)))
                      node.addCell(0, byteSize)
                      m + (offset -> node)
                  case _ => m
            }
          case arg: Variable if varToSym.contains(pos) && varToSym(pos).contains(arg) =>
            varToSym(pos)(arg).foldLeft(m) {
              (m, sym) =>
                sym match
                  case SymbolicAccess(accessor, StackRegion2(regionIdentifier, proc, size), offset) =>
                    if m.contains(offset) then
                      m(offset).addCell(0, byteSize)
                      m
                    else
                      val node = DSN(Some(graph), Some(StackRegion2(pos.toShortString, proc, byteSize)))
                      node.addCell(0, byteSize)
                      m + (offset -> node)
                  case _ => m
            }
          case _ => m
      case _ => m

  }
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

  val graph = DSG(proc, constProp, globals, globalOffsets, externalFunctions, reachingDefs, writesTo)


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

  def visit(n: CFGPosition): Unit = {
    n match
      case DirectCall(proc, target, label) if proc.name == "malloc" =>
        val size: BigInt = evaluateExpression(mallocRegister, constProp(n)) match
          case Some(value) => value.value
          case None => 0
        val node = DSN(Some(graph), Some(HeapRegion2(nextMallocCount, proc, size)))
        graph.nodes.add(node)
        val cell = graph.mergeCells(graph.varToCell(n)(mallocRegister), node.cells(0))
        graph.varToCell(n).update(mallocRegister, cell)

      case LocalAssign(variable, expr, maybeString) =>
        val lhsCell = graph.varToCell(n)(variable)
        if isGlobal(expr, n).isDefined then
          val global = isGlobal(expr, n).get
          val result = graph.mergeCells(lhsCell, global)
          graph.varToCell(n).update(variable, result)
        else
          expr match
            case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
              && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= bitvecnegative =>
              val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
              val node = DSN(Some(graph), Some(StackRegion2("Stack_"+proc.name, proc, -size)))
              graph.nodes.add(node)
              val cell = graph.mergeCells(lhsCell, node.cells(0))
              graph.varToCell(n).update(variable, cell)
            case BinaryExpr(op, arg1: Variable, arg2) if varToSym.contains(n) &&  varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              val nodes: Set[DSN] = getNodes(n, arg1)
              nodes.foreach(_.addCell(offset, 0))
              val cell = nodes.foldLeft(lhsCell){
                (c, node) =>
                  var field = offset
                  node.addCell(offset, 0)
                  if node.collapsed then
                    field = 0
                  graph.mergeCells(c, node.cells(field)) // TODO this causing everything to collapse
              }
              graph.varToCell(n).update(variable, cell)

            case arg: Variable if varToSym(n).contains(arg) =>
              val cells = getCells(n, arg)

              val cell = cells.foldLeft(lhsCell){
                (c, p) =>
                  graph.mergeCells(c, p) // TODO this causing everything to collapse
              }
              graph.varToCell(n).update(variable, cell)

            case MemoryLoad(mem, index, endian, size) =>
              val byteSize = (size.toDouble/8).ceil.toInt
              if isGlobal(index, n, byteSize).isDefined then
                val global = isGlobal(index, n, byteSize).get
                val result = graph.mergeCells(lhsCell, graph.getPointee(global))
                graph.varToCell(n).update(variable, result)
              else
                index match
                  case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
                    assert(varToSym(n).contains(arg1))
                    val offset = evaluateExpression(arg2, constProp(n)).get.value
                    val nodes: Set[DSN] = getNodes(n, arg1)
                    nodes.foreach(_.addCell(offset, byteSize))
                    val cell = nodes.foldLeft(lhsCell){
                      (c, node) =>
                        var field = offset
                        node.addCell(offset, byteSize)
                        if node.collapsed then
                          field = 0
                        graph.mergeCells(c, graph.getPointee(node.cells(field))) // TODO this causing everything to collapse
                    }
                    graph.varToCell(n).update(variable, cell)
                  case arg: Variable =>
                    assert(varToSym(n).contains(arg))
                    val cells: Set[DSC] = getCells(n, arg)

                    val cell = cells.foldLeft(lhsCell){
                      (c, p) =>
                        graph.mergeCells(c, graph.getPointee(p)) // TODO this causing everything to collapse
                    }
                    graph.varToCell(n).update(variable, cell)
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
        val addressCell: DSC =
          if isGlobal(index, n, byteSize).isDefined then
            isGlobal(index, n, byteSize).get
          else
            index match
            case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
              assert(varToSym(n).contains(arg1))
              val offset = evaluateExpression(arg2, constProp(n)).get.value
              val nodes: Set[DSN] = getNodes(n, arg1)
              nodes.foreach(_.addCell(offset, byteSize))
              val cell = nodes.foldLeft(DSN(Some(graph), None).cells(0)) {
                (c, node) =>
                  var field = offset
                  node.addCell(offset, byteSize)
                  if node.collapsed then
                    field = 0
                  graph.mergeCells(c, node.cells(field)) // TODO this causing everything to collapse
              }
              cell
            case arg: Variable =>
              assert(varToSym(n).contains(arg))
              val cells: Set[DSC] = getCells(n, arg)
              val cell = cells.foldLeft(DSN(Some(graph), None).cells(0)) {
                (c, p) =>
                  graph.mergeCells(c, p) // TODO this causing everything to collapse
              }
              cell
            case _ => ???

        val valueCells = getCells(n, value)
        val result = valueCells.foldLeft(graph.getPointee(addressCell)) {
          (c, p) =>
            graph.mergeCells(p, c)
        }

        if reachingDefs(n).contains(value) then
          reachingDefs(n)(value).foreach (
            definition =>
              graph.varToCell(definition).update(value, result)
          )
        else
         graph.formals.update(value, result)

      case _ =>
  }
  def analyze(): Any =
    val domain = computeDomain(IntraProcIRCursor, Set(proc)).toSeq.sortBy(_.toShortString).reverse
    println(domain)

//    println(domain)
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
