//package analysis
//
//import analysis.solvers.UnionFindSolver
//import ir.{BinaryExpr, BitVecLiteral, CFGPosition, Expr, Extract, InterProcIRCursor, IntraProcIRCursor, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Program, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, computeDomain}
//import specification.{ExternalFunction, SpecGlobal}
//
//import scala.collection.mutable
//import scala.util.boundary, boundary.break
//
//class RegionBuilder(program: Program, symResults: Map[CFGPosition, Map[SymbolicAccess, TwoElement]],
//                    constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
//                    globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
//                    externalFunctions: Set[ExternalFunction]) extends Analysis[Any] {
//
//  val graphs: mutable.Map[Procedure, DSG] = mutable.Map()
//  val nodes: mutable.Map[MemoryRegion2, DSN] = mutable.Map()
//  val solver: UnionFindSolver[StTerm] = UnionFindSolver()
//  val loadStore: mutable.Set[CFGPosition] = mutable.Set()
//  val pointTo: mutable.Map[DSC, DSC] = mutable.Map()
//
//
//  private def replaceInPointTo(oldCell: DSC, newCell:DSC) =
//    pointTo.foreach{
//      case (pointer, pointee) =>
//        if pointee.equals(oldCell) then
//          pointTo.update(pointer, newCell)
//    }
//
//  private def getPointee(cell: DSC): DSC =
//    if !pointTo.contains(cell) then
//      val node = DSN(None, None)
//      pointTo.update(cell, node.cells(0))
//    pointTo(cell)
//
//
//
//  private def earlyCollapse(node: DSN) : Unit =
//    node.collapsed = true
//    node.cells.clear()
//    node.addCell(0, 0)
//
//  private def collapseNode(node: DSN): Unit =
//    val e = DSC(None, 0)
//    val cell = node.cells.foldLeft(e){
//      (c, field) => mergeCells(c, getPointee(field._2))
//    }
//    earlyCollapse(node)
//    pointTo.update(node.cells(0), cell)
//
//
//  private def mergeCells(cell1: DSC, cell2: DSC): DSC =
//    if (incompatibleTypes(cell1, cell2)) then
//      collapseNode(cell2.node.get)
//
//    if cell2.node.get.region.isEmpty && cell1.node.isDefined  then
//      cell2.node.get.region = cell1.node.get.region
//
//    if cell2.node.get.collapsed then
//      if cell1.node.isDefined then
//        cell1.node.get.cells.foreach{
//          case (offset, cell) =>
//            if pointTo.contains(cell) then
//              if pointTo.contains(cell2.node.get.cells(0)) then
//                mergeCells(getPointee(cell), getPointee(cell2.node.get.cells(0)))
//              else
//                pointTo.update(cell2.node.get.cells(0), getPointee(cell))
//              pointTo.remove(cell)
//            replaceInPointTo(cell, cell2.node.get.cells(0))
//        }
//        cell2.node.get.cells(0)
//      else
//        if pointTo.contains(cell1) then
//          if pointTo.contains(cell2.node.get.cells(0)) then
//            mergeCells(getPointee(cell1), getPointee(cell2.node.get.cells(0)))
//          else
//            pointTo.update(cell2.node.get.cells(0), getPointee(cell1))
//          pointTo.remove(cell1)
//        replaceInPointTo(cell1, cell2.node.get.cells(0))
//        cell2.node.get.cells(0)
//    else
//      cell1.node.get.cells.foreach{
//        case (offset, cell) =>
//          if pointTo.contains(cell) then
//            if pointTo.contains(cell2.node.get.cells(offset)) then
//              mergeCells(getPointee(cell), getPointee(cell2.node.get.cells(offset)))
//          else
//            pointTo.update(cell2.node.get.cells(offset), getPointee(cell))
//          pointTo.remove(cell)
//          replaceInPointTo(cell, cell2.node.get.cells(offset))
//
//
//      }
//      cell2
//
//
//  private def incompatibleTypes(cell1: DSC, cell2: DSC): Boolean =
//    if cell2.node.get.collapsed then
//      return false
//    else if cell1.node.isEmpty then
//      return true // TODO not sure about this
//    else if cell1.node.get.cells.size != cell2.node.get.cells.size then
//      return true
//    else
//      (cell1.node.get.cells zip cell2.node.get.cells).foreach {
//        case ((o1, c1), (o2, c2)) =>
//          if o1 != o2 || !c1.accessedSizes.equals(c2.accessedSizes) then
//            return true
//      }
//    false
//
//  private def multiAccessesSizes(node: DSN): Boolean =
//    node.cells.foreach(
//      c =>
//        val cell = c._2
//        if cell.accessedSizes.size > 1 then
//          return true
//    )
//    false
//
//
//  private val swappedOffsets = globalOffsets.map(_.swap)
//
//  val globalMapping: mutable.Map[(BigInt, BigInt), DSN] = globals.foldLeft(mutable.Map[(BigInt, BigInt), DSN]()) {
//    (m, global) =>
//      var address: BigInt = global.address
//      if swappedOffsets.contains(address) then
//        address = swappedOffsets(address)
//      m + ((address, address + global.size) -> DSN(None, Some(DataRegion2(global.name, global.address, global.size))))
//  }
//
//  externalFunctions.foreach(
//    external =>
//      var address: BigInt = external.offset
//      if swappedOffsets.contains(address) then
//        address = swappedOffsets(address)
//      globalMapping.update((address, address), DSN(None, Some(DataRegion2(external.name, address, 0))))
//  )
//
//
//  val varToSym: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]] = symResults.foldLeft(Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]()) {
//    (outerMap, syms) =>
//      val position = syms._1
//      val innerMap = syms._2.foldLeft(Map[Variable, Set[SymbolicAccess]]()) {
//        (m, access) =>
//          val b = position
//          if (m.contains(access._1.accessor)) then
//            m + (access._1.accessor -> (m(access._1.accessor) + access._1))
//          else
//            m + (access._1.accessor -> Set(access._1))
//      }
//
//      outerMap + (position -> innerMap)
//  }
//
//  private def isGlobal(address: BigInt): Option[DSN] =
//    for (elem <- globalMapping) {
//      val range = elem._1
//      if address >= range._1 && address <= range._2 then
//        return Some(elem._2)
//    }
//    None
//
//
//
//  private def buildNode(sym: SymbolicAccess, offset: BigInt, size: Int): Unit =
//    val region = sym.symbolicBase
//    val newOffset = sym.offset + offset
//    val proc = region match
//      case DataRegion2(regionIdentifier, start, size) => ???
//      case HeapRegion2(regionIdentifier, proc, size) => proc
//      case StackRegion2(regionIdentifier, proc, size) => proc
//      case UnknownRegion2(regionIdentifier, proc) => proc
//    val graph = graphs(proc)
//    val node = graph.addNode(region, newOffset, size)
//    nodes.update(region, node)
//
//  private def getCell(sym: SymbolicAccess, offset: BigInt): DSC =
//    val region = sym.symbolicBase
//    val newOffset = sym.offset + offset
//    val node = nodes(region)
//    if node.collapsed then
//      node.cells(0)
//    else
//      node.cells(newOffset)
//
//  private def visit(n: CFGPosition): Unit =
//    n match
//      case LocalAssign(variable, expr, maybeString) =>
//        expr match
//          case MemoryLoad(mem, index, endian, size) =>
//            val byteSize = (size.toDouble/8).ceil.toInt
//            if evaluateExpression(index, constProp(n)).isDefined && isGlobal(evaluateExpression(index, constProp(n)).get.value).isDefined then
//              val address = evaluateExpression(index, constProp(n)).get.value
//              val node: DSN = isGlobal(evaluateExpression(index, constProp(n)).get.value).get
//              val baseAddress = node.region.get.asInstanceOf[DataRegion2].start
//              val offset = address - baseAddress
//              node.addCell(offset, size)
//              loadStore.add(n)
//            else
//              index match
//                case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//                  assert(varToSym(n).contains(arg1))
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => buildNode(sym, offset, byteSize))
//                  loadStore.add(n)
//                case arg: Variable =>
//                  assert(varToSym(n).contains(arg))
//                  varToSym(n)(arg).foreach(sym => buildNode(sym, 0, byteSize))
//                  loadStore.add(n)
//                case _ => ???
//          case _ =>
//      case MemoryAssign(mem, MemoryStore(mem2, index, value, endian, size), label) =>
//        val byteSize = (size.toDouble/8).ceil.toInt
//        if evaluateExpression(index, constProp(n)).isDefined && isGlobal(evaluateExpression(index, constProp(n)).get.value).isDefined then
//          val address = evaluateExpression(index, constProp(n)).get.value
//          val node: DSN = isGlobal(evaluateExpression(index, constProp(n)).get.value).get
//          val baseAddress = node.region.get.asInstanceOf[DataRegion2].start
//          val offset = address - baseAddress
//          node.addCell(offset, size)
//          loadStore.add(n)
//        else
//          index match
//            case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//              assert(varToSym(n).contains(arg1))
//              val offset = evaluateExpression(arg2, constProp(n)).get.value
//              varToSym(n)(arg1).foreach(sym => buildNode(sym, offset, byteSize))
//              loadStore.add(n)
//              value match
//                case BinaryExpr(op, arg1: Variable, arg2) if varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => buildNode(sym, offset, byteSize))
//                case variable: Variable if varToSym(n).contains(variable) =>
//                  varToSym(n)(variable).foreach(sym => buildNode(sym, 0, byteSize))
//                case _ =>
//            case arg: Variable =>
//              assert(varToSym(n).contains(arg))
//              varToSym(n)(arg).foreach(sym => buildNode(sym, 0, byteSize))
//              loadStore.add(n)
//              value match
//                case BinaryExpr(op, arg1: Variable, arg2) if varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => buildNode(sym, offset, byteSize))
//                case variable: Variable if varToSym(n).contains(variable) =>
//                  varToSym(n)(variable).foreach(sym => buildNode(sym, 0, byteSize))
//
//                case _ =>
//            case _ => ???
//      case _ =>
//
//  private def coolVisit(n: CFGPosition): Unit =
//    n match
//      case LocalAssign(variable, expr, maybeString) =>
//        val pointers : mutable.Set[DSC] = mutable.Set()
//        varToSym(n).getOrElse(variable, Set()).foreach(sym => pointers.add(getCell(sym, 0)))
//
//        expr match
//          case MemoryLoad(mem, index, endian, size) =>
//            val byteSize = (size.toDouble / 8).ceil.toInt
//            val pointees: mutable.Set[DSC] = mutable.Set()
//            if evaluateExpression(index, constProp(n)).isDefined && isGlobal(evaluateExpression(index, constProp(n)).get.value).isDefined then
//              val address = evaluateExpression(index, constProp(n)).get.value
//              val node: DSN = isGlobal(evaluateExpression(index, constProp(n)).get.value).get
//              val baseAddress = node.region.get.asInstanceOf[DataRegion2].start
//              val offset = address - baseAddress
//              if node.collapsed then pointees.add(getPointee(node.cells(0))) else pointees.add(getPointee(node.cells(offset)))
//            else
//              index match
//                case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//                  assert(varToSym(n).contains(arg1))
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => pointees.add(getPointee(getCell(sym, offset))))
//                case arg: Variable =>
//                  assert(varToSym(n).contains(arg))
//                  varToSym(n)(arg).foreach(sym => pointees.add(getPointee(getCell(sym, 0))))
//                case _ => ???
//            pointees.foreach(
//              pointee =>
//                pointers.foreach(
//                  pointer => mergeCells(pointer, pointee)
//                )
//            )
//          case _ =>
//      case MemoryAssign(mem, MemoryStore(mem2, index, value, endian, size), label) =>
//        val pointees : mutable.Set[DSC] = mutable.Set()
//        val pointers: mutable.Set[DSC] = mutable.Set()
//        val byteSize = (size.toDouble / 8).ceil.toInt
//        if evaluateExpression(index, constProp(n)).isDefined && isGlobal(evaluateExpression(index, constProp(n)).get.value).isDefined then
//          val address = evaluateExpression(index, constProp(n)).get.value
//          val node: DSN = isGlobal(evaluateExpression(index, constProp(n)).get.value).get
//          val baseAddress = node.region.get.asInstanceOf[DataRegion2].start
//          val offset = address - baseAddress
//          if node.collapsed then pointees.add(getPointee(node.cells(0))) else pointees.add(getPointee(node.cells(offset)))
//        else
//          index match
//            case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
//              assert(varToSym(n).contains(arg1))
//              val offset = evaluateExpression(arg2, constProp(n)).get.value
//              varToSym(n)(arg1).foreach(sym => pointees.add(getPointee(getCell(sym, offset))))
//              value match
//                case BinaryExpr(op, arg1: Variable, arg2) if varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => pointers.add(getCell(sym, offset)))
//                case variable: Variable if varToSym(n).contains(variable) =>
//                  varToSym(n)(variable).foreach(sym => pointers.add(getCell(sym, 0)))
//                case _ =>
//            case arg: Variable =>
//              assert(varToSym(n).contains(arg))
//              varToSym(n)(arg).foreach(sym => pointees.add(getPointee(getCell(sym, 0))))
//              value match
//                case BinaryExpr(op, arg1: Variable, arg2) if varToSym(n).contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//                  val offset = evaluateExpression(arg2, constProp(n)).get.value
//                  varToSym(n)(arg1).foreach(sym => pointers.add(getCell(sym, offset)))
//                case variable: Variable if varToSym(n).contains(variable) =>
//                  varToSym(n)(variable).foreach(sym => pointers.add(getCell(sym, 0)))
//
//                case _ =>
//            case _ => ???
//        pointees.foreach(
//          pointee =>
//            pointers.foreach(
//              pointer => mergeCells(pointer, pointee)
//            )
//        )
//      case _ =>
//
//
//  def analyze(): Any = ???
////    program.procedures.foreach(proc => graphs.update(proc, DSG(proc)))
////    computeDomain(InterProcIRCursor, Set(program.mainProcedure)).foreach(visit)
////    nodes.values.foreach(earlyCollapse)
////    loadStore.foreach(coolVisit)
////    pointTo.foreach{
////      case (cell1, cell2) =>
////        println(cell1.toString + " -> " + cell2.toString)
////    }
//}
