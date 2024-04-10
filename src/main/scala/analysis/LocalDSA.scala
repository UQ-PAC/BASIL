//package analysis
//
//import ir.{Assert, Assume, BVADD, BinOp, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, Call, DirectCall, Expr, Extract, GoTo, IndirectCall, IntraProcIRCursor, Literal, LocalAssign, LocalVar, Memory, MemoryAssign, MemoryLoad, MemoryStore, NOP, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, computeDomain}
//
//import java.math.BigInteger
//import scala.annotation.{static, tailrec}
//
//
//class LocalDSA(val program: Program, val procedure: Procedure, constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], var symbolicAccesses: Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]) extends Analysis[Any] {
//  val graph: Graph = Graph(procedure)
//
//
//  private val stackPointer = Register("R31", BitVecType(64))
//  private val linkRegister = Register("R30", BitVecType(64))
//  private val framePointer = Register("R29", BitVecType(64))
//
//  val bitvecnegative: BigInt = new BigInt(new BigInteger("9223372036854775808"))
//
//
//  private val ignoreRegions: Set[Expr] = Set(linkRegister, framePointer)
//
//  val malloc_register = Register("R0", BitVecType(64))
//  private var localVarCount: Int = -1
//  private def getNextLocalVarName: String = {
//    localVarCount += 1
//    s"NormVar_$localVarCount"
//  }
//
////  for (i <- 0 to 31) {
////    graph.pointersToCells.update(Register(s"R$i", BitVecType(64)), graph.makeCell())
////  }
//
//  def visitBinaryLocalAssign(lhs: Variable, op: BinOp, arg1: Variable, offset: BigInt) = {
//    val cell = graph.getVariablePointee(arg1)
//    val node = cell.node.get
//    if node.isCollapsed then
//      graph.collapsePointer(lhs) // TODO ensure passing right memory region here
//    else if !node.isSeq /* && offset == 0 */ then // TODO here we are making everything with a offset a sequence
//      val size = cell.offset + offset + 8 // assuming bitvector of 64, all the fields that matter are pointers
//      node.updateSize(size)
//      graph.unify(lhs, node.cell(cell.offset + offset))
//    else
//      node.setSeq()
//      val size = node.size.gcd(cell.offset)
//      node.updateSize(size)
//      graph.unify(lhs, cell)
//  }
//
//  def atomicPointer(n: CFGPosition) : Unit = {
//    n match
//      case DirectCall(target: Procedure, returnTarget, label) if procedure.name.equals("malloc") =>
//          val cell = graph.makeCell(Some(symbolicAccesses(n)(malloc_register).head.symbolicBase))
//          graph.unify(malloc_register, cell)
//      // case _ => // TODO ignoring all other calls right now. Think about  semantics of a call
//      // should unify returns
//      case LocalAssign(variable, expr, maybeString) =>
//        expr match
//          case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
//            && evaluateExpression(arg2, constantProp(n)).isDefined && evaluateExpression(arg2, constantProp(n)).get.value >= bitvecnegative =>
//            // p = &x
//            val node = graph.makeNode(Some(symbolicAccesses(n)(variable).head.symbolicBase))
//            val cell = node.cell()
//            graph.unify(variable, cell)
////          case BinaryExpr(op, arg1: Variable, arg2) if symbolicAccesses(n).contains(arg1) && evaluateExpression(arg2, constantProp(n)).isDefined => // what TODO if can't evaluate arg2
////            // variable = arg1 + (c = 0 * m) + arg2
////            val offset: BigInt = evaluateExpression(arg2, constantProp(n)).get.value
////            visitBinaryLocalAssign(variable, op, arg1, offset)
////          case vari: Variable if symbolicAccesses(n).contains(vari) => // TODO actually check if q is a pointer
////            // p = q
////            val cell = graph.getVariablePointee(vari)
////            val node = cell.node.get
////            if node.isCollapsed then
////              graph.collapsePointer(variable) // TODO ensure passing right memory region here
////            else if !node.isSeq then
////              val size = cell.offset + 8 // assume all sizes are the same for now since we don't know sizes of everything
////              node.updateSize(size)
////              graph.unify(variable, cell)
////            else
////              node.setSeq()
////              graph.unify(variable, cell) // c is zero here
//          case MemoryLoad(mem, index, endian, size) =>
//              // q = *p
//            index match
//              case BinaryExpr(op, arg1: Variable, arg2) if symbolicAccesses(n).contains(arg1) && evaluateExpression(arg2, constantProp(n)).isDefined =>
//                val local = symbolicAccesses(n).keySet.reduce(
//                  (v1, v2) =>
//                    if v1.name.startsWith("NormVar") then
//                      v1
//                    else if v2.name.startsWith("NormVar") then
//                      v2
//                    else
//                      v1
//                )
//                assert(local.name.startsWith("NormVar"))
//                visitBinaryLocalAssign(local, op, arg1, evaluateExpression(arg2, constantProp(n)).get.value)
//                graph.getCellPointee(graph.getVariablePointee(local)).unify(graph.getVariablePointee(variable))
//              case vari: Variable if symbolicAccesses(n).contains(vari) =>
//                graph.getCellPointee(graph.getVariablePointee(vari)).unify(graph.getVariablePointee(variable))
//              case _ =>
//          case _ =>
//      case MemoryAssign(me, MemoryStore(mem, index, value, endian, size), label) =>
//          //*p = q
//        index match
//          case BinaryExpr(op, arg1: Variable, arg2) if symbolicAccesses(n).contains(arg1) =>
//            val local = symbolicAccesses(n).keySet.reduce(
//              (v1, v2) =>
//                if v1.name.startsWith("NormVar") then
//                  v1
//                else if v2.name.startsWith("NormVar") then
//                  v2
//                else
//                  v1
//            )
//            assert(local.name.startsWith("NormVar"))
//            visitBinaryLocalAssign(local, op, arg1, evaluateExpression(arg2, constantProp(n)).get.value)
//            graph.getCellPointee(graph.getVariablePointee(local)).
//              unify(graph.getVariablePointee(value.variables.head))
//          case vari: Variable if symbolicAccesses(n).contains(vari) =>
//            graph.getCellPointee(graph.getVariablePointee(vari)).unify(graph.getVariablePointee(value.variables.head))
//          case _ =>
//      case _ =>
//    }
//
//    def analyze(): Any = {
//      computeDomain(IntraProcIRCursor, Set(procedure)).foreach(atomicPointer)
//
////      println(graph.pointersToCells)
////      println(graph.pointsToRelations)
////      println(graph.nodes)
//    }
//}
