//package analysis
//
//import analysis.solvers.{SimplePushDownWorklistFixpointSolver, SimpleWorklistFixpointSolver}
//import ir.IRWalk.procedure
//import ir.{Assert, Assume, BVADD, BVAND, BVASHR, BVBinOp, BVCOMP, BVCONCAT, BVEQ, BVLSHR, BVMUL, BVNAND, BVNEQ, BVNOR, BVOR, BVSDIV, BVSGE, BVSGT, BVSHL, BVSLE, BVSLT, BVSMOD, BVSREM, BVSUB, BVUDIV, BVUGE, BVUGT, BVULE, BVULT, BVUREM, BVXNOR, BVXOR, BinOp, BinaryExpr, BitVecLiteral, BitVecType, BoolBinOp, CFGPosition, Call, DirectCall, Expr, Extract, GoTo, IntBinOp, IntraProcIRCursor, Literal, LocalAssign, LocalVar, Memory, MemoryAssign, MemoryLoad, MemoryStore, NOP, Procedure, Program, Register, Repeat, SignExtend, Statement, UnaryExpr, Variable, ZeroExtend, computeDomain}
//import specification.SpecGlobal
//import util.Logger
//
//import java.math.BigInteger
//import scala.collection.mutable
//import scala.math.BigInt
//
//case class SymbolicAccess(symbolicBase: MemoryRegion2, offset: BigInt) {
//  override def toString: String = s"SymbolicAccess($symbolicBase, $offset)"
//}
//
//class PrePass(program: Program,
//              constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], globals: Set[SpecGlobal], globalAddresses: Map[BigInt, String], globalOffsets: Map[BigInt, BigInt])
//
//  extends Analysis[Map[CFGPosition, Map[Variable, Set[SymbolicAccess]]]],
//    IRIntraproceduralForwardDependencies,
//    SimpleWorklistFixpointSolver[CFGPosition, Map[Variable, Set[SymbolicAccess]],
//      MapLattice[Variable, Set[SymbolicAccess], PowersetLattice[SymbolicAccess]]] {
//
//  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, program.procedures).toSet
//  val lattice: MapLattice[CFGPosition, Map[Variable, Set[SymbolicAccess]],
//    MapLattice[Variable, Set[SymbolicAccess], PowersetLattice[SymbolicAccess]]] = MapLattice(MapLattice(PowersetLattice()))
//
//  var mallocCount: Int
//  = 0
//  private var stackCount: Int = 0
//
//  private def nextMallocCount = {
//    mallocCount += 1
//    s"malloc_$mallocCount"
//  }
//
//  private def nextStackCount = {
//    stackCount += 1
//    s"stack_$stackCount"
//  }
//
//  private var localVarCount: Int = -1
//
//  private def getNextLocalVarName: String = {
//    localVarCount += 1
//    s"NormVar_$localVarCount"
//  }
//
//
//  private val stackPointer = Register("R31", BitVecType(64))
//  private val linkRegister = Register("R30", BitVecType(64))
//  private val framePointer = Register("R29", BitVecType(64))
//  private val mallocVariable = Register("R0", BitVecType(64))
//
//  def updateOffsets(variable: Variable, arg: Variable, offsetChange:BigInt, op: BinOp, s: Map[Variable, Set[SymbolicAccess]]): Map[Variable, Set[SymbolicAccess]] = {
//    val newSyms: mutable.Set[SymbolicAccess] = mutable.Set()
//    s(arg).foreach(
//      sym =>
//        op match
//          case BVADD => newSyms.add(SymbolicAccess(sym.symbolicBase, sym.offset + offsetChange))
//          case BVSUB => newSyms.add(SymbolicAccess(sym.symbolicBase, sym.offset - offsetChange))
//          case _ =>  ???// check if this happens often
//    )
//    s + (variable -> newSyms.toSet)
//  }
//
//
//  def decToBinary(n: BigInt): Array[Int] = {
//    val binaryNum: Array[Int] = new Array[Int](64)
//    var i = 0
//    var num = n
//    while (num > 0) {
//      binaryNum(i) = (num % BigInt(2)).intValue
//      num = num / 2
//      i += 1
//    }
//    binaryNum
//  }
//
//  def twosComplementToDec(binary: Array[Int]): BigInt = {
//    var result: BigInt = BigInt(0)
//    var counter: Int = 0
//    binary.foreach(
//      n =>
//        if counter == binary.length - 1 && n == 1 then
//          result = result - BigInt(2).pow(counter)
//        else if n == 1 then
//          result = result + BigInt(2).pow(counter)
//        counter += 1
//    )
//    result
//  }
//
//  def transfer(n: CFGPosition, symbolicAccesses: Map[Variable, Set[SymbolicAccess]]): Map[Variable, Set[SymbolicAccess]] = {
//
//    val bitvecnegative: BigInt = new BigInt(new BigInteger("9223372036854775808")) //"18446744073709551615"
//
//    val s = symbolicAccesses.filter((v, se) =>
//      !v.name.startsWith("NormVar")
//    )
//
//    n match
//      case LocalAssign(variable, expr, maybeString) =>
//        expr match
//          case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
//            && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= bitvecnegative =>
//            val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
//            s + (variable -> Set(SymbolicAccess(StackRegion2(s"Stack_${procedure(n).name}", procedure(n), BitVecLiteral(-size.intValue, 64)), 0)))
//          case BinaryExpr(op, arg1: Variable, arg2) if s.contains(arg1) => // arg1 is a symbolic access variable
//            evaluateExpression(arg2, constProp(n)) match
//              case Some(value) => // arg2 is some constant
//                updateOffsets(variable, arg1, value.value, op, s)
//              case None => // couldn't evaluate R2 to a constant
//                arg2 match
//                  case vari:Variable if s.contains(vari) =>
//                    evaluateExpression(arg1, constProp(n)) match
//                      case Some(value) =>
//                        updateOffsets(variable, vari, value.value, op, s)
//                      case None => s - variable
//                  case _ =>
//                    s - variable
//          case BinaryExpr(op, arg1, arg2: Variable) if s.contains(arg2) =>
//            evaluateExpression(arg1, constProp(n)) match
//              case Some(value) => // arg1 is some constant
//                updateOffsets(variable, arg2, value.value, op, s)
//              case None => s - variable // couldn't evaluate R1 to a constant
//          case vari: Variable if s.contains(vari) =>
//            s + (variable -> s(vari))
//          case MemoryLoad(mem, index, endian, size) =>
//            index match
//              case BinaryExpr(op, arg1: Variable, arg2) if s.contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//                val local = LocalVar(getNextLocalVarName, BitVecType(64))
//                updateOffsets(local, arg1, evaluateExpression(arg2, constProp(n)).get.value, op, s)
//              case _ => s
//          case _ if s.contains(variable) =>
//            s - variable
//          case _ => s
//      case directCall: DirectCall if directCall.target.name == "malloc" =>
//        nextMallocCount
//        evaluateExpression(mallocVariable, constProp(n)) match
//          case Some(value) =>
//            s + (mallocVariable ->  Set(SymbolicAccess(HeapRegion2(s"Malloc-${mallocCount}", value), 0)))
//          case None =>
//            s + (mallocVariable -> Set(SymbolicAccess(HeapRegion2(s"Malloc-${mallocCount}", BitVecLiteral(-1, 64)), 0)))
//      case MemoryAssign(mem, MemoryStore(m, index, value, endian, size), label) =>
//        index match
//          case BinaryExpr(op, arg1: Variable, arg2) if s.contains(arg1) && evaluateExpression(arg2, constProp(n)).isDefined =>
//            val local = LocalVar(getNextLocalVarName, BitVecType(64))
//            updateOffsets(local, arg1, evaluateExpression(arg2, constProp(n)).get.value, op, s)
//          case _ => s
//      case _ => s
//  }
//
//
////  override def analyze() = {
////    val results = super.analyze()
////    var offsetMapping
////    results.foreach(
////
////    )
////    results
////  }
//}
//
//trait MemoryRegion2 {
//  val regionIdentifier: String
//}
//
//case class StackRegion2(override val regionIdentifier: String, parent: Procedure, size: BitVecLiteral) extends MemoryRegion2 {
//  override def toString: String = s"Stack($regionIdentifier, ${parent.name}, $size)"
////  ${if symbolicAccess.isDefined then s", ${symbolicAccess.get}" else ""}
//}
//
//case class HeapRegion2(override val regionIdentifier: String, size: BitVecLiteral) extends MemoryRegion2 {
//  override def toString: String = s"Heap($regionIdentifier, $size)"
//}
//
//case class DataRegion2(override val regionIdentifier: String, start: BitVecLiteral) extends MemoryRegion2 {
//  override def toString: String = s"Data($regionIdentifier, $start)"
//}
//
//
//
