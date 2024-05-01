package analysis

import analysis.solvers.ForwardIDESolver
import ir.IRWalk.procedure
import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, GoTo, IndirectCall, Literal, LocalAssign, Memory, MemoryLoad, MemoryStore, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend}

import java.math.BigInteger

case class SymbolicAccess(accessor: Variable, symbolicBase: MemoryRegion2, offset: BigInt) {
  override def toString: String = s"SymbolicAccess($accessor, $symbolicBase, $offset)"
}

trait MemoryRegion2 {
  val regionIdentifier: String

  override def toString: String = s"MemoryRegion($regionIdentifier)"
}

case class StackRegion2(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryRegion2 {
  override def toString: String = s"Stack($regionIdentifier,  $size)"
}

case class HeapRegion2(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryRegion2 {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataRegion2(override val regionIdentifier: String, start: BigInt, size: BigInt) extends MemoryRegion2 {
  override def toString: String = s"Data($regionIdentifier, $start)"
}

case class UnknownRegion2(override val regionIdentifier: String, proc: Procedure) extends MemoryRegion2 {
  override def toString: String = s"Unknown($regionIdentifier)"
}

trait SymbolicAccessFunctions(constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends ForwardIDEAnalysis[SymbolicAccess, TwoElement, TwoElementLattice] {

  private val stackPointer = Register("R31", BitVecType(64))
  private val linkRegister = Register("R30", BitVecType(64))
  private val framePointer = Register("R29", BitVecType(64))
  private val mallocVariable = Register("R0", BitVecType(64))

  var mallocCount: Int = 0
  private def nextMallocCount = {
    mallocCount += 1
    s"malloc_$mallocCount"
  }

  var unknownCount: Int = 0
  private def nextunknownCount = {
    unknownCount += 1
    s"unknown_$unknownCount"
  }

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[TwoElement, TwoElementLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

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

  def unwrapPaddingAndSlicing(expr: Expr): Expr =
    expr match
      case Extract(end, start, body) if start == 0 && end == 32 => unwrapPaddingAndSlicing(body)
      case ZeroExtend(extension, body) => unwrapPaddingAndSlicing(body)
      case _ => expr

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackRegion2(regionIdentifier, parent, size) => Map()
          case _ => Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: IndirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackRegion2(regionIdentifier, parent, size) => Map()
          case _ =>
            if value.accessor.name == "R29" then
              Map()
            else Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesCallToAfterCall(call: DirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackRegion2(regionIdentifier, parent, size) => Map(d -> IdEdge())
          case _ => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    val bitvecnegative: BigInt = new BigInt(new BigInteger("9223372036854775808")) // negative 64 bit integer

    n match
      case LocalAssign(variable, rhs, maybeString) =>
        val expr = unwrapPaddingAndSlicing(rhs)
        expr match
          case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
            && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= bitvecnegative =>
            d match
              case Left(value) if value.accessor == variable => Map()
              case Left(value) => Map(d -> IdEdge())
              case Right(_) =>
                val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
                Map(d -> IdEdge(), Left(SymbolicAccess(variable, StackRegion2(s"Stack_${procedure(n).name}", procedure(n), -size), 0)) -> ConstEdge(TwoElementTop))
          case BinaryExpr(op, arg1: Variable, arg2) if evaluateExpression(arg2, constProp(n)).isDefined =>
            d match
              case Left(value) if value.accessor == arg1 =>
                val offsetUpdate = evaluateExpression(arg2, constProp(n)).get.value
                val result: Map[DL, EdgeFunction[TwoElement]] = Map(Left(SymbolicAccess(variable, value.symbolicBase, value.offset + offsetUpdate)) -> ConstEdge(TwoElementTop))
                if value.accessor != variable then
                  result + (d -> IdEdge())
                else
                  result
              case Left(value) if value.accessor == variable => Map()
              case _ => Map(d -> IdEdge())
          case arg:Variable =>
            d match
              case Left(value) if value.accessor == arg =>
                val result: Map[DL, EdgeFunction[TwoElement]] = Map(Left(SymbolicAccess(variable, value.symbolicBase, value.offset)) -> ConstEdge(TwoElementTop))
                if value.accessor != variable then
                  result + (d -> IdEdge())
                else
                  result
              case Left(value) if value.accessor == variable => Map()
              case _ => Map(d -> IdEdge())
          case MemoryLoad(mem, index, endian, size) =>
            d match
              case Left(value) if value.accessor == variable => Map()
              case Left(value) => Map(d -> IdEdge())
              case Right(_) => Map(d -> IdEdge(), Left(SymbolicAccess(variable, UnknownRegion2(nextunknownCount, procedure(n)), 0)) -> ConstEdge(TwoElementTop))
          case _ =>
            d match
              case Left(value) if value.accessor == variable => Map()
              case _ => Map(d -> IdEdge())
      case DirectCall(proc, ret, label) if proc.name == "malloc" =>
        d match
          case Left(value) if value.accessor == mallocVariable => Map()
          case Left(value) => Map(d -> IdEdge())
          case Right(value) =>
            val size: BigInt = evaluateExpression(mallocVariable, constProp(n)) match
              case Some(value) => value.value
              case None => -1
            Map(d -> IdEdge(), Left(SymbolicAccess(mallocVariable, HeapRegion2(nextMallocCount, procedure(n), size), 0)) -> ConstEdge(TwoElementTop))
      case _ => Map(d -> IdEdge())
}

class SymbolicAccessAnalysis(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])
  extends ForwardIDESolver[SymbolicAccess, TwoElement, TwoElementLattice](program), SymbolicAccessFunctions(constProp)