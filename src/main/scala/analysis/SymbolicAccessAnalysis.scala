package analysis

import analysis.solvers.ForwardIDESolver
import ir.IRWalk.procedure
import ir.{BVADD, BinaryExpr, BitVecLiteral, BitVecType, CFGPosition, DirectCall, Expr, Extract, GoTo, IndirectCall, Literal, Assign, Memory, MemoryLoad, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend}

import java.math.BigInteger

case class SymbolicAccess(accessor: Variable, symbolicBase: MemoryLocation, offset: BigInt) {
  override def toString: String = s"SymbolicAccess($accessor, $symbolicBase, $offset)"
}

trait MemoryLocation {
  val regionIdentifier: String

  override def toString: String = s"MemoryRegion($regionIdentifier)"
}

case class StackLocation(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryLocation {
  override def toString: String = s"Stack($regionIdentifier,  $size)"
}

case class HeapLocation(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryLocation {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataLocation(override val regionIdentifier: String, start: BigInt, size: BigInt) extends MemoryLocation {
  override def toString: String = s"Data($regionIdentifier, $start, $size)"
}

case class UnkownLocation(override val regionIdentifier: String, proc: Procedure) extends MemoryLocation {
  override def toString: String = s"Unknown($regionIdentifier)"
}

/**
 * environment transformers for SAA or symbolic access analysis
 * Combination of reaching definitions and constant propagation
 * elements in D are symbolic accesses of the form (variable, symbolic base, concrete offset)
 * lattice L is a binary lattice with top and bottom
 */
trait SymbolicAccessFunctions(constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends ForwardIDEAnalysis[SymbolicAccess, TwoElement, TwoElementLattice] {

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)
  private val mallocVariable = Register("R0", 64)

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


  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackLocation(regionIdentifier, parent, size) => Map()
          case _ => Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: IndirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackLocation(regionIdentifier, parent, size) => Map()
          case _ =>
            if value.accessor.name == "R29" then
              Map()
            else Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesCallToAfterCall(call: DirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case StackLocation(regionIdentifier, parent, size) => Map(d -> IdEdge())
          case _ => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    n match
      case Assign(variable, rhs, maybeString: Option[String]) =>
        val expr = unwrapPaddingAndSlicing(rhs)
        expr match
          case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) && arg1.equals(stackPointer)
            && evaluateExpression(arg2, constProp(n)).isDefined && evaluateExpression(arg2, constProp(n)).get.value >= BITVECNEGATIVE =>
            d match
              case Left(value) if value.accessor == variable => Map()
              case Left(value) => Map(d -> IdEdge())
              case Right(_) =>
                val size = twosComplementToDec(decToBinary(evaluateExpression(arg2, constProp(n)).get.value))
                Map(d -> IdEdge(), Left(SymbolicAccess(variable, StackLocation(s"Stack_${procedure(n).name}", procedure(n), -size), 0)) -> ConstEdge(TwoElementTop))
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
              case Right(_) => Map(d -> IdEdge(), Left(SymbolicAccess(variable, UnkownLocation(nextunknownCount, procedure(n)), 0)) -> ConstEdge(TwoElementTop))
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
            Map(d -> IdEdge(), Left(SymbolicAccess(mallocVariable, HeapLocation(nextMallocCount, procedure(n), size), 0)) -> ConstEdge(TwoElementTop))
      case _ => Map(d -> IdEdge())
}

class SymbolicAccessAnalysis(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])
  extends ForwardIDESolver[SymbolicAccess, TwoElement, TwoElementLattice](program), SymbolicAccessFunctions(constProp)