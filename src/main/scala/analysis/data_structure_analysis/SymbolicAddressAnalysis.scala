package analysis.data_structure_analysis

import ir.eval.BitVectorEval.{bv2SignedInt, isNegative}
import analysis.solvers.ForwardIDESolver
import analysis.*
import ir.*

import java.math.BigInteger

case class SymbolicAddress(accessor: Variable, symbolicBase: MemoryLocation, offset: BigInt) {
  override def toString: String = s"SymbolicAddress($accessor, $symbolicBase, $offset)"
}

trait MemoryLocation {
  val regionIdentifier: String
}


trait GlobalLocation {
  val start: BigInt
  val size: BigInt
}

case class StackLocation(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryLocation {
  override def toString: String = s"Stack($regionIdentifier,  $size)"
}

case class HeapLocation(override val regionIdentifier: String, proc: Procedure, size: BigInt) extends MemoryLocation {
  override def toString: String = s"Heap($regionIdentifier, $size)"
}

case class DataLocation(override val regionIdentifier: String, override val start: BigInt, override val size: BigInt)
extends MemoryLocation, GlobalLocation {
  override def toString: String = s"Data($regionIdentifier, $start, $size)"
}

case class ExternalLocation(override val regionIdentifier: String) extends MemoryLocation {
  override def toString: String = s"External($regionIdentifier)"
}

case class Function(regionIdentifier: String, start: BigInt, size: BigInt) extends MemoryLocation, GlobalLocation{
  override def toString: String = s"Func($regionIdentifier, $start, $size)"
}


case class UnknownLocation(override val regionIdentifier: String, proc: Procedure) extends MemoryLocation {
  override def toString: String = s"Unknown($regionIdentifier)"
}

/**
 * environment transformers for SAA or symbolic access analysis
 * Combination of reaching definitions and constant propagation
 * elements in D are symbolic accesses of the form (variable, symbolic base, concrete offset)
 * lattice L is a binary lattice with top being the definition is valid (alive) and bottom being
 * the definition is dead or no longer affects the environment
 */
trait SymbolicAddressFunctions(constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) extends ForwardIDEAnalysis[SymbolicAddress, TwoElement, TwoElementLattice] {

  private val stackPointer = Register("R31", 64)
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
  import edgelattice.{ConstEdge, IdEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case _: StackLocation => Map()
          case _ => Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case _: StackLocation => Map()
          case _ =>
            if value.accessor.name == "R29" then Map()
            else Map(d -> IdEdge())
      case Right(_) => Map(d -> IdEdge())

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) =>
        value.symbolicBase match
          case _: StackLocation => Map(d -> IdEdge())
          case _ => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    n match
      case LocalAssign(variable, rhs, _) =>
        val expr = unwrapPaddingAndSlicing(rhs)
        expr match
          case BinaryExpr(op, arg1: Variable, arg2) if op.equals(BVADD) =>
            evaluateExpression(arg2, constProp(n)) match
              case Some(v) =>
                if op.equals(BVADD) && arg1.equals(stackPointer) && isNegative(v) then
                  d match
                    case Left(value) if value.accessor == variable => Map()
                    case Left(_) => Map(d -> IdEdge())
                    case Right(_) =>
                      val size = bv2SignedInt(v)
                      val procedure = IRWalk.procedure(n)
                      Map(d -> IdEdge(), Left(SymbolicAddress(variable, StackLocation(s"Stack_${procedure.name}", procedure, -size), 0)) -> ConstEdge(TwoElementTop))
                else
                  d match
                    case Left(value) if value.accessor == arg1 =>
                      val offsetUpdate = evaluateExpression(arg2, constProp(n)).get.value
                      val result: Map[DL, EdgeFunction[TwoElement]] = Map(Left(SymbolicAddress(variable, value.symbolicBase, value.offset + offsetUpdate)) -> ConstEdge(TwoElementTop))
                      if value.accessor != variable then
                        result + (d -> IdEdge())
                      else
                        result
                    case Left(value) if value.accessor == variable => Map()
                    case _ => Map(d -> IdEdge())
              case None => Map(d -> IdEdge())
          case arg: Variable =>
            d match
              case Left(value) if value.accessor == arg =>
                val result: Map[DL, EdgeFunction[TwoElement]] = Map(Left(SymbolicAddress(variable, value.symbolicBase, value.offset)) -> ConstEdge(TwoElementTop))
                if value.accessor != variable then
                  result + (d -> IdEdge())
                else
                  result
              case Left(value) if value.accessor == variable => Map()
              case _ => Map(d -> IdEdge())
          case _ =>
            d match
              case Left(value) if value.accessor == variable => Map()
              case _ => Map(d -> IdEdge())
      case MemoryLoad(lhs, _, _, _, _, _) =>
        d match
          case Left(value) if value.accessor == lhs => Map()
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge(), Left(SymbolicAddress(lhs, UnknownLocation(nextunknownCount, IRWalk.procedure(n)), 0)) -> ConstEdge(TwoElementTop))
      case DirectCall(target, _, _, _) if target.procName == "malloc" =>
        d match
          case Left(value) if value.accessor == mallocVariable => Map()
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            val size: BigInt = evaluateExpression(mallocVariable, constProp(n)) match
              case Some(value) => value.value
              case None => -1
            Map(d -> IdEdge(), Left(SymbolicAddress(mallocVariable, HeapLocation(nextMallocCount, IRWalk.procedure(n), size), 0)) -> ConstEdge(TwoElementTop))
      case DirectCall(target, _, _, _) if target.returnBlock.isEmpty => // for when calls are non returning, kills the stack dataflow facts
        d match
          case Left(value) =>
            value.symbolicBase match
              case _: StackLocation => Map()
              case _ => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge())
      case _ => Map(d -> IdEdge())
}

class SymbolicAddressAnalysis(program: Program, constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]])
  extends ForwardIDESolver[SymbolicAddress, TwoElement, TwoElementLattice](program), SymbolicAddressFunctions(constProp)
