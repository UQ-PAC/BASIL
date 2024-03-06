package analysis

import analysis.solvers.{BackwardIDESolver, ForwardIDESolver}
import ir.{BinaryExpr, BitVecType, CFGPosition, DirectCall, Extract, GoTo, IRWalk, IndirectCall, Literal, LocalAssign, Memory, MemoryAssign, MemoryLoad, MemoryStore, Procedure, Program, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend, end}

import scala.collection.immutable.Map
import scala.collection.mutable

trait PointerTypeFunctions extends BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[TwoElement, TwoElementLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match
      case s: CFGPosition if end(program.mainProcedure).equals(s) =>
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge(), Left(Register("R31", BitVecType(64))) -> ConstEdge(TwoElementTop))

      case LocalAssign(lhs, expr, _) =>
        expr match
          case BinaryExpr(op, arg1, arg2) =>
            d match
              case Left(value) if value == arg1 => Map(Left(lhs) -> IdEdge())
              case Left(value) if value == lhs => Map()
              case _ => Map(d -> IdEdge())
          case rhs: Variable =>
            d match
              case Left(value) if value == rhs => Map(Left(lhs) -> IdEdge())
              case Left(value) if value == lhs => Map()
              case _ => Map(d -> IdEdge())
          case MemoryLoad(mem, index, endian, size) =>
            index match
              case BinaryExpr(op, arg1: Variable, arg2) =>
                d match
                  case Left(value) if value == lhs || value == arg1 => Map()
                  case Left(value) => Map(d -> IdEdge())
                  case Right(_) => Map(d -> IdEdge(), Left(arg1) -> ConstEdge(TwoElementTop))
              case variable: Variable =>
                d match
                  case Left(value) if value == lhs || value == variable => Map()
                  case Left(value) => Map(d -> IdEdge())
                  case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(TwoElementTop))
              case _ =>
                d match
                  case Left(value) if value == lhs  => Map()
                  case Left(value) => Map(d -> IdEdge())
                  case Right(_) => Map(d -> IdEdge())
          case _ =>
            d match
              case Left(value) if value == lhs => Map()
              case Left(value) => Map(d -> IdEdge())
              case Right(_) => Map(d -> IdEdge())
      case MemoryAssign(mem, MemoryStore(mem2, index, value, endian, size), _) =>
        index match
          case BinaryExpr(op, arg1: Variable, arg2) =>
            d match
              case Left(value) if value != arg1 => Map(d -> IdEdge())
              case Left(value) if value == arg1 => Map()
              case Right (_) => Map(d -> IdEdge(), Left(arg1) -> ConstEdge(TwoElementTop))
          case variable: Variable =>
            d match
              case Left(value) if value != variable => Map(d -> IdEdge())
              case Left(value) if value == variable => Map()
              case Right (_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(TwoElementTop))
          case _ => Map(d -> IdEdge())
      case _ => Map(d -> IdEdge())
  }

  def edgesCallToEntry(call: GoTo, entry: IndirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = Map(d -> IdEdge())

  def edgesCallToAfterCall(call: GoTo, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] =
    d match
      case Left(value) => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())
}

class PointerTypeAnalysis(program: Program)
  extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program), PointerTypeFunctions
