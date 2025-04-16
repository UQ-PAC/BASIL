package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*

trait TaintAnalysisFunctions(tainted: Map[CFGPosition, Set[Variable]])
    extends ForwardIDEAnalysis[Variable, TwoElement, TwoElementLattice] {
  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    (n match {
      case LocalAssign(variable, expression, _) =>
        d match {
          case Left(v) if expression.variables.contains(v) => Map(d -> IdEdge(), Left(variable) -> IdEdge())
          case Left(v) if v == variable => Map()
          case _ => Map(d -> IdEdge())
        }
      // TODO taint regions
      case MemoryLoad(lhs, mem, index, _, size, _) =>
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge(), Left(lhs) -> ConstEdge(valuelattice.top))
        }
      case Return(_, out) =>
        out.toMap.flatMap { (variable, expression) =>
          {
            d match {
              case Left(v) if expression.variables.contains(v) => Map(d -> IdEdge(), Left(variable) -> IdEdge())
              case Left(v) if v == variable => Map()
              case _ => Map(d -> IdEdge())
            }
          }
        }
      case _ => Map(d -> IdEdge())
    }) ++ (
      d match
        case Left(_) => Map()
        case Right(_) =>
          tainted.getOrElse(n, Set()).foldLeft(Map[DL, EdgeFunction[TwoElement]]()) { (m, t) =>
            m + (Left(t) -> ConstEdge(valuelattice.top))
          }
    )
  }
}

/**
 * Performs taint analysis on a program. Variables are marked as tainted at points in the program as
 * specified by `tainted`, and propogate their taint throughout the program. Assignments containing tainted variables
 * mark the assigned value as tainted.
 */
class TaintAnalysis(program: Program, tainted: Map[CFGPosition, Set[Variable]])
    extends ForwardIDESolver[Variable, TwoElement, TwoElementLattice](program),
      TaintAnalysisFunctions(tainted)
