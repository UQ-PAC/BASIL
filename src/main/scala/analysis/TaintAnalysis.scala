package analysis

import analysis.solvers.ForwardIDESolver
import ir.*

trait TaintAnalysisFunctions(
  tainted: Map[CFGPosition, Set[Variable]],
) extends ForwardIDEAnalysis[Variable, TwoElement, TwoElementLattice] {
  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: IndirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    (n match {
      case Assign(variable, expression, _) => {
        d match {
          case Left(v) => {
            val contains = expression.variables.contains(v)
            if contains then Map(d -> IdEdge(), Left(variable) -> IdEdge())
            else if v == variable then Map()
            else Map(d -> IdEdge())
          }
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case _ => Map(d -> IdEdge())
    }) ++ {
      d match
        case Left(_) => Map()
        case Right(_) => tainted.getOrElse(n, Set()).foldLeft(Map[DL, EdgeFunction[TwoElement]]()) {
          (m, t) => m + (Left(t) -> ConstEdge(valuelattice.top))
        }
    }
  }
}

class TaintAnalysis(
  program: Program,
  tainted: Map[CFGPosition, Set[Variable]],
) extends ForwardIDESolver[Variable, TwoElement, TwoElementLattice](program),
    TaintAnalysisFunctions(tainted)
