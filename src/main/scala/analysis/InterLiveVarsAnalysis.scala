package analysis

import analysis.solvers.BackwardIDESolver
import ir.{Assert, Assume, GoTo, CFGPosition, Command, DirectCall, IndirectCall, Assign, MemoryAssign, Procedure, Program, Variable, toShortString}

/**
 * Micro-transfer-functions for LiveVar analysis
 * this analysis works by inlining function calls (instead of just mapping parameters and returns all
 * live variables (registers) are propagated to and from callee functions)
 * The result of what variables are alive at each point in the program should still be correct
 * However, the functions that are callees of other functions will have an over approximation of their parameters
 * alive at the top of the function
 * Tip SPA IDE Slides include a short and clear explanation of microfunctions
 * https://cs.au.dk/~amoeller/spa/8-distributive.pdf
 */
trait LiveVarsAnalysisFunctions extends BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[TwoElement, TwoElementLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: GoTo, entry: IndirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: GoTo, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match
      case Left(value) => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match
      case Assign(variable, expr, _) => // (s - variable) ++ expr.variables
        d match
          case Left(value) =>
            if value == variable then
              Map()
            else
              Map(d -> IdEdge())

          case Right(_) => expr.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
            (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
          }

      case MemoryAssign(_, index, value, _, _, _) => // s ++ store.index.variables ++ store.value.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            (index.variables ++ value.variables).foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, storVar) => mp + (Left(storVar) -> ConstEdge(TwoElementTop))
            }

      case Assume(expr, _, _, _) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, EdgeFunction[TwoElement]]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }

      case Assert(expr, _, _) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
      case IndirectCall(variable, _, _) =>
        d match
          case Left(value) => if value != variable then Map(d -> IdEdge()) else Map()
          case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(TwoElementTop))
      case _ => Map(d -> IdEdge())

  }
}

class InterLiveVarsAnalysis(program: Program)
  extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program), LiveVarsAnalysisFunctions





