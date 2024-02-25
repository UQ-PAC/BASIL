package analysis

import analysis.solvers.BackwardIDESolver
import ir.{Assert, Assume, GoTo, CFGPosition, Command, DirectCall, IndirectCall, LocalAssign, MemoryAssign, Procedure, Program, Variable, toShortString}

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
trait LiveVarsAnalysisFunctions extends BackwardIDEAnalysis[Variable, TwoElementLatticeEl ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[TwoElementLatticeEl, valuelattice.type ] = new EdgeFunctionLattice[TwoElementLatticeEl, valuelattice.type](valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: GoTo, entry: Command)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: GoTo, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    d match
      case Left(value) => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, edgelattice.Element] = {
    n match
      case LocalAssign(variable, expr, maybeString) => // (s - variable) ++ expr.variables
        d match
          case Left(value) =>
            if value == variable then
              Map()
            else
              Map(d -> IdEdge())

          case Right(_) => expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
            (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
          }

      case MemoryAssign(memory, store, maybeString) => // s ++ store.index.variables ++ store.value.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            (store.index.variables ++ store.value.variables).foldLeft(Map(d -> IdEdge()) : Map[DL, edgelattice.Element]) {
              (mp, storVar) => mp + (Left(storVar) -> ConstEdge(TwoElementTop))
            }

      case Assume(expr, maybeString, maybeString1, bool) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }

      case Assert(expr, maybeString, maybeString1) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
      case IndirectCall(variable, maybeBlock, maybeString) =>
        d match
          case Left(value) => if value != variable then Map(d -> IdEdge()) else Map()
          case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(TwoElementTop))
      case _ => Map(d -> IdEdge())


  }
}

class InterLiveVarsAnalysis(program: Program)
  extends BackwardIDESolver[Variable, TwoElementLatticeEl ,TwoElementLattice](program), LiveVarsAnalysisFunctions





