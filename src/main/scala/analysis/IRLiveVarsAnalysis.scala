package analysis

import analysis.solvers
import analysis.solvers.BackwardIDESolver
import cfg_visualiser.Output
import ir.{Assert, Assume, Block, CFGPosition, Command, DirectCall, IndirectCall, LocalAssign, MemoryAssign, Procedure, Program, Variable}
import util.RunUtils.writeToFile

/**
 * Micro-transfer-functions for LiveVar analysis
 * Tip SPA IDE Slides include a short and clear explanation of microfunctions
 * https://cs.au.dk/~amoeller/spa/8-distributive.pdf
 */
trait IRLiveVarAnalysisFunctions extends BackwardIDEAnalysis[Variable, FlatElement[Nothing] ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = new EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: Block, entry: Command)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: Block, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
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
            (mp, expVar) => mp + (Left(expVar) -> ConstEdge(Top))
          }

      case MemoryAssign(memory, store, maybeString) => // s ++ store.index.variables ++ store.value.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            (store.index.variables ++ store.value.variables).foldLeft(Map(d -> IdEdge()) : Map[DL, edgelattice.Element]) {
              (mp, storVar) => mp + (Left(storVar) -> ConstEdge(Top))
            }

      case Assume(expr, maybeString, maybeString1, bool) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(Top))
            }

      case Assert(expr, maybeString, maybeString1) => // s ++ expr.variables
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(Top))
            }
      case IndirectCall(variable, maybeBlock, maybeString) =>
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(Top))
      case _ => Map(d -> IdEdge())


  }
}

class IRLiveVarIDEAnalysis(program: Program, cache: IRIDECache)
  extends BackwardIDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](program, cache), IRLiveVarAnalysisFunctions

/**
 * Wrapper class for LiveVarIDEAnalysis
 * @param program Program IR
 */
class IRLiveVarAnalysis(program: Program) {
  /**
   * Live Variable Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CFGPosition, Map[Variable, FlatElement[Nothing]]] = {
    val cache = IRIDECache(program)
    cache.cache()

    IRLiveVarIDEAnalysis(program, cache).analyze()
  }
}