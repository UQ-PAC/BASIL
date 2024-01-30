package analysis

import analysis.solvers.IDESolver
import cfg_visualiser.Output
import ir.{Assert, Assume, IndirectCall, LocalAssign, MemoryAssign, Variable}
import util.RunUtils.writeToFile

/**
 * Micro-transfer-functions for LiveVar analysis
 * Tip SPA IDE Slides include a short and clear explanation of microfunctions
 * https://cs.au.dk/~amoeller/spa/8-distributive.pdf
 */
trait LiveVarAnalysisFunctions extends IDEAnalysis[Variable, FlatElement[Nothing] ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = new EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: CfgJumpNode, entry: CfgFunctionEntryNode)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: CfgFunctionExitNode, aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: CfgJumpNode, aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.Element] = {
    n match {
      case sn: CfgStatementNode =>
        sn.data match {
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
          case _ => ???
        }

      case node: CfgJumpNode =>
        node.data match
          case IndirectCall(variable, maybeBlock, maybeString) =>
            d match
              case Left(value) => Map(d -> IdEdge())
              case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(Top))
          case _ => Map(d -> IdEdge())

      case _ => Map(d -> IdEdge())
    }
  }
}

class LiveVarIDEAnalysis(cfg: ProgramCfg, cache: CfgIDECache)
  extends IDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](cfg, cache), LiveVarAnalysisFunctions

/**
 * Wrapper class for LiveVarIDEAnalysis
 * @param cfg Program Cfg
 */
class LiveVarAnalysis(cfg: ProgramCfg) {
  /**
   * Live Variable Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CfgNode, Map[Variable, FlatElement[Nothing]]] = {
    val cache = CfgIDECache()
    cache.cacheCfg(cfg)
    cache.reverseCfg(cfg)
    var livenessAnalysisResult = LiveVarIDEAnalysis(cfg, cache).analyze()
    writeToFile(cfg.toDot(Output.labeler(livenessAnalysisResult, true), Output.dotIder), s"midpoint_liveness.dot")
    cache.entryExitMap.forwardMap.foreach(
      (entry, exit) =>
        val exitResult = livenessAnalysisResult.getOrElse(entry, Map())
        val entryResult = livenessAnalysisResult.getOrElse(exit, Map())
        livenessAnalysisResult = livenessAnalysisResult + (entry -> entryResult, exit -> exitResult)
    )
    cache.callReturnMap.forwardMap.foreach(
      (call, ret) =>
        val retResult = livenessAnalysisResult.getOrElse(call, Map())
        val callResult = livenessAnalysisResult.getOrElse(ret, Map())
        livenessAnalysisResult = livenessAnalysisResult + (call -> callResult, ret -> retResult)
    )
    cache.reverseCfg(cfg)
    livenessAnalysisResult
  }
}