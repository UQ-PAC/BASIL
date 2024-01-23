package analysis

import analysis.solvers.IDESolver
import ir.{Assert, Assume, BitVecType, IndirectCall, LocalAssign, MemoryAssign, Register, Variable}

import scala.collection.mutable

/**
 * Micro-transfer-functions for Uninitialized Variables analysis
 * Tip SPA IDE Slides include a short and clear explanation of microfunctions
 * https://cs.au.dk/~amoeller/spa/8-distributive.pdf
 */
trait UninitVariablesAnalysisFunctions extends IDEAnalysis[Variable, FlatElement[Nothing], TwoElementLattice] {

  val firstSeen: mutable.Map[Variable, CfgNode] = mutable.Map(Register("R31", BitVecType(64)) -> cfg.startNode,
    Register("R30", BitVecType(64)) -> cfg.startNode, Register("R29", BitVecType(64)) -> cfg.startNode)
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
      case entry: CfgFunctionEntryNode if entry == cfg.startNode =>
        d match
          case Left(value) => Map()
          case Right(_) =>
            (for i <- (0 to 28) yield Register(s"R${i}", BitVecType(64))).foldLeft(Map(d -> IdEdge()) : Map[DL, edgelattice.Element]) {
              (mp, r) => mp + (Left(r) -> ConstEdge(Top))
            }

      case sn: CfgStatementNode =>
        sn.data match {
          case LocalAssign(variable, expr, maybeString) =>
            d match
              case Left(value) =>
                val edges = mutable.ListBuffer[(DL, edgelattice.Element)]()
                if value != variable then
                  edges += (d -> IdEdge())
                if expr.variables.contains(value) then
                  edges += (Left(variable) -> IdEdge()) // TODO
                edges.toMap
              case Right(_) => Map(d -> IdEdge())

          case _ => Map(d -> IdEdge())
        }

      case _ => Map(d -> IdEdge())
    }
  }
}

class UninitVariablesIDEAnalysis(cfg: ProgramCfg, cache: CfgIDECache) extends
  IDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](cfg, cache), UninitVariablesAnalysisFunctions

/**
 * Wrapper class for UninitVariablesIDEAnalysis
 * @param cfg Program Cfg
 */
class UninitVariablesAnalysis(cfg: ProgramCfg) {
  /**
   * Uninitialized Variables Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CfgNode, Map[Variable, FlatElement[Nothing]]] = {
    val cache = CfgIDECache()
    cache.cacheCfg(cfg)
    val analysis = UninitVariablesIDEAnalysis(cfg, cache)
    analysis.analyze()
  }
}