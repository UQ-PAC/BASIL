package analysis

import analysis.solvers.IDESolver
import ir.{LocalAssign, Variable}

trait ReachingDefsAnalysisFunctions extends IDEAnalysis[LocalAssign, FlatElement[Nothing] ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)

  import edgelattice.{IdEdge, ConstEdge}
  def edgesCallToEntry(call: CfgJumpNode, entry: CfgFunctionEntryNode)(d: DL): Map[DL, edgelattice.Element] =
    Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: CfgFunctionExitNode,
                           aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesCallToAfterCall(call: CfgJumpNode,
                           aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.Element] = {
    n match
      case sn: CfgStatementNode =>
        sn.data match
          case LocalAssign(variable, _, _) =>
            d match
              case Left(value) =>
                value match
                  case LocalAssign(variableNew, _, _) =>
                    if variableNew == variable then
                      Map()
                    else
                      Map(d -> IdEdge())
                  case _ => ???
              case Right(_) =>
                Map(d -> IdEdge(), Left(sn.data.asInstanceOf[LocalAssign]) -> IdEdge())
          case _ => Map(d -> IdEdge())


      case _ => Map(d -> IdEdge())
  }
}

class ReachingDefsIDEAnalysis(cfg: ProgramCfg, cache: CfgIDECache) extends
  IDESolver[LocalAssign, FlatElement[Nothing] ,TwoElementLattice](cfg, cache), ReachingDefsAnalysisFunctions

/**
 * Wrapper class for ReachingDefsIDEAnalysis
 * @param cfg Program Cfg
 */
class ReachingDefsAnalysis(cfg: ProgramCfg) {
  /**
   * Uninitialized Variables Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CfgNode, Map[LocalAssign, FlatElement[Nothing]]] = {
    val cache = CfgIDECache()
    cache.cacheCfg(cfg)
    //    cache.reverseCfg(cfg)
    //    cache.reverseCfg(cfg)
    val analysis = ReachingDefsIDEAnalysis(cfg, cache)
    analysis.analyze()
  }
}