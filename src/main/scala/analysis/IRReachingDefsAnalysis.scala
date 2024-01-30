package analysis

import analysis.solvers.ForwardIDESolver
import ir.{Block, CFGPosition, Command, DirectCall, LocalAssign, Procedure, Program, Variable}

trait IRReachingDefsAnalysisFunctions extends ForwardIDEAnalysis[LocalAssign, FlatElement[Nothing] ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)

  import edgelattice.{IdEdge, ConstEdge}
  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, edgelattice.Element] =
    Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: Command, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesCallToAfterCall(call: DirectCall, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, edgelattice.Element] = {
    n match
      case LocalAssign(variable, _, _) =>
        d match
          case Left(value) =>
            value match
              case LocalAssign(variableNew, _, _) =>
                if variableNew == variable then // at the variable being redefined
                  Map()
                else
                  Map(d -> IdEdge())
              case _ => ???
          case Right(_) =>
              Map(d -> IdEdge(), Left(n.asInstanceOf[LocalAssign]) -> ConstEdge(Top))
      case _ => Map(d -> IdEdge())
  }
}

class ReachingDefsIRIDEAnalysis(program: Program, cache: IRIDECache) extends
  ForwardIDESolver[LocalAssign, FlatElement[Nothing] ,TwoElementLattice](program, cache), IRReachingDefsAnalysisFunctions

/**
 * Wrapper class for ReachingDefsIDEAnalysis
 * @param cfg Program Cfg
 */
class ReachingDefsIRAnalysis(program: Program) {
  /**
   * Reaching Definitions Analysis
   * @return Analysis Result
   */
  def analyze(): Map[CFGPosition, Map[LocalAssign, FlatElement[Nothing]]] = {
    val cache = IRIDECache(program)
    cache.cache()
    val analysis = ReachingDefsIRIDEAnalysis(program, cache)
    analysis.analyze()
  }
}