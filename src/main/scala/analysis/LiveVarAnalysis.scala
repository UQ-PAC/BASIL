package analysis

import analysis.solvers.IDESolver
import ir.{Assert, Assume, IndirectCall, LocalAssign, MemoryAssign, Register, Variable}

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

      case IndirectCall(variable, maybeBlock, maybeString) =>
        d match
          case Left(value) => Map(d -> IdEdge())
          case Right(_) => Map(Left(variable) -> ConstEdge(Top))


      case _ => Map(d -> IdEdge())
    }
  }
}

class LiveVarIDEAnalysis(cfg: ProgramCfg, cache: CfgIDECache)
  extends IDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](cfg, cache), LiveVarAnalysisFunctions
