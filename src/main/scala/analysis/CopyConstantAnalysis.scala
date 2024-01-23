package analysis

import analysis.solvers.IDESolver
import ir.{BinaryExpr, BitVecLiteral, BitVecType, Extract, Literal, LocalAssign, Memory, MemoryLoad, MemoryStore, Register, Repeat, SignExtend, UnaryExpr, Variable, ZeroExtend}

import scala.collection.mutable

trait CopyConstantAnalysisFunctions extends IDEAnalysis[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] {

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[BitVecLiteral], valuelattice.type] =
    EdgeFunctionLattice[FlatElement[BitVecLiteral],valuelattice.type](valuelattice)

  import edgelattice.{IdEdge, ConstEdge}
  def edgesCallToEntry(call: CfgJumpNode, entry: CfgFunctionEntryNode)(d: DL): Map[DL, edgelattice.Element] =
    Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: CfgFunctionExitNode,
                           aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesCallToAfterCall(call: CfgJumpNode,
                           aftercall: CfgCallReturnNode)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesOther(n: CfgNode)(d: DL): Map[DL, edgelattice.Element] = {
    n match {
      case entry: CfgFunctionEntryNode if entry == cfg.startNode =>
        d match
          case Left(value) => Map()
          case Right(_) =>
            (for i <- (0 to 28) yield Register(s"R${i}", BitVecType(64))).foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, r) => mp + (Left(r) -> ConstEdge(Top))
            }

      case sn: CfgStatementNode =>
        sn.data match {
          case LocalAssign(variable, expr, maybeString) =>
            val edges = mutable.ListBuffer[(DL, edgelattice.Element)]()
            d match
              case Left(value) =>
                if (variable != value) { // not at the variable being written to, so add identity edge
                  edges += (d -> IdEdge())
                }
                expr match
                  case vari: Variable =>
                    if vari == value then // identity edge from variable being read to the variable being written to
                      edges += (Left(variable) -> IdEdge())
                  case _ => // ignore other kinds of expressions
              case Right(_) =>
                edges += (d -> IdEdge())
                expr match
                  case variable: Variable => // if the expression is a variable, no additional edges from lambda
                  // if the expression is a constant, add constant edge from lambda to the variable being assigned to
                  case literal: Literal => edges += (Left(variable) -> ConstEdge(FlatEl(literal.asInstanceOf[BitVecLiteral])))
                  // for other expressions, add top edge from lambda to the variable being assigned to
                  case _ => edges += (Left(variable) -> ConstEdge(Top))
            edges.toMap
          case _ => Map(d -> IdEdge())
        }
      case _ => Map(d -> IdEdge())
    }
  }
}

class CopyConstantIDEAnalysis(cfg: ProgramCfg, cache: CfgIDECache)
  extends IDESolver[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice](cfg, cache),
    CopyConstantAnalysisFunctions

/**
 * Wrapper class for CopyConstantIDEAnalysis
 * @param cfg Program Cfg
 */
class CopyConstantAnalysis(cfg: ProgramCfg) {
  /**
   * Copy Constant Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CfgNode, Map[Variable, FlatElement[BitVecLiteral]]] = {
    val cache = CfgIDECache()
    cache.cacheCfg(cfg)
    val analysis = CopyConstantIDEAnalysis(cfg, cache)
    analysis.analyze()
  }
}