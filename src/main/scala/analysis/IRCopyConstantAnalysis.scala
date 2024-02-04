package analysis

import analysis.solvers.ForwardIDESolver
import ir.{BitVecLiteral, BitVecType, Block, CFGPosition, Command, DirectCall, Literal, LocalAssign,  Procedure, Program, Register, Variable}

import scala.collection.mutable

trait IRCopyConstantAnalysisFunctions extends ForwardIDEAnalysis[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice] {

  val valuelattice: ConstantPropagationLattice = ConstantPropagationLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[BitVecLiteral], valuelattice.type] =
    EdgeFunctionLattice[FlatElement[BitVecLiteral],valuelattice.type](valuelattice)

  import edgelattice.{IdEdge, ConstEdge}
  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, edgelattice.Element] =
    Map(d -> IdEdge())

  def edgesExitToAfterCall(exit: Command, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = Map(d -> IdEdge())

  def edgesCallToAfterCall(call: DirectCall, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = Map()

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, edgelattice.Element] = {
    if n.isInstanceOf[DirectCall] then
      print("")
    n match {
      case p: Procedure if p == program.mainProcedure =>
        d match
          case Left(value) => Map()
          case Right(_) =>
            (for i <- (0 to 28) yield Register(s"R${i}", BitVecType(64))).foldLeft(Map(d -> IdEdge()): Map[DL, edgelattice.Element]) {
              (mp, r) => mp + (Left(r) -> ConstEdge(Top))
            }

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
  }
}

class IRCopyConstantIDEAnalysis(program: Program, cache: IRIDECache)
  extends ForwardIDESolver[Variable, FlatElement[BitVecLiteral], ConstantPropagationLattice](program, cache),
    IRCopyConstantAnalysisFunctions

/**
 * Wrapper class for CopyConstantIDEAnalysis
 * @param program program IR
 */
class IRCopyConstantAnalysis(program: Program) {
  /**
   * Copy Constant Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = {
    val cache = IRIDECache(program)
    cache.cache()
    val analysis = IRCopyConstantIDEAnalysis(program, cache)
    analysis.analyze()
  }
}
