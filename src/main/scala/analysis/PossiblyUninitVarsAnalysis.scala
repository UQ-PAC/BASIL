package analysis


import analysis.solvers.ForwardIDESolver
import ir.{Assert, Assume, BitVecType, Block, CFGPosition, Command, DirectCall, IndirectCall, LocalAssign, MemoryAssign, Procedure, Program, Register, Variable}

import scala.collection.mutable

/**
 * Micro-transfer-functions for Uninitialized Variables analysis
 * Tip SPA IDE Slides include a short and clear explanation of microfunctions
 * https://cs.au.dk/~amoeller/spa/8-distributive.pdf
 */
trait PossiblyUninitVarsFunctions extends ForwardIDEAnalysis[Variable, FlatElement[Nothing], TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = new EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)

  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Command, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Block)(d: DL): Map[DL, edgelattice.Element] = {
    Map(d -> IdEdge())
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, edgelattice.Element] = {
    n match
      case entry: Procedure if entry == program.mainProcedure =>
        d match
          case Left(value) => Map()
          case Right(_) =>
            (for i <- (0 to 28) yield Register(s"R${i}", BitVecType(64))).foldLeft(Map(d -> IdEdge()) : Map[DL, edgelattice.Element]) {
              (mp, r) => mp + (Left(r) -> ConstEdge(Top))
            }
      case LocalAssign(variable, expr, _) =>
        d match
          case Left(value) =>
            val edges = mutable.ListBuffer[(DL, edgelattice.Element)]()
            if value != variable then
              edges += (d -> IdEdge())
            if expr.variables.contains(value) then
              edges += (Left(variable) -> IdEdge()) // values in the expression determine the value of the variable
            edges.toMap
          case Right(_) => Map(d -> IdEdge())
      case _ => Map(d -> IdEdge())
    
  }
}

class PossiblyUninitVarsIDEAnalysis(program: Program, cache: IRIDECache) extends
  ForwardIDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](program, cache), PossiblyUninitVarsFunctions

/**
 * Wrapper class for UninitVariablesIDEAnalysis
 * @param program Program IR
 */
class PossiblyUninitVarsAnalysis(program: Program) {
  /**
   * Uninitialized Variables Analysis
   * Cfg is manipulated for the Analysis (Added edges from fun exit to call return nodes)
   * @return Analysis Result
   */
  def analyze(): Map[CFGPosition, Map[Variable, FlatElement[Nothing]]] = {
    val cache = IRIDECache(program)
    cache.cache()
    val analysis = PossiblyUninitVarsIDEAnalysis(program, cache)
    analysis.analyze()
  }
}