package analysis

import analysis.solvers
import analysis.solvers.BackwardIDESolver
import cfg_visualiser.Output
import ir.{Assert, Assume, GoTo, CFGPosition, Command, DirectCall, IndirectCall, LocalAssign, MemoryAssign, Procedure, Program, Variable}
import util.RunUtils.writeToFile

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
trait LiveVarAnalysisFunctions extends BackwardIDEAnalysis[Variable, FlatElement[Nothing] ,TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type] = new EdgeFunctionLattice[FlatElement[Nothing], valuelattice.type](valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: GoTo, entry: Command)(d: DL): Map[DL, edgelattice.Element] = {
    // this analysis is implemented in order to Identify parameters and function interface
    // if parameters are known this should map only parameter registers to IdEdge() or in this case
    // return (since it is a backward analysis)
    // all other dataflow facts except lambda should be mapped to bottom (return empty Map())
    //    d match
    //      case Left(value) => Map()
    //      case Right(_) => Map(d -> IdEdge())


    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    // this analysis is implemented in order to Identify parameters and function interface
    // if parameters are known this should map return registers to IdEdge() or in this case
    // parameters (since it is a backward analysis)
    // all other dataflow facts except lambda should be mapped to bottom (return empty Map())
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: GoTo, aftercall: DirectCall)(d: DL): Map[DL, edgelattice.Element] = {
    // this analysis is implemented in order to Identify parameters and function interface
    // if parameters are known this should map all non parameter registers to IdEdge() instead and all parameter to bottom
    //    d match
    //      case Left(value) =>
    //        val p = aftercall.target.out.foldLeft(false) {
    //          (b, param) =>
    //            if value == param.value then
    //              true
    //            else
    //              b
    //        }
    //
    //        if p then Map() else Map(d -> IdEdge())
    //      case Right(_) => Map(d -> IdEdge())

    d match
      case Left(value) => Map()
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
          case Left(value) => if value != variable then Map(d -> IdEdge()) else Map()
          case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(Top))
      case _ => Map(d -> IdEdge())


  }
}

class LiveVarAnalysis(program: Program)
  extends BackwardIDESolver[Variable, FlatElement[Nothing] ,TwoElementLattice](program), LiveVarAnalysisFunctions


object LiveVarAnalysis extends AnalysisResult[Map[CFGPosition, Map[Variable, FlatElement[Nothing]]]] {

  def encodeAnalysisResults(result: Map[CFGPosition, Map[Variable, FlatElement[Nothing]]]): String = {
    val pp = result.foldLeft("") {
      (m, f) =>
        val cfgPosition: CFGPosition = f._1
        val mapping: Map[Variable, FlatElement[Nothing]] = f._2
        val positionMaps = mapping.foldLeft("") {
          (line, pair) =>
            line + s"${pair._1}->${pair._2}<>"
        }

        m + s"$cfgPosition==>${positionMaps.dropRight(2)}\n"
    }
    pp.dropRight(1)
  }

  def parseAnalysisResults(input: String): String = {
    input.split("\n").sorted.foldLeft(Map(): Map[String, Set[String]]) {
      (m, line) =>
        val cfgPosition: String = line.split("==>", 2)(0)
        val rest: String = line.split("==>", 2)(1)
        m + (cfgPosition -> rest.split("<>").sorted.toSet)
    }.toString
  }
}



