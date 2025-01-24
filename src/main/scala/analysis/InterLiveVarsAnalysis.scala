package analysis

import analysis.solvers.BackwardIDESolver
import ir.{Assert, LocalAssign, Assume, CFGPosition, Command, DirectCall, IndirectCall, MemoryLoad, MemoryStore, Procedure, Program, Return, Variable}

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
trait LiveVarsAnalysisFunctions extends BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice] {

  val valuelattice: TwoElementLattice = TwoElementLattice()
  val edgelattice: EdgeFunctionLattice[TwoElement, TwoElementLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(l) => Map() // maps all variables before the call to bottom
      case Right(_) => entry.outParams.flatMap(_._2.variables).foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
      }
    }
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(_) => Map() // maps all variables before the call to bottom
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match {
      case LocalAssign(variable, expr, _) => // (s - variable) ++ expr.variables
        d match {
          case Left(value) =>
            if value == variable then
              Map()
            else
              Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      case MemoryLoad(lhs, _, index, _, _, _) =>
        d match {
          case Left(value) =>
            if value == lhs then
              Map()
            else
              Map(d -> IdEdge())
          case Right(_) => index.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
            (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
          }
        }
      case MemoryStore(_, index, value, _, _, _) => // s ++ store.index.variables ++ store.value.variables
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            (index.variables ++ value.variables).foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, storVar) => mp + (Left(storVar) -> ConstEdge(TwoElementTop))
            }
        }
      case Assume(expr, _, _, _) => // s ++ expr.variables
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map(d -> IdEdge()): Map[DL, EdgeFunction[TwoElement]]) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      case Assert(expr, _, _) => // s ++ expr.variables
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) =>
            expr.variables.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      case IndirectCall(variable, _) =>
        d match {
          case Left(value) => if value != variable then Map(d -> IdEdge()) else Map()
          case Right(_) => Map(d -> IdEdge(), Left(variable) -> ConstEdge(TwoElementTop))
        }
      case c: DirectCall if (c.target.isExternal.contains(true) || c.target.blocks.isEmpty) => {
        val writes = ir.transforms.externalCallWrites(c.target.procName).toSet[Variable]
        val reads = ir.transforms.externalCallReads(c.target.procName).toSet[Variable]
        d match {
          case Left(value) =>
            if reads.contains(value) then
              Map()
            else
              Map(d -> IdEdge())
          case Right(_) =>
            reads.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      } 
      case c: DirectCall  => {
        val writes = c.outParams.map(_._2).toSet
        val reads = c.actualParams.flatMap(_._2.variables).toSet
        d match {
          case Left(value) =>
            if reads.contains(value) then
              Map()
            else
              Map(d -> IdEdge())
          case Right(_) =>
            reads.foldLeft(Map[DL, EdgeFunction[TwoElement]](d -> IdEdge())) {
              (mp, expVar) => mp + (Left(expVar) -> ConstEdge(TwoElementTop))
            }
        }
      }
      case _ => Map(d -> IdEdge())
    }
  }
}

class InterLiveVarsAnalysis(program: Program)
  extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program), LiveVarsAnalysisFunctions





