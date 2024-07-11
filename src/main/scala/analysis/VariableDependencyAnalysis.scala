package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*
import util.Logger

trait VariableDependencyAnalysisFunctions(
  variables: Set[Taintable],
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  procedure: Procedure,
) extends ForwardIDEAnalysis[Taintable, Set[Taintable], PowersetLattice[Taintable]] {
  val valuelattice = PowersetLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge, JoinEdge}

  private val stackPointer = Register("R31", 64)

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: IndirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    Map(d -> IdEdge())
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: GoTo)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    d match {
      case Left(_) => Map()
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    def getVars(expression: Expr): Set[Taintable] = {
      val vars: Set[Taintable] = expression.variables.map { v => v: Taintable }
      val loads: Set[Taintable] = expression.loads.map { l => getMemoryVariable(n, l.mem, l.index, l.size, constProp, globals).getOrElse(UnknownMemory()) }
      vars ++ loads
    }

    if n == procedure then d match {
      // At the start of the procedure, no variables should depend on anything.
      case Left(_) => Map()
      case Right(_) => variables.foldLeft(Map(d -> IdEdge())) {
        (m: Map[DL, EdgeFunction[Set[Taintable]]], v) => m + (Left(v) -> ConstEdge(Set(v)))
      }
    } else n match {
      case Assign(variable, expression, _) => {
        val vars = getVars(expression)
        d match {
          case Left(v) if vars.contains(v) => Map(d -> JoinEdge(vars), Left(variable) -> JoinEdge(vars))
          case Left(v) if v == variable => Map()
          case _ => Map(d -> IdEdge())
        }
      }
      case MemoryAssign(mem, index, expression, _, size, _) => {
        val variable = getMemoryVariable(n, mem, index, size, constProp, globals).getOrElse(UnknownMemory())
        
        val vars = getVars(expression)
        d match {
          case Left(v) if vars.contains(v) => Map(d -> JoinEdge(vars), Left(variable) -> JoinEdge(vars))
          case Left(v) if v == variable && v != UnknownMemory() => Map()
          case _ => Map(d -> IdEdge())
        }
      }
      case _ => Map(d -> IdEdge())
    }
  }
}

/**
 * Calculates the set of variables that a variable has been affected by at each CFG node, starting at the given procedure.
 */
class VariableDependencyAnalysis(
  program: Program,
  variables: Set[Taintable],
  globals: Map[BigInt, String],
  mmm: MemoryModelMap,
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  procedure: Procedure,
) extends ForwardIDESolver[Taintable, Set[Taintable], PowersetLattice[Taintable]](program),
    VariableDependencyAnalysisFunctions(variables, globals, mmm, constProp, procedure)
{
  override def phase2Init: Set[Taintable] = Set(Register("R0", 64))
}
