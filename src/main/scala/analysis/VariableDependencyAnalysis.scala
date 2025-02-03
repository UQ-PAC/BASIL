package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import util.StaticAnalysisLogger

import scala.collection.mutable

trait ProcVariableDependencyAnalysisFunctions(
  relevantGlobals: Set[Variable],
  varDepsSummaries: Map[Procedure, Map[Variable, LatticeSet[Variable]]],
  procedure: Procedure,
) extends ForwardIDEAnalysis[Variable, LatticeSet[Variable], LatticeSetLattice[Variable]] {
  val valuelattice = LatticeSetLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge, JoinEdge}
  import LatticeSet.*

  private val reachable = procedure.reachableFrom

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    if varDepsSummaries.contains(entry) then Map() else Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    // TODO is this reachability actually needed/helpful?
    if reachable.contains(aftercall.parent.parent) then {
      d match {
        case Left(v: LocalVar) => exit.outParams.foldLeft(Map(d -> IdEdge())) {
          case (m, (o, e)) => {
            if e.variables.contains(v) then m + (Left(o) -> IdEdge()) else m
          }
        }
        case _ => Map(d -> IdEdge())
      }
    } else Map()
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    d match {
      case Left(v) =>
        // PERFORMANCE:
        // There is a lot of wasted work done here
        // We iterate over the summary map many times, once for each d: DL
        // This could possibly be improved by transposing the summary maps, but there is some difficulty with how
        // this works with Top elements.
        varDepsSummaries.get(call.target).map {
          _.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]]()) {
            case (m, (v2, s)) => if s.contains(v) then m + (Left(v2) -> IdEdge()) else m
          }
          .getOrElse(Map())
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    if n == procedure then d match {
      // At the start of the procedure, no variables should depend on anything but themselves.
      case Left(_) => Map()
      case Right(_) =>
        (relevantGlobals ++ procedure.formalInParam).foldLeft(Map(d -> IdEdge())) {
          (m: Map[DL, EdgeFunction[LatticeSet[Variable]]], v) => m + (Left(v) -> ConstEdge(FiniteSet(Set(v))))
        }
    } else n match {
      case LocalAssign(assigned, expression, _) =>
        val vars = expression.variables
        d match {
          case Left(v) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
          case Left(v) if v == assigned => Map()
          // This needs to be FiniteSet(Set()) and not Bottom() since Bottom() means something
          // special to the IDE solver
          case _ => Map(d -> IdEdge(), Left(assigned) -> ConstEdge(FiniteSet(Set())))
        }
      case MemoryLoad(lhs, mem, index, _, size, _) => d match {
        case Left(_) => Map(d -> IdEdge())
        // An approximation of memory loading is just that the read values could have been anything at all
        // This could perhaps be more made precise by having dependencies on individual memory regions
        case Right(_) => Map(d -> IdEdge(), Left(lhs) -> ConstEdge(Top()))
      }
      // TODO
      // this may be out of place, as this is an exit node
      // see the IDE solver comment in phase 2's process function
      case Return(_, out) =>
        out.toMap.flatMap {
          (assigned, expression) => {
            val vars = expression.variables
            d match {
              case Left(v: Variable) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
              case _ => Map(d -> IdEdge())
            }
          }
        }
      case _ => Map(d -> IdEdge())
    }
  }
}

/**
 * Computes (for a given procedure, and interprocedurally), for each variable, the set of variables whose value
 * has been in an expression affecting the current assigned value of this variable. Equivalently, it gives a
 * set of variables such that the gamma of this variable is less than the join of the set.
 *
 * `relevantGlobals` is the set of global variables are read in the procedure without being assigned.
 * Effectively, they are input variables, though they are not in the formalInParam of the procedure.
 * If procedures have not been rewritten to parameter form, this could be the set of registers.
 *
 * For interprocedurality, `varDepsSummaries` can contain a summary of the result of an analysis for some given
 * procedures. This means that we can avoid inspecting the contents of the procedure to compute our result.
 * Note that this analysis is still interprocedural without this map containing any values, the summaries
 * are only an optimisation.
 */
class ProcVariableDependencyAnalysis(
  program: Program,
  relevantGlobals: Set[Variable],
  varDepsSummaries: Map[Procedure, Map[Variable, LatticeSet[Variable]]],
  procedure: Procedure,
) extends ForwardIDESolver[Variable, LatticeSet[Variable], LatticeSetLattice[Variable]](program),
    ProcVariableDependencyAnalysisFunctions(relevantGlobals, varDepsSummaries, procedure)
{
  override def start: CFGPosition = procedure
}

/**
 * Computes the results of a `ProcVariableDependencyAnalysis` on each procedure in the given program. For
 * performance, scc must be the set of strongly connected components of the call graph of the program in
 * a reverse topological order (see `stronglyConnectedComponents` in '../IRCursor.scala'). The ordering
 * allows a significantly faster interprocedural analysis to be performed, by computing summaries of
 * procedures before they are called.
 */
class VariableDependencyAnalysis(
  program: Program,
  scc: List[Set[Procedure]],
  parameterForm: Boolean = false,
) {
  val relevantGlobals: Set[Variable] = if parameterForm then Set() else 0.to(31).map { n =>
    Register(s"R$n", 64)
  }.toSet

  def analyze(): Map[Procedure, Map[Variable, LatticeSet[Variable]]] = {
    scc.flatten.filter(_.blocks.nonEmpty).foldLeft(Map[Procedure, Map[Variable, LatticeSet[Variable]]]()) {
      (varDepsSummaries, procedure) => {
        StaticAnalysisLogger.debug("Generating variable dependencies for " + procedure)
        var varDepResults = ProcVariableDependencyAnalysis(program, relevantGlobals,
          varDepsSummaries, procedure).analyze()
        val varDepMap = IRWalk.lastInProc(procedure).flatMap(varDepResults.get(_)).getOrElse(Map())
        varDepsSummaries + (procedure -> varDepMap)
      }
    }
  }
}
