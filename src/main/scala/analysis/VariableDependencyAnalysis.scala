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
          m => m.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]]()) {
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
          case _ => Map(d -> IdEdge())
        }
      case MemoryLoad(lhs, mem, index, _, size, _) => d match {
        case Left(_) => Map(d -> IdEdge())
        // Sound approximation of memory loading
        // This could be more made precise by having dependencies on individual memory regions
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

/** Calculates the set of "input variables" that a variable has been affected by at each CFG node, starting at the given
  * procedure.
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

class VariableDependencyAnalysis(
  program: Program,
  scc: mutable.ListBuffer[mutable.Set[Procedure]],
  simplified: Boolean = false,
) {
  val relevantGlobals: Set[Variable] = if simplified then Set() else 0.to(31).map { n =>
    Register(s"R$n", 64)
  }.toSet

  def analyze(): Map[Procedure, Map[Variable, LatticeSet[Variable]]] = {
    var varDepsSummaries = Map[Procedure, Map[Variable, LatticeSet[Variable]]]()
    scc.flatten.filter(_.blocks.nonEmpty).foreach {
      procedure => {
        StaticAnalysisLogger.debug("Generating variable dependencies for " + procedure)
        var varDepResults = ProcVariableDependencyAnalysis(program, relevantGlobals,
          varDepsSummaries, procedure).analyze()
        StaticAnalysisLogger.debug(varDepResults)
        val varDepMap = IRWalk.lastInProc(procedure).flatMap(varDepResults.get(_)).getOrElse(Map())
        varDepsSummaries += procedure -> varDepMap
      }
    }
    varDepsSummaries
  }
}
