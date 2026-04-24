package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import util.StaticAnalysisLogger

private def getLiveVars(p: Procedure): Map[CFGPosition, Set[Variable]] = {
  transforms.reversePostOrder(p)
  val liveVarsDom = transforms.IntraLiveVarsDomain()
  val (before, after) = transforms.getLiveVars(p)
  /* Taken from Simp.scala
   * This should probably be made into a generic function
   */
  after
    .flatMap((block, sts) => {
      val b = Seq(IRWalk.lastInBlock(block) -> sts)
      val stmts =
        if (block.statements.nonEmpty) then
          (List(block.jump) ++ block.statements.toList.tail.reverse).zip(block.statements.toList.reverse)
        else List()
      val transferred = stmts
        .foldLeft((sts, List[(CFGPosition, Set[Variable])](block -> sts)))((st, s) => {
          val x = liveVarsDom.transfer(st._1, s._1)
          (x, (s._2 -> x) :: st._2)
        })
        ._2
        .toMap
      b ++ transferred
    })
}

// TODO this seems to break on mutually recursive procedures that may not terminate

trait ProcVariableDependencyAnalysisFunctions(
  relevantGlobals: Set[Variable],
  varDepsSummaries: Map[Procedure, Map[Variable, LatticeSet[Variable]]],
  procedure: Procedure,
  parameterForm: Boolean
)(using latticeset: Lattice[LatticeSet[Variable]])
    extends ForwardIDEAnalysis[Variable, LatticeSet[Variable], Lattice[LatticeSet[Variable]]] {
  val valuelattice = latticeset
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}
  import LatticeSet.*

  private val reachable = procedure.reachableFrom
  private val liveVars = getLiveVars(procedure)

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    if varDepsSummaries.contains(entry) then Map()
    else if !parameterForm then Map(d -> IdEdge())
    else
      d match {
        case Left(v) =>
          call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]]()) { case (m, (inVar, expr)) =>
            if expr.variables.contains(v) then m + (Left(inVar) -> IdEdge()) else m
          }
        case Right(_) =>
          call.actualParams.toList.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]](d -> IdEdge())) {
            case (m, (inVar, _)) => m + (Left(inVar) -> ConstEdge(FiniteSet(Set())))
          }
      }
  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    // TODO is this reachability actually needed/helpful?
    if reachable.contains(aftercall.parent.parent) then {
      if !parameterForm then
        d match {
          case Left(v: LocalVar) =>
            exit.outParams.foldLeft(Map(d -> IdEdge())) {
              case (m, (o, e)) => {
                if e.variables.contains(v) then m + (Left(o) -> IdEdge()) else m
              }
            }
          case _ => Map(d -> IdEdge())
        }
      else {
        val call: DirectCall = aftercall match {
          case aftercall: Statement => aftercall.parent.statements.getPrev(aftercall).asInstanceOf[DirectCall]
          case _: Jump => aftercall.parent.statements.last.asInstanceOf[DirectCall]
        }

        d match {
          case Left(v) =>
            exit.outParams.toList
              .flatMap((retVar, expr) => {
                if expr.variables.contains(v)
                then List(Left(call.outParams(retVar)) -> IdEdge())
                else List()
              })
              .toMap
          case Right(_) => Map(d -> IdEdge())
        }
      }
    } else Map()
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    if !parameterForm then
      d match {
        case Left(v) =>
          varDepsSummaries
            .get(call.target)
            .map(summary => {
              summary.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]]()) { case (m, (outVar, deps)) =>
                if deps.contains(v) then m + (Left(outVar) -> IdEdge()) else m
              }
            })
            .getOrElse(Map())
        case Right(_) => Map(d -> IdEdge())
      }
    else
      varDepsSummaries.get(call.target) match {
        case Some(summary) =>
          d match {
            // PERFORMANCE:
            // There is a lot of wasted work done here
            // We iterate over the summary map many times, once for each d: DL
            // This could possibly be improved by transposing the summary maps, but there is some difficulty with how
            // this works with Top elements.

            // TODO handle modified global variables
            case Left(v) => {
              // If the variable is assigned to in this call, reassign its value, else keep it.
              val init: Map[DL, EdgeFunction[LatticeSet[Variable]]] =
                if call.outParams.exists(_._2 == v) then Map() else Map(d -> IdEdge())

              call.actualParams.foldLeft(init) { case (m, (inVar, expr)) =>
                if !expr.variables.contains(v) then m
                else {
                  summary.foldLeft(m) { case (m, (endVar, deps)) =>
                    endVar match {
                      case endVar: LocalVar if call.target.formalOutParam.contains(endVar) => {
                        if deps.contains(inVar) then m + (Left(call.outParams(endVar)) -> IdEdge())
                        else m
                      }
                      case _ => m
                    }
                  }
                }
              }
            }
            case Right(_) =>
              val initialise = call.outParams.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]](d -> IdEdge())) {
                case (m, (formalVar, resultVar)) => m + (Left(resultVar) -> ConstEdge(FiniteSet(Set())))
              }
              val ret = summary.foldLeft(initialise) { case (m, (endVar, deps)) =>
                endVar match {
                  case endVar: LocalVar if call.target.formalOutParam.contains(endVar) =>
                    deps match {
                      case Top() | DiffSet(_) => m + (Left(call.outParams(endVar)) -> ConstEdge(Top()))
                      case FiniteSet(s) if s == Set() =>
                        m + (Left(call.outParams(endVar)) -> ConstEdge(FiniteSet(Set())))
                      case _ => m
                    }
                  case _ => m
                }
              }
              ret
          }
        case None =>
          d match {
            case Left(v) if call.outParams.exists(_._2 == v) => Map()
            case Left(v) => Map(d -> IdEdge())
            case Right(_) =>
              call.outParams.foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]](d -> IdEdge())) {
                case (m, (outVar, resultVar)) => m + (Left(resultVar) -> ConstEdge(FiniteSet(Set())))
              }
          }
      }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[LatticeSet[Variable]]] = {
    if n == procedure then
      d match {
        // At the start of the procedure, no variables should depend on anything but themselves.
        case Left(_) => Map()
        case Right(_) =>
          (relevantGlobals ++ procedure.formalInParam).foldLeft(Map(d -> IdEdge())) {
            (m: Map[DL, EdgeFunction[LatticeSet[Variable]]], v) => m + (Left(v) -> ConstEdge(FiniteSet(Set(v))))
          }
      }
    else
      n match {
        case LocalAssign(assigned, expression, _) =>
          val vars = expression.variables
          d match {
            case Left(v) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
            case Left(v) if v == assigned => Map()
            case Left(v) if liveVars.get(n).exists(!_.contains(v)) => Map()
            case Left(_) => Map(d -> IdEdge())
            // This needs to be FiniteSet(Set()) and not Bottom() since Bottom() means something
            // special to the IDE solver
            case Right(_) =>
              Map(d -> IdEdge(), Left(assigned) -> ConstEdge(FiniteSet(Set())))
          }
        case MemoryLoad(lhs, mem, index, _, size, _) =>
          d match {
            case Left(v) if v == lhs => Map()
            case Left(_) => Map(d -> IdEdge())
            // An approximation of memory loading is just that the read values could have been anything at all
            // This could perhaps be more made precise by having dependencies on individual memory regions
            case Right(_) => Map(d -> IdEdge(), Left(lhs) -> ConstEdge(Top()))
          }
        case IndirectCall(_, _) =>
          d match {
            case Left(_) => Map(d -> ConstEdge(Top()))
            case Right(_) => Map(d -> IdEdge())
          }
        case call: DirectCall => {
          d match {
            case Left(v: LocalVar) if call.outParams.exists(_._2 == v) => Map()
            case Left(v) => Map(d -> IdEdge())
            case Right(_) =>
              call.outParams.map(_._2).foldLeft(Map[DL, EdgeFunction[LatticeSet[Variable]]](d -> IdEdge())) { (m, v) =>
                m + (Left(v) -> ConstEdge(Top()))
              }
          }
        }
        // TODO
        // this may be out of place, as this is an exit node
        // see the IDE solver comment in phase 2's process function
        case Return(_, out) =>
          out.toMap.flatMap { (assigned, expression) =>
            {
              val vars = expression.variables
              d match {
                case Left(v) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
                case Left(_) => Map(d -> IdEdge())
                case Right(_) =>
                  Map(d -> IdEdge(), Left(assigned) -> ConstEdge(FiniteSet(Set())))
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
  parameterForm: Boolean = false
) extends ForwardIDESolver[Variable, LatticeSet[Variable], Lattice[LatticeSet[Variable]]](program),
      ProcVariableDependencyAnalysisFunctions(relevantGlobals, varDepsSummaries, procedure, parameterForm) {
  override def start: CFGPosition = procedure
}

/**
 * Computes the results of a `ProcVariableDependencyAnalysis` on each procedure in the given program. For
 * performance, scc must be the set of strongly connected components of the call graph of the program in
 * a reverse topological order (see `stronglyConnectedComponents` in '../IRCursor.scala'). The ordering
 * allows a significantly faster interprocedural analysis to be performed, by computing summaries of
 * procedures before they are called.
 */
class VariableDependencyAnalysis(program: Program, scc: List[Set[Procedure]], parameterForm: Boolean = false) {
  val relevantGlobals: Set[Variable] =
    if parameterForm then Set()
    else
      0.to(31)
        .map { n =>
          Register(s"R$n", 64)
        }
        .toSet

  def analyze(): Map[Procedure, Map[Variable, LatticeSet[Variable]]] = {
    scc.flatten.filter(_.blocks.nonEmpty).foldLeft(Map[Procedure, Map[Variable, LatticeSet[Variable]]]()) {
      (varDepsSummaries, procedure) =>
        {
          StaticAnalysisLogger.debug("Generating variable dependencies for " + procedure)
          var varDepResults =
            ProcVariableDependencyAnalysis(program, relevantGlobals, varDepsSummaries, procedure, parameterForm)
              .analyze()
          val varDepMap = IRWalk.lastInProc(procedure).flatMap(varDepResults.get(_)).getOrElse(Map())
          varDepsSummaries + (procedure -> varDepMap)
        }
    }
  }
}
