package analysis

import analysis.solvers.ForwardIDESolver
import ir.*
import boogie.*
import util.Logger
import specification.SpecGlobal

import scala.collection.mutable

trait ProcVariableDependencyAnalysisFunctions(
  variables: Set[Taintable],
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]],
  procedure: Procedure,
) extends ForwardIDEAnalysis[Taintable, Set[Taintable], PowersetLattice[Taintable]] {
  val valuelattice = PowersetLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge, JoinEdge}

  private val stackPointer = Register("R31", 64)
  private val linkRegister = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoredRegisters = Set(stackPointer, linkRegister, framePointer)

  private val reachable = procedure.reachableFrom

  def edgesCallToEntry(call: DirectCall, entry: Procedure)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    if varDepsSummaries.contains(entry) then Map() else Map(d -> IdEdge())
  }

  def edgesExitToAfterCall(exit: Return, aftercall: Command)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    if reachable.contains(aftercall.parent.parent) then Map(d -> IdEdge()) else Map()
  }

  def edgesCallToAfterCall(call: DirectCall, aftercall: Command)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    d match {
      case Left(v) => varDepsSummaries.get(call.target).flatMap(_.get(v).map( _.foldLeft(Map[DL, EdgeFunction[Set[Taintable]]]()) {
        (m, d) => m + (Left(d) -> IdEdge())
      })).getOrElse(Map())
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[Set[Taintable]]] = {
    def getVars(expression: Expr): Set[Taintable] = {
      expression.variables.map { v => v: Taintable } ++
      expression.loads.map { l => getMemoryVariable(n, l.mem, l.index, l.size, constProp, globals).getOrElse(UnknownMemory()) }
    }

    if n == procedure then d match {
      // At the start of the procedure, no variables should depend on anything but themselves.
      case Left(_) => Map()
      case Right(_) => {
        variables.foldLeft(Map(d -> IdEdge())) {
          (m: Map[DL, EdgeFunction[Set[Taintable]]], v) => m + (Left(v) -> ConstEdge(Set(v)))
        }
      }
    } else n match {
      case Assign(assigned, expression, _) => {
        val vars = getVars(expression) -- ignoredRegisters
        d match {
          case Left(v) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
          case Left(v) if v == assigned => Map()
          case _ => Map(d -> IdEdge())
        }
      }
      case MemoryAssign(mem, index, expression, _, size, _) => {
        val assigned = getMemoryVariable(n, mem, index, size, constProp, globals).getOrElse(UnknownMemory())

        val vars = getVars(expression) -- ignoredRegisters
        d match {
          case Left(v) if vars.contains(v) => Map(d -> IdEdge(), Left(assigned) -> IdEdge())
          case Left(v) if v == assigned && v != UnknownMemory() => Map()
          case _ => Map(d -> IdEdge())
        }
      }
      case _ => Map(d -> IdEdge())
    }
  }
}

/**
 * Calculates the set of "input variables" that a variable has been affected by at each CFG node, starting at the given procedure.
 */
class ProcVariableDependencyAnalysis(
  program: Program,
  variables: Set[Taintable],
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  varDepsSummaries: Map[Procedure, Map[Taintable, Set[Taintable]]],
  procedure: Procedure,
) extends ForwardIDESolver[Taintable, Set[Taintable], PowersetLattice[Taintable]](program),
    ProcVariableDependencyAnalysisFunctions(variables, globals, constProp, varDepsSummaries, procedure)
{
  override def phase2Init: Set[Taintable] = Set(Register("R0", 64))

  override val startNode: CFGPosition = procedure
}

class VariableDependencyAnalysis(
  program: Program,
  specGlobals: Set[SpecGlobal],
  globals: Map[BigInt, String],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  scc: mutable.ListBuffer[mutable.Set[Procedure]],
) {
  val varDepVariables: Set[analysis.Taintable] = 0.to(28).map { n =>
    Register(s"R$n", 64)
  }.toSet ++ specGlobals.map { g =>
    analysis.GlobalVariable(dsl.mem, BitVecLiteral(g.address, 64), g.size, g.name)
  }

  def analyze(): Map[Procedure, Map[Taintable, Set[Taintable]]] = {
    var varDepsSummaries = Map[Procedure, Map[Taintable, Set[Taintable]]]()
    var varDepsSummariesTransposed = Map[Procedure, Map[Taintable, Set[Taintable]]]()
    scc.flatten.filter(_.blocks.nonEmpty).foreach {
      procedure => {
        Logger.debug("Generating variable dependencies for " + procedure)
        val varDepResults = ProcVariableDependencyAnalysis(program, varDepVariables, globals, constProp, varDepsSummariesTransposed, procedure).analyze()
        val varDepMap = varDepResults.getOrElse(IRWalk.lastInProc(procedure).getOrElse(procedure), Map())
        varDepsSummaries += procedure -> varDepMap
        varDepsSummariesTransposed += procedure -> varDepMap.foldLeft(Map[Taintable, Set[Taintable]]()) {
          (m, p) => {
            val (v, s) = p
            s.foldLeft(m) {
              (m, d) => m + (d -> (m.getOrElse(d, Set()) + v))
            }
          }
        }
      }
    }
    varDepsSummaries
  }
}
