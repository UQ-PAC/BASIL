package analysis
import ir.*
import analysis.solvers.*
import util.Logger
import collection.mutable


/**
 * Prepared arguments analysis described in _TypeArmour_ paper, https://ieeexplore.ieee.org/document/7546543/
 */

enum ParamState:
  case Unknown, Clear, WriteBeforeRead, ReadBeforeWrite

class ParamStateLattice extends Lattice[ParamState]:
  /** The bottom element of this lattice. */
  override val bottom: ParamState = ParamState.Clear // or WriteBeforeRead

  /** The top element of this lattice. Default: not implemented.
   */
  override def top: ParamState = ParamState.Unknown

  /** The least upper bound of `x` and `y`.
   */
  def lub(x: ParamState, y: ParamState): ParamState = {
    (x, y) match {
      case (_, ParamState.Unknown) => top
      case (ParamState.Unknown, _) => top
      case (ParamState.ReadBeforeWrite, o) => o
      case (o, ParamState.ReadBeforeWrite) => o
      case (ParamState.Clear, ParamState.WriteBeforeRead) => top
      case (ParamState.WriteBeforeRead, ParamState.Clear) => top
      case (ParamState.Clear, ParamState.Clear) => ParamState.Clear
      case (ParamState.WriteBeforeRead, ParamState.WriteBeforeRead) => ParamState.WriteBeforeRead
    }
  }


trait PreparedArgumentsAnalysisImpl:
  val valuelattice: ParamStateLattice = ParamStateLattice()
  val statelattice: MapLattice[Variable, ParamState, ParamStateLattice] = MapLattice(valuelattice)

  final val paramRegisters = (Range.inclusive(0, 7).map("R" + _) ++ Range.inclusive(0, 7).map("V" + _)).toSet
  /** Transfer function for state lattice elements. */
  def localTransfer(n: CFGPosition, s: statelattice.Element): statelattice.Element =
    n match
      case la: LocalAssign =>
        s ++ la.rhs.variables.filter(v => paramRegisters.contains(v.name)).map(v => v -> ParamState.ReadBeforeWrite)
        ++ (if paramRegisters.contains(la.lhs.name) then Some(la.lhs -> ParamState.WriteBeforeRead) else None)
      case ma: MemoryAssign =>
        s ++ ma.rhs.variables.filter(v => paramRegisters.contains(v.name)).map(v => v -> ParamState.ReadBeforeWrite)
      case c: Call => s ++ paramRegisters.filter(reg => s.keys.exists(v => v.name == reg)).map(n => Register(n, BitVecType(64)) -> ParamState.Clear).toMap
      case _ => s

object PreparedArgumentsAnalysis:
  class Solver(prog: Program) extends PreparedArgumentsAnalysisImpl
    with IRIntraproceduralDependencies.ForwardDependencies
    with Analysis[Map[CFGPosition, Map[Variable, ParamState]]]
    with SimplePushDownWorklistFixpointSolver[CFGPosition, Map[Variable, ParamState], MapLattice[Variable, ParamState, ParamStateLattice]]
    :
    /* Worklist initial set */
    override val lattice: MapLattice[CFGPosition, Map[Variable, ParamState], MapLattice[Variable, ParamState, ParamStateLattice]] = MapLattice(statelattice)

    override val domain : Set[CFGPosition] = computeDomain(IntraProcIRCursor, prog.procedures).toSet
    override def transfer(n: CFGPosition, s: statelattice.Element): statelattice.Element = localTransfer(n, s)

  def getFuncParams(results:Map[CFGPosition, Map[Variable, ParamState]]): Map[Call, Seq[Variable]] = {
    results.collect(m => m match {
      case (d: Call, r: Map[Variable, ParamState]) => d -> (r.collect(i => i match {
        case (v: Variable, p: ParamState) if p == ParamState.WriteBeforeRead => v
      })).toSeq
    })
  }

  def solveAndSimplify(prog: Program): Map[Call, Seq[Variable]] = {
    val solver = Solver(prog)
    val results: Map[CFGPosition, Map[Variable, ParamState]] = solver.analyze()
    getFuncParams(results)
  }
