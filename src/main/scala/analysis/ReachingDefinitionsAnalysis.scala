package analysis

import ir.*
import analysis.solvers.SimpleWorklistFixpointSolver

type TupleElement =
  TupleLattice[MapLattice[Variable, Set[Assign], PowersetLattice[Assign]], MapLattice[Variable, Set[Assign], PowersetLattice[Assign]], Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]]

trait ReachingDefinitionsAnalysis(program: Program) {

  private val tupleLattice: TupleLattice[MapLattice[Variable, Set[Assign], PowersetLattice[Assign]], MapLattice[Variable, Set[Assign], PowersetLattice[
    Assign]], Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]] =
    TupleLattice(
      MapLattice[Variable, Set[Assign], PowersetLattice[Assign]](PowersetLattice[Assign]()),
      MapLattice[Variable, Set[Assign], PowersetLattice[Assign]](PowersetLattice[Assign]())
    )

  val lattice: MapLattice[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]), TupleElement] = MapLattice(
    tupleLattice
  )

  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) =
    localTransfer(n, s)

  def localTransfer(
      n: CFGPosition,
      s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])
  ): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = n match {
    case cmd: Command =>
      eval(cmd, s)
    case _ => s
  }

  private def transformUses(vars: Set[Variable], s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = {
    vars.foldLeft((s(0), Map.empty[Variable, Set[Assign]])) {
      case ((state, acc), v) =>
        (state, acc + (v -> state(v)))
    }
  }

  def eval(cmd: Command, s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])):
    (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = cmd match {
    case assign: Assign =>
      // do the rhs first (should reset the values for this node to the empty set)
      // for each variable in the rhs, find the definitions from the lattice lhs and add them to the lattice rhs
      // for lhs, addOrReplace the definition
      val rhs = assign.rhs.variables
      val lhs = assign.lhs
      val rhsUseDefs: Map[Variable, Set[Assign]] = rhs.foldLeft(Map.empty[Variable, Set[Assign]]) {
        case (acc, v) =>
          acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> Set(assign)), rhsUseDefs)
    case assert: Assert =>
      transformUses(assert.body.variables, s)
    case memoryAssign: MemoryAssign =>
      transformUses(memoryAssign.index.variables ++ memoryAssign.value.variables, s)
    case assume: Assume =>
      transformUses(assume.body.variables, s)
    case indirectCall: IndirectCall =>
      transformUses(indirectCall.target.variables, s)
    case _ => s
  }
}

class InterprocReachingDefinitionsAnalysisSolver(program: Program)
  extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]), TupleElement]
    with IRInterproceduralForwardDependencies

class IntraprocReachingDefinitionsAnalysisSolver(program: Program)
  extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]), TupleElement]
    with IRIntraproceduralForwardDependencies