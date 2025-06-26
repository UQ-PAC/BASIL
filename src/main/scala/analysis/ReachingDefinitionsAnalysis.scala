package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.*

type ReachingDefTuple =
  TupleLattice[MapLattice[Variable, Set[Assign], PowersetLattice[Assign]], MapLattice[Variable, Set[
    Assign
  ], PowersetLattice[Assign]], Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]]

/** Calculates def-use chains for the program. */
trait ReachingDefinitionsAnalysis(program: Program) {

  private val tupleLattice: TupleLattice[
    MapLattice[Variable, Set[Assign], PowersetLattice[Assign]],
    MapLattice[Variable, Set[Assign], PowersetLattice[Assign]],
    Map[Variable, Set[Assign]],
    Map[Variable, Set[Assign]]
  ] =
    TupleLattice(
      MapLattice[Variable, Set[Assign], PowersetLattice[Assign]](PowersetLattice[Assign]()),
      MapLattice[Variable, Set[Assign], PowersetLattice[Assign]](PowersetLattice[Assign]())
    )

  val lattice: MapLattice[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]), ReachingDefTuple] =
    MapLattice(tupleLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(
    n: CFGPosition,
    s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])
  ): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = {
    n match {
      case cmd: Command =>
        eval(cmd, s)
      case _ => s
    }
  }

  private def transformUses(
    vars: Set[Variable],
    s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])
  ): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = {
    vars.foldLeft((s(0), Map.empty[Variable, Set[Assign]])) { case ((state, acc), v) =>
      (state, acc + (v -> state(v)))
    }
  }

  def eval(
    cmd: Command,
    s: (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])
  ): (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]) = cmd match {
    case assign: LocalAssign =>
      // do the rhs first (should reset the values for this node to the empty set)
      // for each variable in the rhs, find the definitions from the lattice lhs and add them to the lattice rhs
      // for lhs, addOrReplace the definition
      val rhs = assign.rhs.variables
      val lhs = assign.lhs
      val rhsUseDefs: Map[Variable, Set[Assign]] = rhs.foldLeft(Map.empty[Variable, Set[Assign]]) { case (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> Set(assign)), rhsUseDefs)
    case assert: Assert =>
      transformUses(assert.body.variables, s)
    case memoryStore: MemoryStore =>
      transformUses(memoryStore.index.variables ++ memoryStore.value.variables, s)
    case memoryLoad: MemoryLoad =>
      val lhs = memoryLoad.lhs
      val rhs = memoryLoad.index.variables
      val rhsUseDefs: Map[Variable, Set[Assign]] = rhs.foldLeft(Map.empty[Variable, Set[Assign]]) { case (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> Set(memoryLoad)), rhsUseDefs)
    case assume: Assume =>
      transformUses(assume.body.variables, s)
    case indirectCall: IndirectCall =>
      transformUses(indirectCall.target.variables, s)
    case r: DirectCall =>
      transformUses(r.actualParams.toSet.flatMap(_._2.variables), s)
    case r: Return =>
      transformUses(r.outParams.toSet.flatMap(_._2.variables), s)
    case _ => s
  }
}

class InterprocReachingDefinitionsAnalysisSolver(program: Program)
    extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[
      CFGPosition,
      (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]),
      ReachingDefTuple
    ]
    with IRInterproceduralForwardDependencies

class IntraprocReachingDefinitionsAnalysisSolver(program: Program)
    extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[
      CFGPosition,
      (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]]),
      ReachingDefTuple
    ]
    with IRIntraproceduralForwardDependencies
