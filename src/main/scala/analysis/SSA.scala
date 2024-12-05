package analysis

import ir.*
import analysis.solvers.SimpleWorklistFixpointSolver

type SSATuple =
  TupleLattice[MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]]

private var ssaCount: Int = 0

private def nextSSACount() = {
  ssaCount += 1
  ssaCount
}

trait SSA(program: Program, writesTo: Map[Procedure, Set[Register]], RNAResult: Map[CFGPosition, Set[Variable]]) {

  private val tupleLattice: TupleLattice[MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]], Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]] =
    TupleLattice(
      MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]](FlatLatticeWithDefault[Int](nextSSACount)),
      MapLattice[Variable, FlatElement[Int], FlatLatticeWithDefault[Int]](FlatLatticeWithDefault[Int](nextSSACount))
    )

  val lattice: MapLattice[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]), SSATuple] = MapLattice(
    tupleLattice
  )

  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) =
    localTransfer(n, s)

  def localTransfer(
    n: CFGPosition,
    s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])
  ): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = n match {
    case cmd: Command =>
      eval(cmd, s)
    case procedure: Procedure =>
      // get params from RNA
      // for every param assign a fresh SSA variable
      val params = RNAResult(procedure).map(v => (v, FlatEl(nextSSACount()))) // TODO: more advanced handling of params required
      (params.toMap, Map.empty)
    case _ => s
  }

  private def transformUses(vars: Set[Variable], s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])): (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = {
    vars.foldLeft((s(0), Map.empty[Variable, FlatElement[Int]])) {
      case ((state, acc), v) =>
        (state, acc + (v -> state(v)))
    }
  }

  def eval(cmd: Command, s: (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]])):
    (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]) = cmd match {
    case assign: LocalAssign =>
      val rhs = assign.rhs.variables
      val lhs = assign.lhs
      val rhsUseDefs = rhs.foldLeft(Map.empty[Variable, FlatElement[Int]]) { (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> FlatEl(nextSSACount())), rhsUseDefs)
    case load: MemoryLoad =>
      val rhs = load.index.variables
      val lhs = load.lhs
      val rhsUseDefs = rhs.foldLeft(Map.empty[Variable, FlatElement[Int]]) { (acc, v) =>
        acc + (v -> s(0)(v))
      }
      (s(0) + (lhs -> FlatEl(nextSSACount())), rhsUseDefs)
    case assert: Assert =>
      transformUses(assert.body.variables, s)
    case store: MemoryStore =>
      transformUses(store.index.variables ++ store.value.variables, s)
    case assume: Assume =>
      transformUses(assume.body.variables, s)
    case indirectCall: IndirectCall =>
      transformUses(indirectCall.target.variables, s)
    case directCall: DirectCall =>
      writesTo.get(directCall.target) match {
        case Some(registers) =>
          val result = registers.foldLeft(Map[Variable, FlatElement[Int]]()) { (m, register) =>
            m + (register -> FlatEl(nextSSACount()))
          }
          (s(0) ++ result, s(1))
        case None =>
          s
      }
    case _ => s
  }
}

class IntraprocSSASolver(program: Program, writesTo: Map[Procedure, Set[Register]], RNAResult: Map[CFGPosition, Set[Variable]])
  extends SSA(program, writesTo, RNAResult)
    with IRIntraproceduralForwardDependencies
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, FlatElement[Int]], Map[Variable, FlatElement[Int]]), SSATuple]
