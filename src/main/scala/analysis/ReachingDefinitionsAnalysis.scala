package analysis

import ir.*
import analysis.solvers.SimpleWorklistFixpointSolver

case class ReachingDefinitionsAnalysis(program: Program) {

  type Definition = Assign // local assign is a definition because it is a statement and statements are assumed to be unique
  type TupleElement =
    TupleLattice[MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]]

  val tupleLattice: TupleLattice[MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], MapLattice[Variable, Set[Assign], PowersetLattice[
    Assign]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]] =
    new TupleLattice(
      new MapLattice[Variable, Set[Definition], PowersetLattice[Definition]](new PowersetLattice[Definition]()),
      new MapLattice[Variable, Set[Definition], PowersetLattice[Definition]](new PowersetLattice[Definition]())
    )

  val lattice: MapLattice[CFGPosition, (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]), TupleElement] = MapLattice(
    tupleLattice
  )

  val domain: Set[CFGPosition] = Set.empty ++ program

  /*
   * Good enough as stmts are unique
   */
  private def generateUniqueDefinition(
      variable: Variable
  ): Assign = {
    Assign(variable, BitVecLiteral(0, 0))
  }

  def transfer(n: CFGPosition, s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) =
    localTransfer(n, s)

  def localTransfer(
      n: CFGPosition,
      s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) = n match {
    case cmd: Command =>
      eval(cmd, s)
    case _ => s
  }

  def transformUses(vars: Set[Variable], s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) = {
    vars.foldLeft((s._1, Map.empty[Variable, Set[Definition]])) {
      case ((state, acc), v) =>
        (state, acc + (v -> state(v)))
    }
  }

  def eval(cmd: Command, s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) = cmd match {
    case assign: Assign =>
      // do the rhs first (should reset the values for this node to the empty set)
      // for each variable in the rhs, find the definitions from the lattice lhs and add them to the lattice rhs
      // for lhs, addOrReplace the definition
      val rhs = assign.rhs.variables
      val lhs = assign.lhs
      val rhsUseDefs: Map[Variable, Set[Definition]] = rhs.foldLeft(Map.empty[Variable, Set[Definition]]) {
        case (acc, v) =>
          acc + (v -> s._1(v))
      }
      (s._1 + (lhs -> Set(assign)), rhsUseDefs)
    case assert: Assert =>
      transformUses(assert.body.variables, s)
    case memoryAssign: MemoryAssign =>
      transformUses(memoryAssign.index.variables ++ memoryAssign.value.variables, s)
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

class ReachingDefinitionsAnalysisSolver(program: Program)
  extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]]), ReachingDefinitionsAnalysis#TupleElement]
    with IRIntraproceduralForwardDependencies
