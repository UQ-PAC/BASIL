package analysis

import ir._
import analysis.solvers.SimpleWorklistFixpointSolver

case class ReachingDefinitionsAnalysis(program: Program) {

  type Definition = LocalAssign // local assign is a definition because it is a statement and statements are assumed to be unique
  type TupleElement =
    TupleLattice[MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]]

  val tupleLattice: TupleLattice[MapLattice[Variable, Set[Definition], PowersetLattice[Definition]], MapLattice[Variable, Set[LocalAssign], PowersetLattice[
    LocalAssign]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]] =
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
  ): LocalAssign = {
    LocalAssign(variable, BitVecLiteral(0, 0))
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

  def eval(cmd: Command, s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) = cmd match {
    case localAssign: LocalAssign =>
      // do the rhs first (should reset the values for this node to the empty set)
      // for each variable in the rhs, find the definitions from the lattice lhs and add them to the lattice rhs
      // for lhs, addOrReplace the definition
      val rhs = localAssign.rhs.variables
      val lhs = localAssign.lhs
      val rhsUseDefs: Map[Variable, Set[Definition]] = rhs.foldLeft(Map.empty[Variable, Set[Definition]]) {
        case (acc, v) =>
          acc + (v -> s._1(v))
      }
      (s._1 + (lhs -> Set(localAssign)), rhsUseDefs)
    case assert: Assert =>
      assert.body.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> s._1(v)))
      }
    case memoryAssign: MemoryAssign =>
      memoryAssign.rhs.index.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> s._1(v)))
      }
      memoryAssign.rhs.value.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> s._1(v)))
      }
    case assume: Assume =>
      assume.body.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> s._1(v)))
      }
    case indirectCall: IndirectCall =>
      Set(indirectCall.target).foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> s._1(v)))
      }
    case _ => s
  }
}

class ReachingDefinitionsAnalysisSolver(program: Program)
  extends ReachingDefinitionsAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, (Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]]), ReachingDefinitionsAnalysis#TupleElement]
    with IRIntraproceduralForwardDependencies
