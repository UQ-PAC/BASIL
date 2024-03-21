package analysis

import ir._
import analysis.solvers.SimpleWorklistFixpointSolver

case class ReachingDefinitionsAnalysis(cfg: ProgramCfg) {

  type Definition = LocalAssign // local assign is a definition because it is a statement and statements are assumed to be unique
  type TupleElement =
    TupleLattice[MapLattice[Variable, Option[Definition], SingleElementLattice[Definition]], MapLattice[Variable, Set[Definition], PowersetLattice[LocalAssign]], Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]]

  val tupleLattice: TupleLattice[MapLattice[Variable, Option[Definition], SingleElementLattice[Definition]], MapLattice[Variable, Set[LocalAssign], PowersetLattice[
    LocalAssign]], Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]] =
    new TupleLattice(
      new MapLattice[Variable, Option[Definition], SingleElementLattice[Definition]](new SingleElementLattice[Definition]()),
      new MapLattice[Variable, Set[LocalAssign], PowersetLattice[LocalAssign]](new PowersetLattice[LocalAssign]())
    )

  val lattice: MapLattice[CfgNode, (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]), TupleElement] = MapLattice(
    tupleLattice
  )

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  /*
   * Good enough as stmts are unique
   */
  private def generateUniqueDefinition(
      variable: Variable
  ): LocalAssign = {
    LocalAssign(variable, BitVecLiteral(0, 0))
  }

  def transfer(n: CfgNode, s: (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]])): (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]) =
    localTransfer(n, s)

  def localTransfer(
      n: CfgNode,
      s: (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]) = n match {
    case cmd: CfgCommandNode =>
      eval(cmd.data, s)
    case _ => s
  }

  def eval(cmd: Command, s: (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Option[Definition]], Map[Variable, Set[Definition]]) = cmd match {
    case localAssign: LocalAssign =>
      // do the rhs first (should reset the values for this node to the empty set)
      // for each variable in the rhs, find the definitions from the lattice lhs and add them to the lattice rhs
      // for lhs, addOrReplace the definition
      val rhs = localAssign.rhs.variables
      val lhs = localAssign.lhs
      val rhsUseDefs: Map[Variable, Set[Definition]] = rhs.foldLeft(Map.empty[Variable, Set[Definition]]) {
        case (acc, v) =>
          //val rhsDef = if s._1(v).isDefined then s._1(v).get else None
          // TODO: may be good to assign a unique identifier instead of an empty set
          acc + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v))))
      }
      (s._1 + (lhs -> Option(localAssign)), rhsUseDefs)
    case assert: Assert =>
      assert.body.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v)))))
      }
    case memoryAssign: MemoryAssign =>
      memoryAssign.rhs.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v)))))
      }
      memoryAssign.lhs.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v)))))
      }
    case assume: Assume =>
      assume.body.variables.foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v)))))
      }
    case indirectCall: IndirectCall =>
      Set(indirectCall.target).foldLeft(s) {
        case (acc, v) =>
          (acc._1, acc._2 + (v -> (if s._1(v).isDefined then Set(s._1(v).get) else Set(generateUniqueDefinition(v)))))
      }
    case _ => s
  }
}

class ReachingDefinitionsAnalysisSolver(
    cfg: ProgramCfg
) extends ReachingDefinitionsAnalysis(cfg)
    with IntraproceduralForwardDependencies
    with Analysis[Map[CfgNode, (Map[Variable, Option[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]])]]
    with SimpleWorklistFixpointSolver[
      CfgNode,
      (Map[Variable, Option[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]]), ReachingDefinitionsAnalysis#TupleElement] {}
