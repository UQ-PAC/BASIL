package analysis

import ir._
import analysis.solvers.SimpleWorklistFixpointSolver

case class ReachingDefinitionsAnalysis(cfg: ProgramCfg) {

  type Definition = LocalAssign // local assign is a definition because it is a statement and statements are assumed to be unique
  type TupleElement =
    TupleLattice[MapLattice[Variable, Set[LocalAssign], PowersetLattice[LocalAssign]], MapLattice[Variable, Set[LocalAssign], PowersetLattice[LocalAssign]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]]

  val tupleLattice: TupleLattice[MapLattice[Variable, Set[LocalAssign], PowersetLattice[
    LocalAssign]], MapLattice[Variable, Set[LocalAssign], PowersetLattice[
    LocalAssign]], Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]] =
    new TupleLattice(
      new MapLattice[Variable, Set[LocalAssign], PowersetLattice[LocalAssign]](new PowersetLattice[LocalAssign]()),
      new MapLattice[Variable, Set[LocalAssign], PowersetLattice[LocalAssign]](new PowersetLattice[LocalAssign]())
    )

  val lattice: MapLattice[CfgNode, (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]), TupleElement] = MapLattice(
    tupleLattice
  )

  val domain: Set[CfgNode] = cfg.nodes.toSet

  val first: Set[CfgNode] = Set(cfg.startNode)

  def transfer(n: CfgNode, s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) =
    localTransfer(n, s)

  def localTransfer(
      n: CfgNode,
      s: (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]])
  ): (Map[Variable, Set[Definition]], Map[Variable, Set[Definition]]) = n match {
    case cmd: CfgCommandNode =>
      eval(cmd.data, s)
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
          val rhsDef = s._1.getOrElse(v, Set.empty)
          acc + (v -> rhsDef)
      }
      (s._1 + (lhs -> Set(localAssign)), rhsUseDefs)
    case _ => s
  }
}

class ReachingDefinitionsAnalysisSolver(
    cfg: ProgramCfg
) extends ReachingDefinitionsAnalysis(cfg)
    with IntraproceduralForwardDependencies
    with Analysis[Map[CfgNode, (Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]])]]
    with SimpleWorklistFixpointSolver[
      CfgNode,
      (Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]], Map[Variable, Set[ReachingDefinitionsAnalysis#Definition]]), ReachingDefinitionsAnalysis#TupleElement] {}
