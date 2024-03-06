package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, BitVecType, Call, DirectCall, Expr, GoTo, IndirectCall, LocalAssign, MemoryAssign, NOP, Register, Variable}

abstract class ReachingDefs(cfg: ProgramCfg) extends Analysis[Any] {

  val domain: Set[CfgNode] = cfg.nodes.toSet
  val lattice: MapLattice[CfgNode, Set[CfgStatementNode], PowersetLattice[CfgStatementNode]] = new MapLattice(new PowersetLattice[CfgStatementNode]())

  def transfer(n: CfgNode, s: Set[CfgStatementNode]): Set[CfgStatementNode] = {
    n match
      case statementNode: CfgStatementNode =>
        statementNode.data match
          case LocalAssign(variable, expr, maybeString) => (s -- s.filter(cfgStatementNode =>
            cfgStatementNode.data.asInstanceOf[LocalAssign].lhs == variable)) + statementNode
          case _ => s
      case _ => s
  }
}

class ReachingDefsAnalysis(cfg: ProgramCfg) extends ReachingDefs(cfg),  IntraproceduralForwardDependencies,
  SimpleWorklistFixpointSolver[CfgNode, Set[CfgStatementNode], PowersetLattice[CfgStatementNode]]
