package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, LocalAssign, MemoryAssign, Variable}

abstract class LivenessAnalysis(cfg:ProgramCfg) extends Analysis[Any] {
  val lattice: MapLattice[CfgNode, Set[Variable], PowersetLattice[Variable]] = new MapLattice(new PowersetLattice())
  val domain: Set[CfgNode] = Set.empty ++ cfg.nodes

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    {
      n match {
        case _: CfgFunctionExitNode => lattice.sublattice.bottom
        case sn: CfgStatementNode =>
          sn.data match {
            case LocalAssign(variable, expr, maybeString) => (s - variable ) ++ expr.variables
            case MemoryAssign(memory, store, maybeString) => s ++ store.index.variables ++ store.value.variables
            case Assume(expr, maybeString, maybeString1, bool) => s ++ expr.variables
            case Assert(expr, maybeString, maybeString1) => s ++ expr.variables
            case _ => ???
          }
        case _: CfgFunctionEntryNode  => lattice.sublattice.bottom
        case _ => s
      }
    }
}

class LivenessAnalysisWorklistSolver(cfg: ProgramCfg)
  extends LivenessAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode, Set[Variable], PowersetLattice[Variable]]
    with IntraproceduralBackwardDependencies



