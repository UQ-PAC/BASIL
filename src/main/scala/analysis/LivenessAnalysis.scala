package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, CFGPosition, Call, DirectCall, GoTo, IndirectCall, InterProcIRCursor, IntraProcIRCursor, Jump, LocalAssign, MemoryAssign, NOP, Program, Statement, Variable}

//abstract class LivenessAnalysis(cfg:ProgramCfg) extends Analysis[Any] {
//  val lattice: MapLattice[CfgNode, Set[Variable], PowersetLattice[Variable]] = new MapLattice(new PowersetLattice())
//  val domain: Set[CfgNode] = Set.empty ++ cfg.nodes
//
//  /**
//   * Liveness Analysis base class
//   */
//  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
//    {
//      n match {
//        case _: CfgFunctionExitNode => lattice.sublattice.bottom
//        case sn: CfgStatementNode =>
//          sn.data match {
//            case LocalAssign(variable, expr, maybeString) => (s - variable) ++ expr.variables
//            case MemoryAssign(memory, store, maybeString) => s ++ store.index.variables ++ store.value.variables
//            case Assume(expr, maybeString, maybeString1, bool) => s ++ expr.variables
//            case Assert(expr, maybeString, maybeString1) => s ++ expr.variables
//            case _ => ???
//          }
//        case j: CfgJumpNode => {
//          j.data match {
//            case IndirectCall(variable, maybeBlock, maybeString) => s + variable
//            case _ => s
//          }
//        }
//        case _: CfgProcedureReturnNode => Set()
//        case _: CfgFunctionEntryNode  => lattice.sublattice.bottom
//        case _ => s
//      }
//    }
//}
//
//class LivenessAnalysisWorklistSolver(cfg: ProgramCfg)
//  extends LivenessAnalysis(cfg)
//    with SimpleWorklistFixpointSolver[CfgNode, Set[Variable], PowersetLattice[Variable]]
//    with IntraproceduralBackwardDependencies


abstract class LivenessAnalysis(program: Program) extends Analysis[Any]:
  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = new MapLattice(new PowersetLattice())
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] =
  {
    n match
      case sm: Statement =>
        sm match
          case LocalAssign(variable, expr, maybeString) => (s - variable) ++ expr.variables
          case MemoryAssign(memory, store, maybeString) => s ++ store.index.variables ++ store.value.variables
          case Assume(expr, maybeString, maybeString1, bool) => s ++ expr.variables
          case Assert(expr, maybeString, maybeString1) => s ++ expr.variables
          case nop: NOP => ???
          case _ => ???
      case j: Jump =>
        j match
          case call: Call =>
            call match
              case IndirectCall(variable, maybeBlock, maybeString) => s + variable
              case call: DirectCall =>
//                val a = InterProcIRCursor.succ(call)
//                val b = IntraProcIRCursor.succ(call)
                s
          case to: GoTo => s
      case _ => s

  }


class LivenessAnalysisWorklistSolver(program: Program)
  extends LivenessAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
    with IRIntraproceduralBackwardDependencies