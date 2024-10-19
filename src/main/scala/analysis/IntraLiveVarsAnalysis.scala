package analysis

import analysis.solvers.SimpleWorklistFixpointSolver
import ir.{Assert, Assume, Block, CFGPosition, Call, DirectCall, GoTo, IndirectCall, Jump, Assign, MemoryAssign, NOP, Procedure, Program, Statement, Variable, Return, Unreachable}

abstract class LivenessAnalysis(program: Program) extends Analysis[Any]:
  val lattice: MapLattice[CFGPosition, Set[Variable], PowersetLattice[Variable]] = MapLattice(PowersetLattice())
  val domain: Set[CFGPosition] = Set.empty ++ program

  def transfer(n: CFGPosition, s: Set[Variable]): Set[Variable] = {
    n match {
      case p: Procedure => s
      case b: Block => s
      case Assign(variable, expr, _) => (s - variable) ++ expr.variables
      case MemoryAssign(_, index, value, _, _, _) => s ++ index.variables ++ value.variables
      case Assume(expr, _, _, _) => s ++ expr.variables
      case Assert(expr, _, _) => s ++ expr.variables
      case IndirectCall(variable, _) => s + variable
      case c: DirectCall => s -- c.outParams.map(_._2) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo => s
      case r: Return => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable => s
      case _ => ???
    }
  }

class IntraLiveVarsAnalysis(program: Program)
  extends LivenessAnalysis(program)
    with SimpleWorklistFixpointSolver[CFGPosition, Set[Variable], PowersetLattice[Variable]]
    with IRIntraproceduralBackwardDependencies
