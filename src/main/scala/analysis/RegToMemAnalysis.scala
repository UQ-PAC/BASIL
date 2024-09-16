package analysis

import ir.{MemoryLoad, *}
import analysis.solvers.*
import util.Logger

import scala.collection.immutable

/**
 * Collects all the memory loads and the expressions that are assigned to a register but cannot be evaluated.
 *
 * Tracks:
 * R_x = MemoryLoad[Base + Offset]
 * R_x = Base + Offset
 *
 * Both in which constant propagation mark as TOP which is not useful.
 */
trait RegionAccessesAnalysis(program: Program, constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]], reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) {

  val mapLattice: MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]] = MapLattice(FlatLattice[_root_.ir.Expr]())

  val lattice: MapLattice[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] = MapLattice(mapLattice)

  val domain: Set[CFGPosition] = program.toSet 

  val first: Set[CFGPosition] = program.procedures.toSet

  /** Default implementation of eval.
   */
  def eval(cmd: Statement, constants: Map[Variable, FlatElement[BitVecLiteral]], s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = {
    cmd match {
      case assign: Assign =>
        assign.rhs match {
          case memoryLoad: MemoryLoad =>
            s + (RegisterVariableWrapper(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs)) -> FlatEl(memoryLoad))
          case binaryExpr: BinaryExpr =>
            if (evaluateExpression(binaryExpr.arg1, constants).isEmpty) { // approximates Base + Offset
              Logger.debug(s"Approximating $assign in $binaryExpr")
              Logger.debug(s"Reaching defs: ${reachingDefs(cmd)}")
              s + (RegisterVariableWrapper(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs)) -> FlatEl(binaryExpr))
            } else {
              s
            }
          case _ => s
        }
      case _ =>
        s
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = n match {
    case cmd: Statement =>
      eval(cmd, constantProp(cmd), s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CFGPosition, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = localTransfer(n, s)
}

class RegionAccessesAnalysisSolver(
                         program: Program,
                         constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                         reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                       ) extends RegionAccessesAnalysis(program, constantProp, reachingDefs)
  with IRInterproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] {
}
