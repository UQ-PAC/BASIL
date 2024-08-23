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

  val mapLattice: MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]] = MapLattice(FlatLattice[_root_.ir.Expr]())

  val lattice: MapLattice[CFGPosition, Map[RegisterWrapperPartialEquality, FlatElement[Expr]], MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]]] = MapLattice(mapLattice)

  val domain: Set[CFGPosition] = Set.empty ++ program

  val first: Set[CFGPosition] = Set.empty ++ program.procedures

  /** Default implementation of eval.
   */
  def eval(cmd: Command, constants: Map[Variable, FlatElement[BitVecLiteral]], s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = {
    cmd match {
      case assign: Assign =>
        assign.rhs match {
          case memoryLoad: MemoryLoad =>
            s + (RegisterWrapperPartialEquality(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs)) -> FlatEl(memoryLoad))
          case binaryExpr: BinaryExpr =>
            if (evaluateExpression(binaryExpr.arg1, constants).isEmpty) { // approximates Base + Offset
              s + (RegisterWrapperPartialEquality(assign.lhs, getDefinition(assign.lhs, cmd, reachingDefs)) -> FlatEl(binaryExpr))
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
  def localTransfer(n: CFGPosition, s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = n match {
    case cmd: Command =>
      eval(cmd, constantProp(cmd), s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CFGPosition, s: Map[RegisterWrapperPartialEquality, FlatElement[Expr]]): Map[RegisterWrapperPartialEquality, FlatElement[Expr]] = localTransfer(n, s)
}

class RegionAccessesAnalysisSolver(
                         program: Program,
                         constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                         reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])],
                       ) extends RegionAccessesAnalysis(program, constantProp, reachingDefs)
  with IRInterproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Map[RegisterWrapperPartialEquality, FlatElement[Expr]]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Map[RegisterWrapperPartialEquality, FlatElement[Expr]], MapLattice[RegisterWrapperPartialEquality, FlatElement[Expr], FlatLattice[Expr]]] {
}