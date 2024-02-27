package analysis

import ir.{MemoryLoad, *}
import analysis.solvers.*

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
trait RegionAccessesAnalysis(prog: Program, constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]]) {

  val mapLattice: MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]] = MapLattice(FlatLattice[_root_.ir.Expr]())

  val lattice: MapLattice[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] = MapLattice(mapLattice)

  val domain: Set[CFGPosition] = computeDomain(IntraProcIRCursor, prog.procedures).toSet

  /** Default implementation of eval.
   */
  def eval(cmd: Command, constants:  Map[Variable, FlatElement[BitVecLiteral]], s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = {
    cmd match {
      case localAssign: LocalAssign =>
        localAssign.rhs match {
          case memoryLoad: MemoryLoad =>
            s + (RegisterVariableWrapper(localAssign.lhs) -> FlatEl(memoryLoad).asInstanceOf[FlatElement[Expr]])
          case binaryExpr: BinaryExpr =>
            if (evaluateExpression(binaryExpr.arg1, constants).isEmpty) { // approximates Base + Offset
              return s + (RegisterVariableWrapper(localAssign.lhs) -> FlatEl(binaryExpr).asInstanceOf[FlatElement[Expr]])
            }
            s
          case _ => s
        }
      case _ =>
        s
    }
  }

  /** Transfer function for state lattice elements.
   */
  def localTransfer(n: CFGPosition, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = n match {
    case cmd: Command =>
      eval(cmd, constantProp(n), s)
    case _ => s // ignore other kinds of nodes
  }

  /** Transfer function for state lattice elements.
   */
  def transfer(n: CFGPosition, s: Map[RegisterVariableWrapper, FlatElement[Expr]]): Map[RegisterVariableWrapper, FlatElement[Expr]] = localTransfer(n, s)
}

class RegionAccessesAnalysisSolver(
                         prog: Program,
                         constantProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
                       ) extends RegionAccessesAnalysis(prog, constantProp)
  with IRInterproceduralForwardDependencies
  with Analysis[Map[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]]]]
  with SimpleWorklistFixpointSolver[CFGPosition, Map[RegisterVariableWrapper, FlatElement[Expr]], MapLattice[RegisterVariableWrapper, FlatElement[Expr], FlatLattice[Expr]]] {
}