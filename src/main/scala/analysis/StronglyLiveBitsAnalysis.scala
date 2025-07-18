package analysis

import analysis.solvers.BackwardIDESolver
import ir.*

/**
 * 
 */


/**
 * 
 * LiveBits Analysis
 * 
 * 
 */

trait StronglyLiveBitsAnalysisFunctions()
    extends BackwardIDEAnalysis[Variable, FlatElement[BigInt], IntLattice] {
  val valuelattice: IntLattice = IntLattice(minimum = FlatEl(0))

  /** The edge lattice.
   */
  val edgelattice: EdgeFunctionLattice[FlatElement[BigInt], IntLattice] = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}
  
  /** Edges for call-to-entry.
   */
  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[FlatElement[BigInt]]] = ???

  /** Edges for exit-to-aftercall.
   */
  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[FlatElement[BigInt]]] = ???

  /** Edges for call-to-aftercall.
   */
  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[FlatElement[BigInt]]] = ???
  
  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[FlatElement[BigInt]]] = {
    n match {
      case LocalAssign(variable: Variable, expr: Expr, _) => // (s - variable) ++ for each variable in expr.variables
        // if variable is mapped to top, then 
        d match {
          case Left(value) => 
            if value == variable then ??? //remove variable from the set -- optimize later?
            else Map(d -> IdEdge())
          case Right(_) => ??? // Check if the variable is in the set. 
        }
      case MemoryLoad(lhs, _, index, _, _, _) => ???
      case MemoryStore(_, index, value, _, _, _) => ???
      case Assume(expr, _, _, _) => ???
      case Assert(expr, _, _) => ???
      case IndirectCall(variable, _) => ???
      // case c: DirectCall if addExternals && (c.target.isExternal.contains(true) || c.target.blocks.isEmpty) => ???
      case c: DirectCall => ???
    }
  }
} 
  
// TODO: figure out the lattice
// class StronglyLiveBitsAnalysis(program: Program) 
//   extends BackwardIDESolver[Variable, FlatElement[BigInt], IntLattice](program),
//     StronglyLiveBitsAnalysisFunctions()

class StronglyLiveBitsAnalysis(program: Program)
  extends BackwardIDESolver[Variable, FlatElement[BigInt], IntLattice](program),
    StronglyLiveBitsAnalysisFunctions() {
  /** The edge lattice.
   */
}  
