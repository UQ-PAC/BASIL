package analysis

import ir.{CFGPosition, Command, DirectCall, GoTo, Return, Procedure, Program}

final case class Lambda()

/**
 * Base trait for IDE analyses.
 *
 * @tparam E Type of the function entry CFGPosition
 * @tparam EE Type of the function exit CFGPosition
 * @tparam C Type of a function call
 * @tparam R Type of a call return site
 * @tparam D the type of items
 * @tparam T type of elements of the value lattice
 * @tparam L the type of the value lattice
 * Adapted from Tip
 * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDEAnalysis.scala
 * The special item representing the empty element in IDE.
 */
trait IDEAnalysis[E, EE, C, R, D, T, L <: Lattice[T]] {
  val program: Program

  type DL = Either[D, Lambda]

  /**
   * The value lattice.
   */
  val valuelattice: L

  /**
   * The edge lattice.
   */
  val edgelattice: EdgeFunctionLattice[T, L]

  /**
   * Edges for call-to-entry.
   */
  def edgesCallToEntry(call: C, entry: E)(d: DL): Map[DL, EdgeFunction[T]]

  /**
   * Edges for exit-to-aftercall.
   */
  def edgesExitToAfterCall(exit: EE, aftercall: R)(d: DL): Map[DL, EdgeFunction[T]]

  /**
   * Edges for call-to-aftercall.
   */
  def edgesCallToAfterCall(call: C, aftercall: R)(d: DL): Map[DL, EdgeFunction[T]]

  /**
   * Edges for other CFG nodes.
   */
  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[T]]
}

// IndirectCall in these is because they are returns so that can be further tightened in future
trait ForwardIDEAnalysis[D, T, L <: Lattice[T]] extends IDEAnalysis[Procedure, Return, DirectCall, GoTo, D, T, L]

trait BackwardIDEAnalysis[D, T, L <: Lattice[T]] extends IDEAnalysis[Return, Procedure, GoTo, DirectCall, D, T, L]
