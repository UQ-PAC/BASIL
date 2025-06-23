package analysis

import ir.{CFGPosition, Command, DirectCall, GoTo, Return, IndirectCall, Procedure, Program}

/**
  * Adapted from Tip
  * https://github.com/cs-au-dk/TIP/blob/master/src/tip/solvers/IDEAnalysis.scala 
  *
  * The special item representing the empty element in IDE.
  */
final case class Lambda()

/** Transfer functions that define IDE analysis edges.
  *
  * @tparam E
  *   Type of the function entry CFGPosition
  * @tparam EE
  *   Type of the function exit CFGPosition
  * @tparam C
  *   Type of a function call
  * @tparam R
  *   Type of a call return site
  * @tparam D
  *   the type of items
  * @tparam T
  *   type of elements of the value lattice
  * @tparam L
  *   the type of the value lattice
  */
trait IDETransferFunctions[E, EE, C, R, D, T, L <: Lattice[T]] {
  type DL = Either[D, Lambda]

  /** The value lattice.
    */
  val valuelattice: L

  /** The edge lattice.
    */
  val edgelattice: EdgeFunctionLattice[T, L]

  /** Edges for call-to-entry.
    */
  def edgesCallToEntry(call: C, entry: E)(d: DL): Map[DL, EdgeFunction[T]]

  /** Edges for exit-to-aftercall.
    */
  def edgesExitToAfterCall(exit: EE, aftercall: R)(d: DL): Map[DL, EdgeFunction[T]]

  /** Edges for call-to-aftercall.
    */
  def edgesCallToAfterCall(call: C, aftercall: R)(d: DL): Map[DL, EdgeFunction[T]]

  /** Edges for other CFG nodes.
    */
  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[T]]
}

/* Traits for Forward and Backward IDE Transfer Functions */
trait ForwardIDETransferFunctions[D, T, L <: Lattice[T]] extends IDETransferFunctions[Procedure, Return, DirectCall, Command, D, T, L]

trait BackwardIDETransferFunctions[D, T, L <: Lattice[T]] extends IDETransferFunctions[Return, Procedure, Command, DirectCall, D, T, L]


/** Base trait for IDE analyses.
  *
  * @tparam E
  *   Type of the function entry CFGPosition
  * @tparam EE
  *   Type of the function exit CFGPosition
  * @tparam C
  *   Type of a function call
  * @tparam R
  *   Type of a call return site
  * @tparam D
  *   the type of items
  * @tparam T
  *   type of elements of the value lattice
  * @tparam L
  *   the type of the value lattice
  */
trait IDEAnalysis[E, EE, C, R, D, T, L <: Lattice[T]] extends IDETransferFunctions[E, EE, C, R, D, T, L] {
  val program: Program
}

trait ForwardIDEAnalysis[D, T, L <: Lattice[T]] extends IDEAnalysis[Procedure, Return, DirectCall, Command, D, T, L]

trait BackwardIDEAnalysis[D, T, L <: Lattice[T]] extends IDEAnalysis[Return, Procedure, Command, DirectCall, D, T, L]
