package analysis

import ir.*
import ir.transforms.{IntraproceduralWorklistSolver, NarrowingWorklistSolver, reversePostOrder}

// TODO allow for interprocedural analysis

/** Performs a single analysis to generate loop invariants. Loop invariants are encoded as assertions, and will only be
 *  generated on loop heads. This means that loop head detection must be run. See `ForwardLoopInvariantGenerator` and
 *  `BackwardLoopInvariantGenerator`.
 *
 * @tparam L
 *   : Domain value type
 * @tparam A
 *   : The type of abstract domain being used
 *
 * @param domain
 *   : The concrete abstract domain being used
 * @param solver
 *   : A type of worklist solver which may or may not support widening or narrowing
 * @param backwards
 *   : Distinguishes between backwards and forwards analyses, see `ForwardLoopInvariantGenerator` and
 *     `BackwardLoopInvariantGenerator`.
 */
class SingleLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A],
  backwards: Boolean
) {
  def generateInvariants(procedure: Procedure): Map[Block, Predicate] = {
    val (startResults, endResults) = solver.solveProc(procedure, backwards)

    (backwards match {
      case false => endResults
      case true => startResults
    }).flatMap((block, l) => if block.isLoopHeader() then Some((block, domain.toPred(l))) else None)
  }
}

/** Performs a single forwards analysis to generate loop invariants that hold at the end of a block. This will be
 *  encoded as an assertion at the end of a block. Invariants will only be generated on loop heads, and so loop head
 *  detection must be run.
 *
 * @tparam L
 *   : Domain value type
 * @tparam A
 *   : The type of abstract domain being used
 *
 * @param domain
 *   : The concrete abstract domain being used
 * @param solver
 *   : A type of worklist solver which may or may not support widening or narrowing
 */
class ForwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, false) {
  def genAndAddInvariants(procedure: Procedure) =
    generateInvariants(procedure).foreach { (block, pred) =>
      if pred != Predicate.True then block.postconditions += pred
    }
}

/** Performs a single backwards analysis to generate loop invariants that hold at the start of a block. This will be
 *  encoded as an assertion at the start of a block. Invariants will only be generated on loop heads, and so loop head
 *  detection must be run.
 *
 * @tparam L
 *   : Domain value type
 * @tparam A
 *   : The type of abstract domain being used
 *
 * @param domain
 *   : The concrete abstract domain being used
 * @param solver
 *   : A type of worklist solver which may or may not support widening or narrowing
 */
class BackwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, true) {
  def genAndAddInvariants(procedure: Procedure) =
    generateInvariants(procedure).foreach { (block, pred) =>
      if pred != Predicate.True then block.preconditions += pred
    }
}

/** Runs a collection of analyses to produce loop invariants, then inserts them into the IR.
 */
class FullLoopInvariantGenerator(program: Program) {
  def addInvariants() = {
    program.procedures.foreach(addInvariantsToProc(_))
  }

  def addInvariantsToProc(procedure: Procedure) = {
    reversePostOrder(procedure)

    // Intervals give us constant bounds on variables, for example an iteration variable in a loop guard.
    val intervals = DoubleIntervalDomain(Some(procedure))
    ForwardLoopInvariantGenerator(intervals, NarrowingWorklistSolver(intervals)).genAndAddInvariants(procedure)

    // Gamma domains ensure that relations of the gammas of variables are maintained throughout loop iterations.
    // Currently broken! The gamma domains don't produce predicates correctly it seems
    // val gammaMap =
    // LatticeMap.BottomMap(procedure.formalInParam.unsorted.map((v: Variable) => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    // val mayGammas = MayGammaDomain(gammaMap)
    // val mustGammas = MustGammaDomain(gammaMap)
    // ForwardLoopInvariantGenerator(mayGammas, worklistSolver(mayGammas)).genAndAddInvariants(procedure)
    // ForwardLoopInvariantGenerator(mustGammas, worklistSolver(mustGammas)).genAndAddInvariants(procedure)

    // Add more domains here
  }
}
