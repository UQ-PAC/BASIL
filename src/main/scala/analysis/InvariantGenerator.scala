package analysis

import ir.*
import ir.transforms.{IntraproceduralWorklistSolver, NarrowingWorklistSolver, reversePostOrder, worklistSolver}

/** Performs a single analysis to generate loop invariants.
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

class ForwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, false) {
  def genAndAddInvariants(procedure: Procedure) =
    generateInvariants(procedure).foreach { (block, pred) => block.postconditions += pred }
}

class BackwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, true) {
  def genAndAddInvariants(procedure: Procedure) =
    generateInvariants(procedure).foreach { (block, pred) => block.preconditions += pred }
}

/** Runs a collection of analyses to produce loop invariants, then inserts them into the IR.
 */
class FullLoopInvariantGenerator(program: Program) {
  def addInvariants() = {
    program.procedures.foreach(addInvariantsToProc(_))
  }

  def addInvariantsToProc(procedure: Procedure) = {
    reversePostOrder(procedure)

    val intervals = DoubleIntervalDomain(Some(procedure))
    ForwardLoopInvariantGenerator(intervals, NarrowingWorklistSolver(intervals)).genAndAddInvariants(procedure)

    val gammas = MayGammaDomain(
      LatticeMap.TopMap(procedure.formalInParam.unsorted.map(v => (v, LatticeSet.FiniteSet(Set(v)))).toMap)
    )
    ForwardLoopInvariantGenerator(gammas, worklistSolver(gammas)).genAndAddInvariants(procedure)

    // Add more domains here
  }
}
