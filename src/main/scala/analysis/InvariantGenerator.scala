package analysis

import ir.*
import ir.transforms.{IntraproceduralWorklistSolver, NarrowingWorklistSolver, reversePostOrder}
import util.StaticAnalysisLogger as Logger

/** Performs a single analysis to generate loop invariants.
 */
class SingleLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A],
  backwards: Boolean
) {
  def generateInvariants(procedure: Procedure): Map[Block, Predicate] = {
    val (startResults, endResults) = solver.solveProc(procedure, backwards)

    Logger.debug(endResults)

    (backwards match {
      case false => endResults
      case true => startResults
    }).flatMap((block, l) => if block.isLoopHeader() then Some((block, domain.toPred(l))) else None)
  }
}

class ForwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, false)

class BackwardLoopInvariantGenerator[L, A <: PredicateEncodingDomain[L]](
  domain: A,
  solver: IntraproceduralWorklistSolver[L, A]
) extends SingleLoopInvariantGenerator(domain, solver, true)

/** Runs a collection of analyses to produce loop invariants, then inserts them into the IR.
 */
class FullLoopInvariantGenerator(program: Program) {
  def addInvariants() = {
    program.procedures.foreach(addInvariantsToProc(_))
  }

  def addInvariantsToProc(procedure: Procedure) = {
    Logger.debug(procedure)
    reversePostOrder(procedure)
    val intervalDomain = DoubleIntervalDomain(Some(procedure))
    val intervalInvariantGenerator =
      ForwardLoopInvariantGenerator(intervalDomain, NarrowingWorklistSolver(intervalDomain))
    intervalInvariantGenerator.generateInvariants(procedure).foreach { (block, pred) =>
      {
        Logger.debug(block)
        Logger.debug(pred)
        block.postconditions += pred
      }
    }
  }
}
