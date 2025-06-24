package analysis

import util.StaticAnalysisLogger as Logger

import ir.transforms.worklistSolver
import ir.*

/** Performs a single analysis to generate loop invariants.
 */
class SingleLoopInvariantGenerator[L](domain: PredicateEncodingDomain[L], backwards: Boolean) {
  def generateInvariants(procedure: Procedure): Map[Block, Predicate] = {
    val (startResults, endResults) = worklistSolver(domain).solveProc(procedure, backwards)

    Logger.debug(endResults)

    (backwards match {
      case false => endResults
      case true => startResults
    }).flatMap((block, l) => if block.isLoopHeader() then Some((block, domain.toPred(l))) else None)
  }
}

class ForwardLoopInvariantGenerator[L](domain: PredicateEncodingDomain[L]) extends SingleLoopInvariantGenerator[L](domain, false)
class BackwardLoopInvariantGenerator[L](domain: PredicateEncodingDomain[L]) extends SingleLoopInvariantGenerator[L](domain, true)

/** Runs a collection of analyses to produce loop invariants, then inserts them into the IR.
 */
class FullLoopInvariantGenerator(program: Program) {
  def addInvariants() = {
    program.procedures.foreach(addInvariantsToProc(_))
  }

  def addInvariantsToProc(procedure: Procedure) = {
    Logger.debug(procedure)
    val intervalInvariantGenerator = ForwardLoopInvariantGenerator(DoubleIntervalDomain(Some(procedure)))
    intervalInvariantGenerator.generateInvariants(procedure).foreach {
      (block, pred) => {
        Logger.debug(block)
        Logger.debug(pred)
        block.postconditions += pred
      }
    }
  }
}
