package analysis

import ir.*
import ir.transforms.*
import util.StaticAnalysisLogger

import scala.collection.mutable.Queue

/** To generate guarantee conditions, we need to:
  * 1. Generate the set of reachable states using the state domain.
  * 2. Generate the set of reachable state transitions by applying the "derive"
  *    function to these reachable states.
  * In our analysis, these two steps are implemented simultaneously using the
  * InterferenceProductDomain to track both the reachable states and reachable
  * state transitions at a particular program point.
  * 
  * @param intDom: Instance of the interference domain, to provide operations.
  * @param rely: The rely condition under which we stabilise generated states.
  */
class InterferenceProductDomain[T, S](intDom: InterferenceDomain[T, S], rely: T) extends AbstractDomain[(T, S)] {

  /* We initialise the interference domain to bottom, meaning we have
  encountered no reachable state transitions. However, we initialise the state
  domain to top, meaning the program may exist in any state at this position. */
  override def init(b: Block): (T, S) = (intDom.bot, intDom.stateLattice.top)

  // this is undefined because top is undefined for InterferenceDomains
  def top: (T, S) = ???

  // this is a bit of a sketchy definition
  def bot: (T, S) = (intDom.bot, intDom.stateLattice.bottom)

  // simply the component-wise join
  def join(a: (T, S), b: (T, S), pos: Block): (T, S) = pureJoin(a, b)

  // join without the position argument
  def pureJoin(a: (T, S), b: (T, S)): (T, S) =
    (intDom.join(a._1, b._1), intDom.stateLattice.lub(a._2, b._2))

  // updates and stabilises the state while collecting reachable transitions
  def transfer(a: (T, S), b: Command): (T, S) = {
    // stabilise the pre-state under the rely
    val pre_state = intDom.apply(rely, a._2)
    // derive post-state from the stabilised pre-state
    val post_state = intDom.stateTransfer(pre_state, b)
    // derive the possible state transitions resulting from this statement
    // todo: for now, we only consider "interference" from LocalAssigns
    val transitions = b match {
      case a: Assign => intDom.derive(pre_state, a)
      case _ => intDom.bot
    }
    // update the guarantee by joining these transitions
    val guar = intDom.join(a._1, transitions)
    // return results
    (guar, post_state)
  }
}

/** This class contains the function for generating rely-guarantee conditions
  * for a given concurrent program using the given interference and state
  * domains. Currently, a thread is a represented as a Procedure, and only
  * intraprocedural analysis is supported.
  */
class RelyGuaranteeGenerator[T, S](intDom: InterferenceDomain[T, S]) {

  private def getReachableProcedures(proc: Procedure): Set[Procedure] = {
    var reachable = Set[Procedure]()
    var toAdd = Queue[Procedure](proc)
    while (!toAdd.isEmpty) {
      val p = toAdd.dequeue
      reachable = reachable + p
      for (callee <- p.calls) {
        if (!reachable.contains(callee)) {
          toAdd.enqueue(callee)
        }
      }
    }
    reachable
  }

  def generate(threads: List[Procedure]): Map[Procedure, (T, T)] = {
    val reachableProcedures: Map[Procedure, Set[Procedure]] =
      threads.map(p => p -> getReachableProcedures(p)).toMap
    var old_guars, new_guars = threads.map(_ -> intDom.bot).toMap
    var fixpoint_reached = false
    val max_iterations = 50
    var iterations = 0
    while (!fixpoint_reached && iterations < max_iterations) {
      for (p <- threads) {
        // construct rely for p
        var rely = intDom.bot
        for (other_p <- threads) {
          if (p != other_p) {
            rely = intDom.join(rely, new_guars(other_p))
          }
        }
        // fixme: derive transitive closure for this rely???
        // generate new guar for p
        val localDom = InterferenceProductDomain[T, S](intDom, rely)
        val summaryGenerator = GuarGenSummaryGenerator[T, S](localDom)
        val solver = interprocSummaryFixpointSolver(localDom, summaryGenerator)
        val (guar, _) = solver.solveProcsInterProc(reachableProcedures(p))(p)
        new_guars = new_guars + (p -> guar)
      }
      // check if fixpoint is reached
      fixpoint_reached = true
      for (p <- threads) {
        if (old_guars(p) != new_guars(p)) {
          fixpoint_reached = false
        }
      }
      iterations = iterations + 1
      old_guars = new_guars
    }
    var rg_conditions = Map.empty[Procedure, (T, T)]
    for (p <- threads) {
      var rely = intDom.bot
      for (other_p <- threads) {
        if (p != other_p) {
          rely = intDom.join(rely, new_guars(other_p))
        }
      }
      rg_conditions = rg_conditions + (p -> (rely, new_guars(p)))
    }
    rg_conditions
  }
}

class GuarGenSummaryGenerator[T, S](dom: InterferenceProductDomain[T, S])
    extends ProcedureSummaryGenerator[(T, S), (T, S)] {

  def bot: (T, S) = dom.bot

  def top: (T, S) = ???

  def join(a: (T, S), b: (T, S), pos: Procedure): (T, S) = dom.pureJoin(a, b)

  def transfer(a: (T, S), b: Procedure): (T, S) = ???

  def localTransferCall(localState: (T, S), summaryForTarget: (T, S), call: DirectCall): (T, S) =
    // the postcondition of a procedure call is simply the procedure's summary
    summaryForTarget

  def updateSummary(
    prevSummary: (T, S),
    p: Procedure,
    resBefore: Map[Block, (T, S)],
    resAfter: Map[Block, (T, S)]
  ): (T, S) =
    // we want to expand the previous postcondition by joining this one
    dom.pureJoin(prevSummary, resAfter(p.returnBlock.get))
}

def getGenerateRgConditionsTransform(threads: List[Procedure]): Transform =
  Transform(
    "GenerateRgConditions",
    (ctx, man) => {
      type StateLatticeElement = LatticeMap[Variable, analysis.Interval]
      type InterferenceLatticeElement = Map[Variable, StateLatticeElement]
      val stateLattice = IntervalLatticeExtension()
      val stateTransfer = SignedIntervalDomain().transfer
      val intDom = ConditionalWritesDomain[StateLatticeElement](stateLattice, stateTransfer)
      val relyGuarantees =
        RelyGuaranteeGenerator[InterferenceLatticeElement, StateLatticeElement](intDom).generate(threads)
      // fixme: these should not be printed to stdout
      for ((p, (rely, guar)) <- relyGuarantees) {
        StaticAnalysisLogger.info("--- " + p.procName + " " + "-" * 50 + "\n")
        StaticAnalysisLogger.info("Rely:")
        StaticAnalysisLogger.info(intDom.toString(rely) + "\n")
        StaticAnalysisLogger.info("Guarantee:")
        StaticAnalysisLogger.info(intDom.toString(guar) + "\n")
      }
      man.ClobberAll
    },
    notice = "Generating Rely-Guarantee Conditions"
  )
