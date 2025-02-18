/** Todo:
  * - Use function summaries developed by Ben to get more precise results for
  *   get_two.
  * - Use Ben's disjunctive completion domain to get accurate results for a
  *   bigger example.
  * - Enquire about going to top for mem calls - this seems stifling.
  */

package analysis

import ir.*
import ir.transforms.*
import analysis.*
import scala.collection.mutable.Queue

/** An interference domain is similar to an abstract domain, except that it
  * represents sets of state transitions rather than sets of states. These sets
  * of state transitions can represent both rely and guarantee conditions.
  * Similar to a transfer function, interference domains are equipped with a
  * "derive" function which effectively generates a sound guarantee condition of
  * a given assignment with respect to a given pre-state. They are also equipped
  * with an "apply" function that weakens a given state by accounting for the
  * interference captured by a rely condition.
  * 
  * Of course, we also need some way to represent these aforementioned
  * "states". For this, interference domains are parameterised with an abstract
  * domain, which we call a "state domain". This gives us the ability to
  * mix-and-match different interference and state domains.
  * 
  * Type parameters:
  * T: The type of lattice element representing a set of state transitions.
  * S: The type of lattice element representing a set of states.
  * 
  * @param stateLattice: A lattice providing operations over S.
  * @param stateTransfer: A transfer function for deriving reachable states.
  */
abstract class InterferenceDomain[T, S](
  val stateLattice: InterferenceCompatibleLattice[S],
  val stateTransfer: (S, Command) => S
) {
  def bot: T
  def derive(s: S, c: Command): T
  def apply(t: T, s: S): S
  def join(t1: T, t2: T): T
  def close(t: T): T
  def toString(t: T): String
}

/** The conditional-writes domain is an interference domain that represents
  * guarantee conditions of the form:
  * (!P_1 ==> v_1' == v_1) && (!P_2 ==> v_2' == v_2) && ...
  * 
  * This is implemented with a map [v_i -> P_i] from variables to an
  * overapproximation of the set of states under which they may be written to in
  * the thread. Variables that are never written to are omitted from the map,
  * rather than being mapped to bot.
  */
case class ConditionalWritesDomain[S](
  override val stateLattice: InterferenceCompatibleLattice[S], 
  override val stateTransfer: (S, Command) => S
) extends InterferenceDomain[Map[Variable, S], S](stateLattice, stateTransfer) {
  
  // an empty map means no variables have been written to
  def bot: Map[Variable, S] = Map.empty[Variable, S]
  
  // simply map all assigned variables to the precondition
  def derive(s: S, c: Command): Map[Variable, S] = 
    if (s == stateLattice.bottom) Map.empty
    else c match {
      case a: Assign => a.assignees.map(v => v -> s).toMap
      case _ => bot
    }
  
  /* If a variable v maps to a set of states that intersects s, then there
  exists a state satisfying s under which v can be written. Thus, we stabilise s
  by eliminating v. */
  def apply(t: Map[Variable, S], s: S): S = {
    var new_s = s
    t.foreach {
      case (v, cond) if stateLattice.glb(cond, s) != stateLattice.bottom =>
        new_s = stateLattice.drop(v, new_s)
      case _ =>
    }
    new_s
  }

  // the join of two elements is just the component-wise join over the map
  def join(t1: Map[Variable, S], t2: Map[Variable, S]): Map[Variable, S] = {
    var new_t = t1
    t2.foreach {
      case (v, cond) if new_t.contains(v) =>
        new_t = new_t + (v -> stateLattice.lub(new_t(v), cond))
      case (v, cond) =>
        new_t = new_t + (v -> cond)
    }
    new_t
  }

  // the transitive closure algorithm is best described in the white-paper
  def close(t: Map[Variable, S]): Map[Variable, S] = {
    // iterate 50 times to find a closure, then give up and go to top
    val max_iterations = 50
    var count = 0
    var old_t, new_t = t
    var closed = false
    while (!closed) {
      for ((v1, cond1) <- old_t) {
        for ((v2, cond2) <- old_t) {
          val drop_v2 = stateLattice.drop(v2, old_t(v1))
          val to_join = stateLattice.glb(old_t(v2), drop_v2)
          val new_cond = stateLattice.lub(new_t(v1), to_join)
          if (new_cond == stateLattice.bottom) {
            new_t = new_t.removed(v1)
          } else {
            new_t = new_t + (v1 -> new_cond)
          }
        }
      }
      count = count + 1
      if (new_t == old_t || count == max_iterations) {
        closed = true
      }
      old_t = new_t
    }
    new_t
  }

  def toString(t: Map[Variable, S]): String = {
    if (t.isEmpty) {
      return "forall v in Vars :: v' == v"
    }
    val sb = new StringBuilder
    val vars = t.keys.toList
    for (v <- vars) {
      val condStr = stateLattice.toPredString(t(v))
      sb.append(s"(~(${condStr}) ==> ${v.name}' == ${v.name})\n&& ")
    }
    sb.append("forall v in Vars - {")
    sb.append(vars.map(_.name).mkString(", "))
    sb.append("} :: v' == v")
    sb.toString()
  }
}

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
class InterferenceProductDomain[T, S](intDom: InterferenceDomain[T, S], rely: T)
    extends AbstractDomain[(T, S)] {

  

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
    val transitions = intDom.derive(pre_state, b)
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

  def localTransferCall(localState: (T, S), summaryForTarget: (T, S),
      call: DirectCall) : (T, S) =
    // the postcondition of a procedure call is simply the procedure's summary
    summaryForTarget

  def updateSummary(prevSummary: (T, S), p: Procedure,
      resBefore: Map[Block, (T, S)], resAfter: Map[Block, (T, S)]) : (T, S) =
    // we want to expand the previous postcondition by joining this one
    dom.pureJoin(prevSummary, resAfter(p.returnBlock.get))
}
