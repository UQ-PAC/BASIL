package analysis

import ir.*
import ir.transforms.AbstractDomain

/**
  * State lattices must define these functions to be used in interference domains.
  */
trait CompatibleLattice[S] extends Lattice[S] {
  // weakens s by eliminating v
  def drop(v: Variable, s: S): S
  // greatest lower bound, i.e. meet
  def glb(s1: S, s2: S): S
  // display this set of states as a boogie predicate
  def toPredString(s: S): String
}

// class CompatibleLatticeMapLattice[D, L, LA <: Lattice[L]](l: LA) extends LatticeMapLattice[D, L, LA](l: LA) with CompatibleLattice[LatticeMap[D, L]] {
//   def drop(v: Variable, s: LatticeMap[D, L]): LatticeMap[D, L] = {
//     s.filter((d: D, _: L) => d != v)
//   }
// }

class IntervalLatticeExtension(l: IntervalLattice) extends LatticeMapLattice[Variable, Interval, IntervalLattice](l) with CompatibleLattice[LatticeMap[Variable, Interval]] {
  def drop(v: Variable, s: LatticeMap[Variable, Interval]): LatticeMap[Variable, Interval] =
    s + (v -> Interval.Top)

  def toPredString(s: LatticeMap[Variable, Interval]): String = SignedIntervalDomain().toPred(s).toString()
}

/**
  * An interference domain is similar to an abstract domain, except that it
  * represents sets of state transitions rather than sets of states. These
  * state transitions can represent both rely and guarantee conditions. Similar
  * to a transfer function, the interference domain is equipped with a "derive"
  * function which effectively generates a sound guarantee condition of a
  * given assignment with respect to a given pre-state. It is also equipped with
  * an "apply" function that weakens a given state by accounting for the
  * interference captured by a rely condition (i.e. an element of this domain).
  * 
  * Of course, we also need some way to represent these aforementioned
  * "pre-states" and "states". For this, we parameterise our interference
  * domain with an abstract domain, which we call a "state domain". The ability
  * to mix-and-match different interference and state domains is one of the
  * strengths of this analysis.
  * 
  * Some interference domains require that the parameterised state domain
  * defines some extra functions. Hence, we require that state domains extend
  * the CompatibleLattice trait.
  * 
  * Type parameters:
  * T: The type of lattice element representing a set of state transitions.
  * S: The type of lattice element representing a set of states.
  * L: A lattice type, defined over element type S.
  * stateTransfer: A transfer function for lattice elements of type S.
  */
abstract class InterferenceDomain[T, S](val stateLattice: CompatibleLattice[S], val stateTransfer: (S, Command) => S) {
  def bot: T
  // until the memory region analysis is complete, we test our static analysis by generating RG conditions for local variables
  def derive(s: S, c: LocalAssign): T
  def apply(t: T, s: S): S
  def join(t1: T, t2: T): T
  def close(t: T): T
  def toString(t: T): String
}

/**
  * Represents guarantee conditions of the form:
  * (!P1 ==> v1' == v1) && (!P2 ==> v2' == v2) && ...
  * 
  * This is implemented with a map [v_i -> P_i] from variables to the conditions under which they may be written to in the program.
  * Variables that are never written to are omitted from the map, rather than being mapped to bot.
  */
class ConditionalWritesDomain[S](stateLattice: CompatibleLattice[S], stateTransfer: (S, Command) => S) extends InterferenceDomain[Map[Variable, S], S](stateLattice, stateTransfer) {
  // an empty map means no variables have been written to
  def bot: Map[Variable, S] = Map.empty[Variable, S]
  
  // for assignments, simply return a mapping from the assigned variable to the given state
  def derive(s: S, c: LocalAssign): Map[Variable, S] = {
    if (s == stateLattice.bottom) {
      return Map.empty
    }
    return Map(c.lhs -> s)
  }
  
  // weaken s by eliminating each variable v that maps to a condition that overlaps with s
  def apply(t: Map[Variable, S], s: S): S = {
    var new_s = s
    t.foreach {
      case (v, cond) if stateLattice.glb(cond, s) != stateLattice.bottom =>
        new_s = stateLattice.drop(v, new_s)
      case _ =>
    }
    new_s
  }

  // the join of two conditional-writes elements is just their component-wise join
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

  def close(t: Map[Variable, S]): Map[Variable, S] = {
    // iterate 50 times to find a transitive closure, then give up and set to top
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
      sb.append(s"(~(${stateLattice.toPredString(t(v))}) ==> ${v.name}' == ${v.name})\n&& ")
    }
    sb.append("forall v in Vars - {")
    sb.append(vars.map(_.name).mkString(", "))
    sb.append("} :: v' == v")
    sb.toString()
  }
}

/**
  * To generate guarantee conditions, we need to:
  * 1. Generate the set of reachable states using the state domain.
  * 2. Generate the set of reachable state transitions by applying the "derive"
  *    function to these reachable states.
  * In our analysis, these two steps are implemented simultaneously using the
  * InterferenceProductDomain to track both the reachable states and reachable
  * state transitions at a particular program point.
  */
class InterferenceProductDomain[T, S](intDom: InterferenceDomain[T, S], rely: T) extends AbstractDomain[(T, S)] {
  override def init(b: Block): (T, S) = bot

  def top: (T, S) = ???
  
  def bot: (T, S) = (intDom.bot, intDom.stateLattice.top)

  def join(a: (T, S), b: (T, S), pos: Block): (T, S) = (intDom.join(a._1, b._1), intDom.stateLattice.lub(a._2, b._2))
  
  def transfer(a: (T, S), b: Command): (T, S) = {
    // stabilise the pre-state under the rely
    val pre_state = intDom.apply(rely, a._2)
    // derive post-state from the stabilised pre-state
    val post_state = intDom.stateTransfer(pre_state, b)
    // derive the possible state transitions resulting from this statement
    val transitions = b match {
      case a: LocalAssign => intDom.derive(pre_state, a)
      case _ => intDom.bot
    }
    // update the guarantee by joining these transitions
    val guar = intDom.join(a._1, transitions)
    // return results
    (guar, post_state)
  }
}

/**
  * This class contains the function for generating rely-guarantee conditions
  * for a given concurrent program using the given interference and state
  * domains. Currently, a thread is a represented as a Procedure, and only
  * intraprocedural analysis is supported.
  */
class RelyGuaranteeGenerator[T, S](intDom: InterferenceDomain[T, S], threads: List[Procedure]) {
  def generate(): Map[Procedure, (T, T)] = {
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
        val productDom = InterferenceProductDomain[T, S](intDom, rely)
        val solver = transforms.worklistSolver(productDom)
        val (_, block_postconditions) = solver.solveProc(p)
        val guar = block_postconditions(p.returnBlock.get)._1
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
