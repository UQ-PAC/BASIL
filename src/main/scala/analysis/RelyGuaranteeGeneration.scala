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
}

/**
  * Whereas an abstract domain is used for deriving sets of states in the form
  * of some lattice, an interference domain is used for deriving sets of state
  * _transitions_, also in the form of some lattice. As sets of state
  * transitions, they can represent both rely and guarantee conditions.
  * 
  * Interference domains are parameterised by some abstract domain A, and
  * provide functionality for deriving themselves (i.e. as guarantees) from
  * elements of A, as well as stabilising elements of A
  * under themselves (i.e. as rely conditions).
  * 
  * Type parameters:
  * T: The type of lattice element representing a set of state transitions.
  * S: The type of lattice element representing a set of states.
  * L: A lattice type, defined over element type S.
  * stateTransfer: A transfer function for lattice elements of type S.
  */
abstract class InterferenceDomain[T, S](stateLattice: CompatibleLattice[S], stateTransfer: S => Command => S) {
  def bot: T
  def derive(s: S, c: Command): T
  def apply(t: T, s: S): S
  def join(t1: T, t2: T): T
  def close(t: T): T
}

/**
  * Represents guarantee conditions of the form:
  * (!P1 ==> v1' == v1) && (!P2 ==> v2' == v2) && ...
  * 
  * This is implemented with a map [v_i -> P_i] from variables to the conditions under which they may be written to in the program.
  * Variables that are never written to are omitted from the map, rather than being mapped to bot.
  */
class ConditionalWritesDomain[S](stateLattice: CompatibleLattice[S], stateTransfer: S => Command => S) extends InterferenceDomain[Map[Variable, S], S](stateLattice, stateTransfer) {
  // an empty map means no variables have been written to
  def bot: Map[Variable, S] = Map.empty[Variable, S]
  
  // for assignments, simply return a mapping from the assigned variable to the given state
  def derive(s: S, c: Command): Map[Variable, S] = c match {
    case LocalAssign(lhs, _, _) => Map(lhs -> s)
    case _ => Map.empty[Variable, S]
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
          new_t = new_t + (v1 -> stateLattice.lub(new_t(v1), to_join))
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
}

class InterferenceProductDomain[T, S](intDom: InterferenceDomain[T, S]/*, stateDom: AbstractDomain[S] */) extends AbstractDomain[(T, S)](rely: T) {
  def join(a: (T, S), b: (T, S), pos: Block): (T, S) = (intDom.join(a._1, b._1), stateLattice.join(a._2, b._2))
  
  def transfer(a: (T, S), b: Command): (T, S) = {
    // stabilise the pre-state under the rely
    var pre_state = intDom.apply(rely, a._2)
    // derive post-state from the stabilised pre-state
    var post_state = stateTransfer(pre_state, b)
    // derive the new guarantee
    var guar = intDom.join(a._1, intDom.derive(pre_state, b))
    // return results
    (guar, post_state)
  }
  
  def top: (T, S) = ???
  
  def bot: (T, S) = (intDom.bot, stateDom.bot)
}
