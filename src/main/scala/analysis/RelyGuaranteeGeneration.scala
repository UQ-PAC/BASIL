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
abstract class InterferenceDomain[T, S](val stateLattice: CompatibleLattice[S], val stateTransfer: (Command, S) => S) {
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
class ConditionalWritesDomain[S](stateLattice: CompatibleLattice[S], stateTransfer: (Command, S) => S) extends InterferenceDomain[Map[Variable, S], S](stateLattice, stateTransfer) {
  // an empty map means no variables have been written to
  def bot: Map[Variable, S] = Map.empty[Variable, S]
  
  // for assignments, simply return a mapping from the assigned variable to the given state
  def derive(s: S, c: LocalAssign): Map[Variable, S] = Map(c.lhs -> s)
  
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

  def toString(t: Map[Variable, S]): String = {
    if (t.isEmpty) {
      return "forall v in Vars :: v' == v"
    }
    val sb = new StringBuilder
    val vars = t.keys.toList
    for (v <- vars) {
      sb.append(s"(~(${t(v)}) ==> ${v}' == ${v})\n&& ")
    }
    sb.append("forall v in Vars - {")
    sb.append(vars.map(_.toString).mkString(", "))
    sb.append("} :: v' == v")
    sb.toString()
  }
}

class InterferenceProductDomain[T, S](intDom: InterferenceDomain[T, S], rely: T) extends AbstractDomain[(T, S)] {
  def join(a: (T, S), b: (T, S), pos: Block): (T, S) = (intDom.join(a._1, b._1), intDom.stateLattice.lub(a._2, b._2))
  
  def transfer(a: (T, S), b: Command): (T, S) = {
    // stabilise the pre-state under the rely
    val pre_state = intDom.apply(rely, a._2)
    // derive post-state from the stabilised pre-state
    val post_state = intDom.stateTransfer(b, pre_state)
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
  
  def top: (T, S) = ???
  
  def bot: (T, S) = (intDom.bot, intDom.stateLattice.bottom)
}

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
        val guar = block_postconditions(???)._1
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
