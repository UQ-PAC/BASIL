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
 trait InterferenceDomain[T, S](
  val stateLattice: InterferenceCompatibleLattice[S],
  val stateTransfer: (S, Command) => S
) {
  // the empty set of state transitions
  def bot: T
  // the state transitions that may occur by executing c in a state satisfying s
  // todo: until memory regions are complete, we test RG gen on local variables
  def derive(s: S, c: Assign): T
  // a weakened version of s that accounts for all transitions in t
  def apply(t: T, s: S): S
  // an overapproximation of the union of transitions captured by t1 and t2
  def join(t1: T, t2: T): T
  // an overapproximation of the transitive closure of t
  def close(t: T): T
  // representation of t as a predicate over primed and unprimed variables
  def toString(t: T): String
}

/* Fixme: This may result in naming clashes, however we don't always have a
Procedure at hand to call getFreshSSAVar(...). Ideally, we might pass around
some kind of variable factory. */
def get_temp(ty: IRType): LocalVar = LocalVar("rg_tmp_var", ty, 0)

/** The conditional-writes domain is an interference domain that represents
  * guarantee conditions of the form:
  * (!P_1 ==> v_1' == v_1) && (!P_2 ==> v_2' == v_2) && ...
  * where P_i is an element of our state domain and v_i is a variable.
  * 
  * This is implemented with a map [v_i -> P_i]. Variables that are never
  * written to are omitted from the map, rather than being mapped to bot.
  */
class ConditionalWritesDomain[S](
  stateLattice: InterferenceCompatibleLattice[S],
  stateTransfer: (S, Command) => S
) extends InterferenceDomain[Map[Variable, S], S](stateLattice, stateTransfer) {
  // an empty map means no variables have been written to
  def bot: Map[Variable, S] = Map.empty[Variable, S]

  // simply map the assigned variable to the precondition
  def derive(s: S, c: Assign): Map[Variable, S] =
    if s == stateLattice.bottom then bot else c.assignees.map(v => v -> s).toMap

  /* If a variable v maps to a set of states that intersects s, then there
  exists a state satisfying s under which v can be written. Thus, we stabilise s
  by eliminating v. Since this weakens s, we have to keep doing this until we
  reach a fixpoint. (However, we may not need to if t is transitive?) */
  def apply(t: Map[Variable, S], s: S): S = {
    var old_s = s
    var new_s = s
    var fixpoint = false
    while (!fixpoint) {
      for ((v, cond) <- t) {
        if (stateLattice.glb(cond, s) != stateLattice.bottom) {
          new_s = stateLattice.drop(v, new_s)
        }
      }
      fixpoint = new_s == old_s
      old_s = new_s
    }
    new_s
  }

  // the join of two elements is just the component-wise join over the map
  def join(t1: Map[Variable, S], t2: Map[Variable, S]): Map[Variable, S] = {
    // set new_t = t1, then add everything in t2 to new_t
    var new_t = t1
    for ((v, cond) <- t2) {
      if (new_t.contains(v)) {
        // take the join
        new_t = new_t + (v -> stateLattice.lub(new_t(v), cond))
      } else {
        // just insert (v, cond) into new_t
        new_t = new_t + (v -> cond)
      }
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

/** We use this enum for the Direction lattice, to indicate the direction in
  * which a variable may be modified.
  */
enum Direction:
  case Top, // no constraints; variable can either increase or decrease
    Increasing, // variable only ever increases, or stays the same
    Decreasing, // variable only ever decreases, or stays the same
    Bot // variable stays the same; neither increases nor decreases

/** The Direction lattice is the core part of the monotonicity domain. The
  * lattice elements are the values of the Direction enum. It takes the shape:
  *   Top
  *   / \
  * Inc Dec
  *   \ /
  *   Bot
  */
object DirectionLattice extends Lattice[Direction] {
  val bottom: Direction = Direction.Bot

  override def top: Direction = Direction.Top

  def lub(x: Direction, y: Direction): Direction =
    if x == Direction.Bot then y
    else if y == Direction.Bot then x
    else if x == y then x
    else Direction.Top

  override def glb(x: Direction, y: Direction): Direction =
    if x == Direction.Top then y
    else if y == Direction.Top then x
    else if x == y then x
    else Direction.Bot
}

/** The monotonicity domain is an interference domain that represents
  * guarantee conditions of the form:
  * (v_1' <*> v_1) && (v_2' <*> v_2) && ...
  * where each v_i is a variable and <*> is any of: <, <=, ==, >=, >
  * 
  * This is implemented with a map [v_i -> Direction]. Variables that are never
  * written to are omitted from the map, rather than being mapped to bot.
  */
class MonotonicityDomain[S](
  stateLattice: InterferenceCompatibleLattice[S],
  stateTransfer: (S, Command) => S
) extends InterferenceDomain[Map[Variable, Direction], S](stateLattice, stateTransfer) {

  // an empty map means no variables have been written to
  def bot: Map[Variable, Direction] = Map.empty[Variable, Direction]

  /** To derive the direction in which an assignment v := E changes v, we
    * perform the assignment v' := E for a fresh variable v', then check whether
    * we can prove that v' <*> v for any <*> in: <, <=, ==, >=, >.
    */
  def derive(s: S, a: Assign): Map[Variable, Direction] =
    if s == stateLattice.bottom then Map.empty
    else
      a match {
        case c: LocalAssign => {
          // create fresh variable x
          val x = get_temp(c.lhs.irType)
          // do the assignment on x, with pre-state s
          val post = stateTransfer(s, LocalAssign(x, c.rhs))
          // apply the transfer function to assume x' > x and assume x' < x
          val assumeInc = stateTransfer(post, Assume(BinaryExpr(BVSGT, x, c.lhs)))
          val decreases = assumeInc == stateLattice.bottom
          val assumeDec = stateTransfer(post, Assume(BinaryExpr(BVSLT, x, c.lhs)))
          val increases = assumeDec == stateLattice.bottom
          // from the above, derive the direction in which x moved from a.lhs
          val dir =
            if (decreases && increases) then Direction.Bot // x' <= x && x' >= x
            else if (decreases) then Direction.Decreasing // x' <= x
            else if (increases) then Direction.Increasing // x' >= x
            else Direction.Top // could not prove any constraints of x' w.r.t. x
          // map a.lhs to the appropriate result
          Map(c.lhs -> dir)
        }
        case _ => bot
      }

  /** To simulate interference on a variable v, we introduce a temporary
    * variable y, together with an "assume" statement to add the appropriate
    * constraints to y, then we assign v := y, and finally remove y from the
    * abstract state. We do this for every entry in our map. We do not need a
    * fixpoint computation here, since the result after one pass is stable.
    */
  def apply(t: Map[Variable, Direction], s: S): S = {
    var postState = s
    for ((v, d) <- t) {
      // create fresh variable y
      val y = get_temp(v.irType)
      // apply assume statement to add constraints to y
      val yConstraints = d match {
        case Direction.Bot =>
          stateTransfer(postState, Assume(BinaryExpr(IntEQ, y, v)))
        case Direction.Increasing =>
          stateTransfer(postState, Assume(BinaryExpr(BVSGE, y, v)))
        case Direction.Decreasing =>
          stateTransfer(postState, Assume(BinaryExpr(BVSLE, y, v)))
        case Direction.Top => postState // add no constraints
      }
      // assign v := y
      postState = stateTransfer(postState, LocalAssign(v, y))
      // drop y
      postState = stateLattice.drop(y, postState)
    }
    postState
  }

  // the join of two elements is just the component-wise join over the map
  def join(t1: Map[Variable, Direction], t2: Map[Variable, Direction]): Map[Variable, Direction] = {
    // set new_t = t1, then add everything in t2 to new_t
    var new_t = t1
    for ((v, dir) <- t2) {
      if (new_t.contains(v)) {
        // take the join
        new_t = new_t + (v -> DirectionLattice.lub(new_t(v), dir))
      } else {
        // just insert (v, dir) into new_t
        new_t = new_t + (v -> dir)
      }
    }
    new_t
  }

  // this domain is inherently transitive
  def close(t: Map[Variable, Direction]): Map[Variable, Direction] = t

  def toString(t: Map[Variable, Direction]): String = {
    if (t.isEmpty) {
      return "forall v in Vars :: v' == v"
    }
    val sb = new StringBuilder
    val vars = t.keys.toList
    for (v <- vars) {
      val condStr = t(v) match {
        case Direction.Bot => s"${v}' == ${v} &&\n"
        case Direction.Increasing => s"${v}' >= ${v} &&\n"
        case Direction.Decreasing => s"${v}' <= ${v} &&\n"
        case Direction.Top => ""
      }
      sb.append(condStr)
    }
    sb.append("forall v in Vars - {")
    sb.append(vars.map(_.name).mkString(", "))
    sb.append("} :: v' == v")
    sb.toString()
  }
}

class PreciseDomain[S](
  stateLattice: InterferenceCompatibleLattice[S],
  stateTransfer: (S, Command) => S
) extends InterferenceDomain[Map[LocalAssign, S], S](stateLattice, stateTransfer) {

  def bot: Map[LocalAssign, S] = Map.empty[LocalAssign, S]

  def derive(s: S, a: Assign): Map[LocalAssign, S] =
    if (s == stateLattice.bottom) then Map.empty[LocalAssign, S]
    else
      a match {
        case c: LocalAssign => Map(c -> s)
        case _ => bot
      }

  /** Stabilising s under and element e in t requires adding to s the states
    * that are reachable by executing e.inst under the meet of e.pre and s. This
    * needs to be repeated for each element in t until a fixpoint is reached.
    * (The requirement to compute a fixpoint may be possibly avoided if t has
    * been made transitive.) Since we do not have a decent means of widening,
    * our fixpoint computation involves going to top after reaching some maximum
    * number of iterations.
    */
  def apply(t: Map[LocalAssign, S], s: S): S = {
    var old_s = s
    var new_s = s
    var fixpoint = false
    var iterations = 0
    val maxIterations = 10
    while (!fixpoint && iterations < 10) {
      for ((inst, pre) <- t) {
        val meet = stateLattice.glb(new_s, pre)
        new_s = stateLattice.lub(new_s, stateTransfer(meet, inst))
      }
      fixpoint = new_s == old_s
      old_s = new_s
      iterations = iterations + 1
    }
    new_s
  }

  def join(t1: Map[LocalAssign, S], t2: Map[LocalAssign, S]): Map[LocalAssign, S] = {
    // set new_t = t1, then add everything in t2 to new_t
    var new_t = t1
    for ((c, s) <- t2) {
      if (new_t.contains(c)) {
        // take the join
        new_t = new_t + (c -> stateLattice.lub(new_t(c), s))
      } else {
        // just insert (c, s) into new_t
        new_t = new_t + (c -> s)
      }
    }
    new_t
  }

  // todo: not sure if this is possible
  def close(t: Map[LocalAssign, S]): Map[LocalAssign, S] = t

  def toString(t: Map[LocalAssign, S]): String = {
    if (t.isEmpty) {
      return "forall v in Vars :: v' == v"
    }
    val sb = new StringBuilder
    for ((inst, pre) <- t) {
      sb.append("(")
      sb.append(stateLattice.toPredString(pre))
      sb.append(" && ")
      sb.append(inst.lhs.name)
      sb.append("' == ")
      sb.append(inst.rhs) // todo: convert expr into string?
      sb.append(") || \n")
    }
    sb.append("forall v in Vars :: v' == v")
    sb.toString()
  }
}
