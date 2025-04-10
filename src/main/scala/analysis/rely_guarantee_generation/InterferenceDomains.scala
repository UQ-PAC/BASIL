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
  // the empty set of state transitions
  def bot: T
  // the state transitions that may occur by executing c in a state satisfying s
  def derive(s: S, c: Command): T
  // a weakened version of s that accounts for all transitions in t
  def apply(t: T, s: S): S
  // an overapproximation of the union of transitions captured by t1 and t2
  def join(t1: T, t2: T): T
  // an overapproximation of the transitive closure of t
  def close(t: T): T
  // representation of t as a predicate over primed and unprimed variables
  def toString(t: T): String
}

/** The conditional-writes domain is an interference domain that represents
  * guarantee conditions of the form:
  * (!P_1 ==> v_1' == v_1) && (!P_2 ==> v_2' == v_2) && ...
  * where P_i is an element of our state domain and v_i is a variable.
  * 
  * This is implemented with a map [v_i -> P_i]. Variables that are never
  * written to are omitted from the map, rather than being mapped to bot.
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
case class MonotonicityDomain[S](
  override val stateLattice: InterferenceCompatibleLattice[S], 
  override val stateTransfer: (S, Command) => S
) extends InterferenceDomain[Map[Variable, Direction], S](stateLattice, 
  stateTransfer) {
  
  /* Fixme: This may result in naming clashes, however we don't always have a
  Procedure at hand to call getFreshSSAVar(...). Ideally, we might pass around
  some kind of variable factory. */
  private def get_temp(ty: IRType): LocalVar = LocalVar("rg_temp_variable", ty, 0)

  // an empty map means no variables have been written to
  def bot: Map[Variable, Direction] = Map.empty[Variable, Direction]

  /** To derive the direction in which an assignment v := E changes v, we
    * perform the assignment v' := E for a fresh variable v', then check whether
    * we can prove that v' <*> v for any <*> in: <, <=, ==, >=, >.
    */
  def derive(s: S, c: Command): Map[Variable, Direction] =
    if (s == stateLattice.bottom) Map.empty
    else c match {
      case a: LocalAssign => {
        // create fresh variable x
        val x = get_temp(a.lhs.irType)
        // do the assignment on x, with pre-state s
        val post = stateTransfer(s, LocalAssign(x, a.rhs))
        // apply the transfer function to assume x' > x and assume x' < x
        val assumeInc = stateTransfer(post, Assume(BinaryExpr(BVSGT, x, a.lhs)))
        val decreases = assumeInc == stateLattice.bottom
        val assumeDec = stateTransfer(post, Assume(BinaryExpr(BVSLT, x, a.lhs)))
        val increases = assumeDec == stateLattice.bottom
        // from the above, derive the direction in which x moved from a.lhs
        val dir =
          if (decreases && increases) then Direction.Bot // x' <= x && x' >= x
          else if (decreases) then Direction.Decreasing // x' <= x
          else if (increases) then Direction.Increasing // x' >= x
          else Direction.Top // could not prove any constraints of x' w.r.t. x
        // map a.lhs to the appropriate result
        Map(a.lhs -> dir)
      }
      case _ => bot // fixme: for now, we only handle LocalAssigns
    }

  /** To simulate interference on a variable v, we introduce a temporary
    * variable y, together with an "assume" statement to add the appropriate
    * constraints to y, then we assign v := y, and finally remove y from the
    * abstract state. We do this for every entry in our map.
    */
  def apply(t: Map[Variable, Direction], s: S): S = {
    var postState = s
    for ((v, d) <- t) {
      // create fresh variable y
      // todo: to create a variable, we need access to the program
      // this is resolved in crab by having a var factory passed to every analysis
      // val y = c.parent.parent.getFreshSSAVar("temp", a.lhs.irType)
      val y = get_temp(v.irType)
      // apply assume statement to add constraints to y
      val yConstraints = d match {
        case Direction.Bot => stateTransfer(postState, Assume(BinaryExpr(IntEQ, y, v)))
        case Direction.Increasing => stateTransfer(postState, Assume(BinaryExpr(BVSGE, y, v)))
        case Direction.Decreasing => stateTransfer(postState, Assume(BinaryExpr(BVSLE, y, v)))
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
    var result = Map.empty[Variable, Direction]
    for ((v, d) <- t1) {
      val direction = if (t2.contains(v)) DirectionLattice.lub(d, t2(v)) else d
      result = result + (v -> direction)
    }
    result
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

case class TransitionPair[S](pre: S, inst: LocalAssign)

case class PreciseDomain[S](
  override val stateLattice: InterferenceCompatibleLattice[S], 
  override val stateTransfer: (S, Command) => S
) extends InterferenceDomain[Set[TransitionPair[S]], S](stateLattice,
  stateTransfer) {
  
  /* Fixme: This may result in naming clashes, however we don't always have a
  Procedure at hand to call getFreshSSAVar(...). Ideally, we might pass around
  some kind of variable factory. */
  private def get_temp(ty: IRType): LocalVar = LocalVar("rg_temp_variable", ty, 0)

  def bot: Set[TransitionPair[S]] = Set.empty[TransitionPair[S]]

  def derive(s: S, c: Command): Set[TransitionPair[S]] =
    if (s == stateLattice.bottom) Set.empty
    else c match {
      case a: LocalAssign => Set(TransitionPair[S](s, a))
      case _ => bot // fixme: for now, we only handle LocalAssigns
    }

  def apply(t: Set[TransitionPair[S]], s: S): S = {
    var new_s = s
    for (transition <- t) {
      // fixme: this applies every transition once instead of deriving a fixpoint
      // it might be good to make transitivity optional for particular domains
      // this would allow fixpoints to be computed only where it makes sense,
      // like in this domain (but not say, conditional writes?)
      val meet = stateLattice.glb(new_s, transition.pre)
      new_s = stateLattice.lub(new_s, stateTransfer(meet, transition.inst))
    }
    new_s
  }

  /* Fixme: This implementation requires that interference domain elements are
  cleared on every iteration of RG-Gen for this thread. This differs from the
  approach taken by Mine, so we have to confirm this is what we want to do. */
  def join(t1: Set[TransitionPair[S]], 
    t2: Set[TransitionPair[S]]): Set[TransitionPair[S]] = t1 | t2

  // todo; not sure if this is possible
  def close(t: Set[TransitionPair[S]]): Set[TransitionPair[S]] = t

  def toString(t: Set[TransitionPair[S]]): String = {
    if (t.isEmpty) {
      return "forall v in Vars :: v' == v"
    }
    val sb = new StringBuilder
    for (transition <- t) {
      sb.append("(")
      sb.append(stateLattice.toPredString(transition.pre))
      sb.append(" && ")
      sb.append(transition.inst.lhs.name)
      sb.append("' == ")
      sb.append(transition.inst.rhs) // todo: convert expr into string?
      sb.append(") || \n")
    }
    sb.append("forall v in Vars :: v' == v")
    sb.toString()
  }
}
