package analysis

import analysis.Interval.ConcreteInterval
import ir.*
import ir.transforms.AbstractDomain
import util.assertion.*

trait MayAnalysis
trait MustAnalysis

/** A domain that performs two analyses in parallel.
  */
trait ProductDomain[L1, L2] extends AbstractDomain[(L1, L2)] {
  def join(a: (L1, L2), b: (L1, L2), pos: Block): (L1, L2) = (d1.join(a._1, b._1, pos), d2.join(a._2, b._2, pos))

  def d1: AbstractDomain[L1]
  def d2: AbstractDomain[L2]

  override def widen(a: (L1, L2), b: (L1, L2), pos: Block): (L1, L2) =
    (d1.widen(a._1, b._1, pos), d2.widen(a._2, b._2, pos))
  override def narrow(a: (L1, L2), b: (L1, L2)): (L1, L2) = (d1.narrow(a._1, b._1), d2.narrow(a._2, b._2))
  def transfer(a: (L1, L2), b: Command): (L1, L2) = (d1.transfer(a._1, b), d2.transfer(a._2, b))
  override def init(b: Block): (L1, L2) = (d1.init(b), d2.init(b))

  override def isFixed(prev: (L1, L2), next: (L1, L2)): Boolean =
    d1.isFixed(prev._1, next._1) && d2.isFixed(prev._2, next._2)

  def top: (L1, L2) = (d1.top, d2.top)
  def bot: (L1, L2) = (d1.bot, d2.bot)
}

/**
 * A domain that is the reduced product of two other domains.
 */
trait ReducedProductDomain[L1, L2] extends ProductDomain[L1, L2] {
  def reduce(a: (L1, L2), c: Command): (L1, L2)
  override def transfer(a: (L1, L2), c: Command): (L1, L2) = {
    val things = (d1.transfer(a._1, c), d2.transfer(a._2, c))
    // PERFORMANCE: This reduces every variable every transfer. Needless to say that this is
    // quite inefficient.
    reduce(things, c)
  }
}

private implicit val intervalTerm: Interval = Interval.Bottom

/**
 * The reduced product between the interval and tnum domains.
 */
class TNumIntervalReducedProduct extends ReducedProductDomain[LatticeMap[Variable, Interval], Map[Variable, TNum]] {
  override def d1: AbstractDomain[LatticeMap[Variable, Interval]] = UnsignedIntervalDomain()
  override def d2: AbstractDomain[Map[Variable, TNum]] = TNumDomain()

  override def reduce(
    unreduced: (LatticeMap[Variable, Interval], Map[Variable, TNum]),
    c: Command
  ): (LatticeMap[Variable, Interval], Map[Variable, TNum]) = {
    def reduceSingle(int: Interval, x: TNum): (Interval, TNum) = {

      int match {
        case int: ConcreteInterval =>
          val interval =
            Interval.ConcreteInterval(refineLowerBound(int.lower, x), refineUpperBound(int.upper, x), int.width)
          val tnum = refineTnum(int.lower, int.upper, x)
          (interval, tnum)
        case Interval.Top => (Interval.Top, x)
        case Interval.Bottom => (Interval.Bottom, x)
      }
    }

    c match {
      case c: LocalAssign =>
        val result = reduceSingle(unreduced._1(c.lhs), unreduced._2(c.lhs))
        (unreduced._1 + (c.lhs -> result._1), unreduced._2 + (c.lhs -> result._2))
      case c: MemoryAssign =>
        val result = reduceSingle(unreduced._1(c.lhs), unreduced._2(c.lhs))
        (unreduced._1 + (c.lhs -> result._1), unreduced._2 + (c.lhs -> result._2))
      case c: SimulAssign =>
        val (ints, tnums) = c.assignments
          .map((v, e) => v)
          .map(v => (v, reduceSingle(unreduced._1(v), unreduced._2(v))))
          .map((v, t) => ((v, t._1), (v, t._2)))
          .unzip
        (unreduced._1 ++ ints.toMap, unreduced._2 ++ tnums)
      case c: Return =>
        val (ints, tnums) = c.outParams
          .map((v, e) => v)
          .map(v => (v, reduceSingle(unreduced._1(v), unreduced._2(v))))
          .map((v, t) => ((v, t._1), (v, t._2)))
          .unzip
        (unreduced._1 ++ ints.toMap, unreduced._2 ++ tnums)
      case _ => unreduced
    }
  }

  /* These three methods (refine*()) are only exposed publicly so they can be tested, since they
   * contain most of the logic for this domain. You probably shouldn't use them.
   */
  def refineLowerBound(a: BigInt, x: TNum): BigInt = {
    var newBound = x.maxUnsignedValue().value
    for i <- x.width to 0 by -1
    do
      if 0 != (x.mask.value & (1 << i)) then
        newBound = newBound & (~(1 << i)) // Unset the ith bit
        if newBound < a then newBound = newBound | (1 << i) // Re-set the ith bit
    newBound
  }

  def refineUpperBound(a: BigInt, x: TNum): BigInt = {
    var newBound = x.minUnsignedValue().value
    for i <- x.width to 0 by -1
    do
      if 0 != (x.mask.value & (1 << i)) then
        newBound = newBound | (1 << i) // Set the ith bit
        if newBound > a then newBound = newBound & (~(1 << i)) // Unset the ith bit
    newBound
  }

  def refineTnum(a: BigInt, b: BigInt, x: TNum): TNum = {
    val mask = ~(a ^ b)
    var lb = a // An extra copy to obliterate instead of breaking from the loop
    var stupidVariable = 1
    var value = x.value.value
    var tnumMask = x.mask.value
    for i <- x.width - 1 to 0 by -1 do
      if (mask & (1 << i)) == 0 then
        lb = 0
        stupidVariable = 0 // Because break doesn't exist because actually this for loop is a
      // method call! or something. Just do nothing for the rest of the loop instead.
      /* if !(((value & (1 << i)) > 0) || ((value & (1 << i)) == (a & (1 << i)))) then
       *    TODO: go to bottom here because interval and tnum don't overlap */
      value = value | (lb & (1 << i))
      tnumMask = tnumMask & (~(stupidVariable << i))

    TNum(BitVecLiteral(value, x.width), BitVecLiteral(tnumMask, x.width))
  }
}

/**
 * Encodes the conjunction of two domain predicates.
 */
class PredProductDomain[L1, L2](
  override val d1: PredicateEncodingDomain[L1],
  override val d2: PredicateEncodingDomain[L2]
) extends ProductDomain[L1, L2]
    with PredicateEncodingDomain[(L1, L2)] {

  def toPred(x: (L1, L2)): Predicate = Predicate.and(d1.toPred(x._1), d2.toPred(x._2)).simplify

  override def fromPred(p: Predicate): (L1, L2) = (d1.fromPred(p), d2.fromPred(p))
}

import collection.mutable

/**
 * This domain stores as abstract values, sets of abstract values in the provided abstract domain.
 * A set of values represents the disjunction of the values in the set. For example, if S = {a, b, c},
 * then a state s is represented by S if and only if s is represented by either a, b or c (inclusive).
 * Doing this allows us to replace the join operator with a set union of abstract states, making the
 * join exact.
 */
class DisjunctiveCompletion[L](d: AbstractDomain[L]) extends AbstractDomain[Set[L]] {
  def collapse(a: Set[L], pos: Block): Set[L] = Set(a.foldLeft(d.bot) { (a, b) => d.join(a, b, pos) })

  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    a.union(b)

  override def widen(a: Set[L], b: Set[L], pos: Block): Set[L] =
    for {
      a2 <- collapse(a, pos)
      b2 <- collapse(b, pos)
    } yield d.widen(a2, b2, pos)
  override def narrow(a: Set[L], b: Set[L]): Set[L] = ???
  def transfer(a: Set[L], b: Command): Set[L] = a.map(l => d.transfer(l, b))
  override def init(b: Block): Set[L] = Set(d.init(b))

  def top: Set[L] = Set(d.top)
  def bot: Set[L] = Set(d.bot)
}

/**
 * Encodes a disjunctive completion as the disjunction of a set of predicates.
 */
class PredDisjunctiveCompletion[L](d: PredicateEncodingDomain[L])
    extends DisjunctiveCompletion[L](d)
    with PredicateEncodingDomain[Set[L]] {
  def toPred(x: Set[L]): Predicate = x.foldLeft(Predicate.False) { (p, l) => Predicate.or(p, d.toPred(l)) }.simplify

  override def fromPred(p: Predicate): Set[L] = p match {
    case Predicate.Disj(s) => s.map(d.fromPred(_))
    case _ => Set(d.fromPred(p))
  }
}

/**
 * Obtain an exact join by encoding sets of abstract states, taking set unions.
 * If the set's size exceeds the bound, join all elements into a single term using the underlying domain's join operator.
 */
class BoundedDisjunctiveCompletion[L](d: AbstractDomain[L], bound: Int) extends AbstractDomain[Set[L]] {
  debugAssert(bound > 0)

  private var collapsePoints: mutable.Set[Block] = mutable.Set()

  def collapse(a: Set[L], pos: Block): Set[L] =
    collapsePoints += pos
    Set(a.foldLeft(d.bot) { (a, b) => d.join(a, b, pos) })

  def bound(a: Set[L], pos: Block): Set[L] =
    if collapsePoints.contains(pos) || a.size > bound then collapse(a, pos) else a

  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    bound(a.union(b), pos)

  override def widen(a: Set[L], b: Set[L], pos: Block): Set[L] =
    for {
      a2 <- collapse(a, pos)
      b2 <- collapse(b, pos)
    } yield d.widen(a2, b2, pos)

  override def narrow(a: Set[L], b: Set[L]): Set[L] = ???
  def transfer(a: Set[L], b: Command): Set[L] = a.map(l => d.transfer(l, b))
  override def init(b: Block): Set[L] = Set(d.init(b))

  def top: Set[L] = Set(d.top)
  def bot: Set[L] = Set(d.bot)
}

/**
 * Encodes a bounded disjunctive completion as the disjunction of a set of predicates.
 */
class PredBoundedDisjunctiveCompletion[L](d: PredicateEncodingDomain[L], bound: Int)
    extends BoundedDisjunctiveCompletion[L](d, bound)
    with PredicateEncodingDomain[Set[L]] {
  def toPred(x: Set[L]): Predicate = x.foldLeft(Predicate.False) { (p, l) => Predicate.or(p, d.toPred(l)) }.simplify

  override def fromPred(p: Predicate): Set[L] = p match {
    // TODO we can't bound since bounding performs a join, and joining requires a block position, and we want to call fromPred from assume and assert commands
    case Predicate.Disj(s) => s.map(d.fromPred(_))
    case _ => Set(d.fromPred(p))
  }
}
