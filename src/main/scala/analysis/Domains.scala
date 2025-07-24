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

/** A domain that is the reduced product of two other domains.
 *  */
trait ReducedProductDomain[L1, L2] extends  ProductDomain[L1, L2] {
  def reduce(a: (L1, L2)): (L1, L2)
  override def transfer(a: (L1, L2), b: Command): (L1, L2) = {
    val things = (d1.transfer(a._1, b), d2.transfer(a._2, b))
    reduce(a)
  }
}

private implicit val intervalTerm: Interval = Interval.Bottom
// private implicit val tnumTerm: TNum = TNum.top(3506) //

class TNumIntervalReducedProduct extends ReducedProductDomain[LatticeMap[Variable, Interval],
  Map[Variable, TNum]] {
  override def d1: AbstractDomain[LatticeMap[Variable, Interval]] = UnsignedIntervalDomain()
  override def d2 : AbstractDomain[Map[Variable, TNum]] = TNumDomain()

  override def reduce(a: (LatticeMap[Variable, Interval], Map[Variable, TNum])): (LatticeMap[Variable, Interval], Map[Variable, TNum]) = {
    def reduceSingle(int: Interval, x: TNum): Option[(Interval, TNum)] = {
      def refineLowerBound(a: BigInt, x: TNum): BigInt = {
        var newBound = x.maxUnsignedValue().value
        for i <- x.width to 0 by -1
          do
          newBound = newBound & (~(1 << i)) // Unset the ith bit
          if newBound < a then
            newBound = newBound | (1 << i) // Re-set the ith bit
        newBound
      }
      def refineUpperBound(a: BigInt, x: TNum): BigInt = {
        var newBound = x.minUnsignedValue().value
        for i <- x.width to 0 by -1
          do
            newBound = newBound | (1 << i) // Set the ith bit
            if newBound > a then
              newBound = newBound & (~(1 << i)) // Unset the ith bit
        newBound
      }
      def refineTnum(a: BigInt, b: BigInt, x: TNum): Option[TNum] = {
        var mask = ~(a ^ b)
        var value = x.value.value
        var tnumMask = x.mask.value
        for i <- x.width to 0 by -1 do
            if (mask & (1 << i)) != 0 then
              mask = 0 // Because break doesn't exist because actually this for loop is
            // a method call! or something
            if !(((value & (1 << i)) > 0) || ((value & (1 << i)) == (a & (1 << i)))) then
              return None
            value = value | (a & (1 << i))
            tnumMask = tnumMask & (~(1 << i))

        Some(TNum(BitVecLiteral(value, x.width), BitVecLiteral(tnumMask, x.width)))
      }

      int match {
        case int@ConcreteInterval(lo, hi, width) =>
          var interval: ConcreteInterval = int
          var tnum: Option[TNum] = Some(x)
          while
            val oldInt = interval
            val oldx = tnum

            interval = Interval.ConcreteInterval(refineLowerBound(interval.lower, tnum.get),
              refineUpperBound(interval.upper, tnum.get), interval.width)
            tnum = refineTnum(interval.lower, interval.upper, tnum.get)

            oldInt == interval && oldx == tnum && tnum.nonEmpty
          do ()

          tnum match {
            case Some(thing) => Some((interval, thing))
            case None => None
          }
        case Interval.Top => Some((Interval.Top, x))
        case Interval.Bottom => Some((Interval.Bottom, x))
      }
    }

    for ((vara, interval) <- a._1.toMap) {
      val x: TNum = a._2(vara)
      val updated = reduceSingle(interval, x)
    }
    a
  }
}

/**
 * Encodes the conjunction of two domain predicates.
 */
class PredProductDomain[L1, L2](override val d1: PredicateEncodingDomain[L1],
                                override val d2: PredicateEncodingDomain[L2])
    extends ProductDomain[L1, L2]
    with PredicateEncodingDomain[(L1, L2)] {

  def toPred(x: (L1, L2)): Predicate = Predicate.and(d1.toPred(x._1), d2.toPred(x._2))

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

  private var joinCount: mutable.HashMap[Block, Int] = mutable.HashMap()

  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    joinCount += pos -> (joinCount.getOrElse(pos, 0) + 1)
    if a.contains(d.top) || b.contains(d.top) then top
    else {
      // TODO this is manual widening, maybe widening should be added to the solver instead
      if pos.isLoopHeader() || joinCount(pos) > 20 then widen(a, b, pos) else a.union(b)
    }

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

  def collapse(a: Set[L], pos: Block): Set[L] = Set(a.foldLeft(d.bot) { (a, b) => d.join(a, b, pos) })

  def bound(a: Set[L], pos: Block): Set[L] =
    if a.size > bound then collapse(a, pos) else a

  private var joinCount: mutable.Map[Block, Int] = mutable.Map()

  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    joinCount += pos -> (joinCount.getOrElse(pos, 0) + 1)
    bound(
      if a.contains(d.top) || b.contains(d.top) then top
      else {
        // TODO this is manual widening, maybe widening should be added to the solver instead
        if pos.isLoopHeader() || joinCount(pos) > 20 then widen(a, b, pos) else a.union(b)
      },
      pos
    )

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
