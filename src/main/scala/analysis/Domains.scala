package analysis

import ir.*
import ir.transforms.AbstractDomain

trait MayAnalysis
trait MustAnalysis

/** A domain that performs two analyses in parallel.
  */
class ProductDomain[L1, L2](d1: AbstractDomain[L1], d2: AbstractDomain[L2]) extends AbstractDomain[(L1, L2)] {
  def join(a: (L1, L2), b: (L1, L2), pos: Block): (L1, L2) = (d1.join(a._1, b._1, pos), d2.join(a._2, b._2, pos))

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
 * Encodes the conjunction of two domain predicates.
 */
class PredProductDomain[L1, L2](d1: PredicateEncodingDomain[L1], d2: PredicateEncodingDomain[L2])
  extends ProductDomain[L1, L2](d1, d2) with PredicateEncodingDomain[(L1, L2)] {

  def toPred(x: (L1, L2)): Predicate = Predicate.Bop(BoolAND, d1.toPred(x._1), d2.toPred(x._2))

  override def fromPred(p: Predicate): (L1, L2) = (d1.fromPred(p), d2.fromPred(p))
}

/**
 * This domain stores as abstract values, sets of abstract values in the provided abstract domain.
 * A set of values represents the disjunction of the values in the set. For example, if S = {a, b, c},
 * then a state s is represented by S if and only if s is represented by either a, b or c (inclusive).
 * Doing this allows us to replace the join operator with a set union of abstract states, making the
 * join exact.
 */
class DisjunctiveCompletion[L](d: AbstractDomain[L]) extends AbstractDomain[Set[L]] {
  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    if a.contains(d.top) || b.contains(d.top) then top else a.union(b)

  override def widen(a: Set[L], b: Set[L], pos: Block): Set[L] = ???
  override def narrow(a: Set[L], b: Set[L]): Set[L] = ???
  def transfer(a: Set[L], b: Command): Set[L] = a.map(l => d.transfer(l, b))
  override def init(b: Block): Set[L] = Set(d.init(b))

  def top: Set[L] = Set(d.top)
  def bot: Set[L] = Set(d.bot)
}

/**
 * Encodes a disjunctive completion as the disjunction of a set of predicates.
 */
class PredDisjunctiveCompletion[L](d: PredicateEncodingDomain[L]) extends DisjunctiveCompletion[L](d) with PredicateEncodingDomain[Set[L]] {
  def toPred(x: Set[L]): Predicate = x.foldLeft(Predicate.Lit(FalseLiteral)) { (p, l) => Predicate.Bop(BoolOR, p, d.toPred(l)) }.simplify

  override def fromPred(p: Predicate): Set[L] = p match {
    case Predicate.Bop(BoolOR, a, b) => fromPred(a).union(fromPred(b))
    case _ => Set(d.fromPred(p))
  }
}

/**
 * Obtain an exact join by encoding sets of abstract states, taking set unions.
 * If the set's size exceeds the bound, join all elements into a single term using the underlying domain's join operator.
 */
class BoundedDisjunctiveCompletion[L](d: AbstractDomain[L], bound: Int) extends AbstractDomain[Set[L]] {
  assert(bound > 0)

  def bound(a: Set[L], pos: Block): Set[L] =
    if a.size > bound then Set(a.foldLeft(d.bot) { (a, b) => d.join(a, b, pos) }) else a

  def join(a: Set[L], b: Set[L], pos: Block): Set[L] =
    bound(if a.contains(d.top) || b.contains(d.top) then top else a.union(b), pos)

  override def widen(a: Set[L], b: Set[L], pos: Block): Set[L] = ???
  override def narrow(a: Set[L], b: Set[L]): Set[L] = ???
  def transfer(a: Set[L], b: Command): Set[L] = a.map(l => d.transfer(l, b))
  override def init(b: Block): Set[L] = Set(d.init(b))

  def top: Set[L] = Set(d.top)
  def bot: Set[L] = Set(d.bot)
}
