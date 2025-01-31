package analysis

import ir.*
import ir.transforms.AbstractDomain

/** Lattice structure internal to a type.
  */
trait InternalLattice[T <: InternalLattice[T]] {
  def join(other: T): T
  def meet(other: T): T

  def top: T
  def bottom: T
}

/** An element of a powerset lattice. This type represents Top and Bottom and finite sets, and is closed under unions,
  * intersections, and set difference.
  */
enum LatticeSet[T] extends InternalLattice[LatticeSet[T]] {
  /* The set of all terms of type T */
  case Top[T1]() extends LatticeSet[T1]
  /* The empty set */
  case Bottom[T1]() extends LatticeSet[T1]
  /* A finite set. Note that an empty set encodes the same information as Bottom(). */
  case FiniteSet[T1](s: Set[T1]) extends LatticeSet[T1]
  /* Represents Top() minus some finite set. */
  case DiffSet[T1](s: Set[T1]) extends LatticeSet[T1]

  def contains(v: T): Boolean = {
    this match {
      case Top() => true
      case Bottom() => false
      case FiniteSet(s) => s.contains(v)
      case DiffSet(s) => !s.contains(v)
    }
  }

  def join(other: LatticeSet[T]): LatticeSet[T] = {
    (this, other) match {
      case (Top(), _) => Top()
      case (Bottom(), b) => b
      case (FiniteSet(a), FiniteSet(b)) => FiniteSet(a.union(b))
      case (FiniteSet(a), DiffSet(b)) => DiffSet(b -- a)
      case (DiffSet(a), DiffSet(b)) => DiffSet(a.intersect(b))
      case (a, b) => b.join(a)
    }
  }

  def meet(other: LatticeSet[T]): LatticeSet[T] = {
    (this, other) match {
      case (Top(), b) => b
      case (Bottom(), _) => Bottom()
      case (FiniteSet(a), FiniteSet(b)) => FiniteSet(a.intersect(b))
      case (FiniteSet(a), DiffSet(b)) => FiniteSet(a.filter(x => !b.contains(x)))
      case (DiffSet(a), DiffSet(b)) => DiffSet(a.union(b))
      case (a, b) => b.meet(a)
    }
  }

  def diff(other: LatticeSet[T]): LatticeSet[T] = {
    (this, other) match {
      case (_, Top()) => Bottom()
      case (a, Bottom()) => a
      case (Bottom(), _) => Bottom()
      case (Top(), FiniteSet(b)) => DiffSet(b)
      case (FiniteSet(a), FiniteSet(b)) => FiniteSet(a.diff(b))
      case (DiffSet(a), FiniteSet(b)) => DiffSet(a.union(b))
      case (Top(), DiffSet(b)) => FiniteSet(b)
      case (FiniteSet(a), DiffSet(b)) => FiniteSet(a.intersect(b))
      case (DiffSet(a), DiffSet(b)) => FiniteSet(b.diff(a))
    }
  }

  // TODO maybe the performance could be better here
  def +(v: T): LatticeSet[T] = this.join(FiniteSet(Set(v)))
  def -(v: T): LatticeSet[T] = this.diff(FiniteSet(Set(v)))
  def union(other: LatticeSet[T]): LatticeSet[T] = this.join(other)
  def intersect(other: LatticeSet[T]): LatticeSet[T] = this.meet(other)
  def ++(other: LatticeSet[T]): LatticeSet[T] = this.union(other)
  def ++(other: Iterable[T]): LatticeSet[T] = this.union(FiniteSet(other.toSet))
  def --(other: LatticeSet[T]): LatticeSet[T] = this.diff(other)
  def --(other: Iterable[T]): LatticeSet[T] = this.diff(FiniteSet(other.toSet))

  def top: LatticeSet[T] = Top()
  def bottom: LatticeSet[T] = Bottom()

  /** Try to convert to a finitely represented set. Returns None if this set is infinite
    */
  def tryToSet: Option[Set[T]] = {
    this match {
      case Top() => None
      case Bottom() => Some(Set())
      case FiniteSet(s) => Some(s)
      case DiffSet(_) => None
    }
  }
}

class LatticeSetLattice[T] extends Lattice[LatticeSet[T]] {
  import LatticeSet.{Top, Bottom, FiniteSet, DiffSet}

  type Element = LatticeSet[T];

  def lub(a: LatticeSet[T], b: LatticeSet[T]): LatticeSet[T] = a.join(b)

  override def glb(a: LatticeSet[T], b: LatticeSet[T]): LatticeSet[T] = a.meet(b)

  override def top: LatticeSet[T] = Top()
  val bottom: LatticeSet[T] = Bottom()
}

/** A map which defaults to either the top or bottom element of a lattice. This is more efficient to use in static
  * analyses as it is common to default most values in a map to either top or bottom.
  */
enum LatticeMap[D, L] {
  /* PERFORMANCE:
   * Something like an AVL tree could be more efficient, see section 4.1.4 of Antoine Miné's abstract interpretation tutorial.
   */

  /* A map that is top everywhere */
  case Top[D1, L1]() extends LatticeMap[D1, L1]
  /* A map that is bottom everywhere */
  case Bottom[D1, L1]() extends LatticeMap[D1, L1]
  /* A Map which defaults to top and is else specified by the internal map */
  case TopMap[D1, L1](m: Map[D1, L1]) extends LatticeMap[D1, L1]
  /* A Map which defaults to bottom and is else specified by the internal map */
  case BottomMap[D1, L1](m: Map[D1, L1]) extends LatticeMap[D1, L1]

  /** Update this map so that `from` now maps to `to`
    */
  def update(from: D, to: L): LatticeMap[D, L] = this match {
    case Top() => TopMap(Map(from -> to))
    case Bottom() => BottomMap(Map(from -> to))
    case TopMap(m) => TopMap(m + (from -> to))
    case BottomMap(m) => BottomMap(m + (from -> to))
  }

  /** Keeps all key value pairs satisfying the predicate, sending the rest to default
    */
  def filter(pred: ((D, L)) => Boolean): LatticeMap[D, L] = this match {
    case Top() => Top()
    case Bottom() => Bottom()
    case TopMap(m) => TopMap(m.filter(pred))
    case BottomMap(m) => BottomMap(m.filter(pred))
  }

  def toMap: Map[D, L] = this match {
    case Top() => Map()
    case Bottom() => Map()
    case TopMap(m) => m
    case BottomMap(m) => m
  }

  def +(kv: (D, L)): LatticeMap[D, L] = update(kv._1, kv._2)
  def ++(kv: Map[D, L]): LatticeMap[D, L] = kv.foldLeft(this) { (m, kv) => m + kv }

  /** Evaluate the function at `v`, accounting for defaulting behaviour.
    */
  def apply[L1 <: InternalLattice[L1]](v: D)(implicit s: L <:< L1, l: L1): L1 = this match {
    case Top() => l.top
    case Bottom() => l.bottom
    case TopMap(m) => m.getOrElse(v, l.top).asInstanceOf[L1]
    case BottomMap(m) => m.getOrElse(v, l.bottom).asInstanceOf[L1]
  }

  def join[L1 <: InternalLattice[L1]](other: LatticeMap[D, L1])(implicit s: L <:< L1, l: L1): LatticeMap[D, L1] =
    latticeMapJoin(this.asInstanceOf[LatticeMap[D, L1]], other, (a, b) => a.join(b), l.top, l.bottom)

  def meet[L1 <: InternalLattice[L1]](other: LatticeMap[D, L1])(implicit s: L <:< L1, l: L1): LatticeMap[D, L1] =
    latticeMapMeet(this.asInstanceOf[LatticeMap[D, L1]], other, (a, b) => a.meet(b), l.top, l.bottom)

  def top: LatticeMap[D, L] = Top()
  def bottom: LatticeMap[D, L] = Bottom()
}

private def latticeMapJoin[D, L](
  a: LatticeMap[D, L],
  b: LatticeMap[D, L],
  join: ((L, L) => L),
  top: => L,
  bottom: => L
): LatticeMap[D, L] = {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  (a, b) match {
    case (Top(), _) => Top()
    case (Bottom(), b) => b
    case (TopMap(a), TopMap(b)) =>
      TopMap(a.foldLeft(b)((m, p) => {
        val (k, v) = p
        m + (k -> join(m.getOrElse(k, top), v))
      }))
    case (TopMap(a), BottomMap(b)) =>
      TopMap(b.foldLeft(a)((m, p) => {
        val (k, v) = p
        m + (k -> join(m.getOrElse(k, top), v))
      }))
    case (BottomMap(a), BottomMap(b)) =>
      BottomMap(a.foldLeft(b)((m, p) => {
        val (k, v) = p
        m + (k -> join(m.getOrElse(k, bottom), v))
      }))
    case (a, b) => latticeMapJoin(b, a, join, top, bottom)
  }
}

private def latticeMapMeet[D, L](
  a: LatticeMap[D, L],
  b: LatticeMap[D, L],
  meet: ((L, L) => L),
  top: => L,
  bottom: => L
): LatticeMap[D, L] = {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  (a, b) match {
    case (Top(), b) => b
    case (Bottom(), _) => Bottom()
    case (TopMap(a), TopMap(b)) =>
      TopMap(a.foldLeft(b)((m, p) => {
        val (k, v) = p
        m + (k -> meet(m.getOrElse(k, top), v))
      }))
    case (TopMap(a), BottomMap(b)) =>
      BottomMap(a.foldLeft(b)((m, p) => {
        val (k, v) = p
        m + (k -> meet(m.getOrElse(k, bottom), v))
      }))
    case (BottomMap(a), BottomMap(b)) =>
      BottomMap(a.foldLeft(b)((m, p) => {
        val (k, v) = p
        m + (k -> meet(m.getOrElse(k, bottom), v))
      }))
    case (a, b) => latticeMapMeet(b, a, meet, top, bottom)
  }
}

/** Evaluate the map m at value d, defaulting based on the top and bottom values in the lattice l.
  */
def latticeMapApply[D, L, LA <: Lattice[L]](m: LatticeMap[D, L], d: D, l: LA): L = {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  m match {
    case Top() => l.top
    case Bottom() => l.bottom
    case TopMap(m) => m.getOrElse(d, l.top)
    case BottomMap(m) => m.getOrElse(d, l.bottom)
  }
}

class LatticeMapLattice[D, L, LA <: Lattice[L]](l: LA) extends Lattice[LatticeMap[D, L]] {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  type Element = LatticeMap[D, L];

  def lub(a: LatticeMap[D, L], b: LatticeMap[D, L]): LatticeMap[D, L] =
    latticeMapJoin(a, b, (x, y) => l.lub(x, y), l.top, l.bottom)

  override def glb(a: LatticeMap[D, L], b: LatticeMap[D, L]): LatticeMap[D, L] =
    latticeMapMeet(a, b, (x, y) => l.glb(x, y), l.top, l.bottom)

  override def top: LatticeMap[D, L] = Top()
  val bottom: LatticeMap[D, L] = Bottom()
}

/** A domain which has terms as maps. Implementing a MapDomain involves only defining operations element wise on the
  * codomain of the map (along with the transfer function).
  */
trait MapDomain[D, L] extends AbstractDomain[LatticeMap[D, L]] {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  def joinTerm(a: L, b: L, pos: Block): L

  def widenTerm(a: L, b: L, pos: Block): L = joinTerm(a, b, pos)

  def botTerm: L
  def topTerm: L

  def join(a: LatticeMap[D, L], b: LatticeMap[D, L], pos: Block): LatticeMap[D, L] =
    latticeMapJoin(a, b, (x, y) => joinTerm(x, y, pos), topTerm, botTerm)

  override def widen(a: LatticeMap[D, L], b: LatticeMap[D, L], pos: Block): LatticeMap[D, L] =
    (a, b) match {
      case (Bottom(), b) => b
      case (a, Bottom()) => a
      // TODO
      case (BottomMap(a), BottomMap(b)) => ???
      case (BottomMap(a), TopMap(b)) => ???
      case (BottomMap(a), Top()) => ???
      case (TopMap(a), BottomMap(b)) => ???
      case (TopMap(a), TopMap(b)) => ???
      case (TopMap(a), Top()) => ???
      case (Top(), BottomMap(b)) => ???
      case (Top(), TopMap(b)) => ???
      case (Top(), Top()) => ???
    }

  def bot: LatticeMap[D, L] = Bottom()
  def top: LatticeMap[D, L] = Top()
}

/**
 * A map domain that encodes predicates per term of a map.
 *
 * If you want to implement this trait, instead implement either `MayPredMapDomain` or `MustPredMapDomain`
 */
trait PredMapDomain[D, L] extends MapDomain[D, L] with PredicateEncodingDomain[LatticeMap[D, L]] {
  /**
   * Encode the information the abstract value `l` represents, as a predicate, when `l` is the result
   * of applying `d` to `m`.
   */
  def termToPred(m: LatticeMap[D, L], d: D, l: L): Predicate
}

/**
 * A map domain encoding predicates that is a may analysis.
 *
 * As described in `PredicateEncodingDomain`, a may analysis must overapproximate its encoded predicate,
 * where a concretised predicate should give a superset of the concretisation of the lattice element that
 * encoded the predicate.
 *
 * Extending this trait gives a sound implementation of the toPred method over a map using termToPred
 * for a may analysis, namely by mapping top to true, bottom and a map defaulting to bottom to false,
 * and mapping a top defaulting map to a conjunction of each individual predicate of the non default
 * elements. An analysis implementing this trait should thus have the initial state be a top defaulting
 * map.
 */
trait MayPredMapDomain[D, L] extends PredMapDomain[D, L] with MayAnalysis {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  def toPred(x: LatticeMap[D, L]): Predicate = x match {
    case Top() => Predicate.Lit(TrueLiteral)
    case TopMap(m) => m.foldLeft(Predicate.Lit(TrueLiteral)) {
      (p, z) => {
        val (d, l) = z
        termToPred(x, d, l) match {
          case Predicate.Lit(TrueLiteral) => p
          case q => Predicate.Bop(BoolAND, p, q)
        }
      }
    }.simplify
    case Bottom() => Predicate.Lit(FalseLiteral)
    case BottomMap(m) => Predicate.Lit(FalseLiteral)
  }
}

/**
 * A map domain encoding predicates that is a must analysis.
 *
 * See `MayPredMapDomain` for a description, but replace superset with subset, and bottom with top.
 * Importantly, note that an analysis implementing this trait should have the initial state be a
 * bottom defaulting map.
 */
trait MustPredMapDomain[D, L] extends PredMapDomain[D, L] with MustAnalysis {
  import LatticeMap.{Top, Bottom, TopMap, BottomMap}

  def toPred(x: LatticeMap[D, L]): Predicate = x match {
    case Top() => Predicate.Lit(FalseLiteral)
    case TopMap(m) => Predicate.Lit(FalseLiteral)
    case Bottom() => Predicate.Lit(TrueLiteral)
    case BottomMap(m) => m.foldLeft(Predicate.Lit(TrueLiteral)) {
      (p, z) => {
        val (d, l) = z
        termToPred(x, d, l) match {
          case Predicate.Lit(TrueLiteral) => p
          case q => Predicate.Bop(BoolAND, p, q)
        }
      }
    }.simplify
  }
}
