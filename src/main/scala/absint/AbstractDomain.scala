package absint
import ir.*
import analysis.*
import scala.collection.mutable
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

trait AbstractDomain[L] {
  def join(a: L, b: L): L
  def widen(a: L, b: L): L = join(a, b)
  def narrow(a: L, b: L): L = a
  // def transfer(a: L, b: Statement): L

  def top: L
  def bot: L

  def isTop(s: L): Boolean = s == top
  def isBot(s: L): Boolean = s == bot

  def transfer(a: L, b: Statement): L
  // def isTrue(s: L): Boolean
  // def isFalse(s: L): Boolean
}

class ProductDomain[L, R](val l: AbstractDomain[L], val r: AbstractDomain[R]) extends AbstractDomain[(L, R)] {
  override def join(a: (L, R), b: (L, R)): (L, R) = (l.join(a._1, b._1), r.join(a._2, b._2))
  override def widen(a: (L, R), b: (L, R)): (L, R) = (l.widen(a._1, b._1), r.widen(a._2, b._2))
  override def narrow(a: (L, R), b: (L, R)): (L, R) = (l.narrow(a._1, b._1), r.narrow(a._2, b._2))
  override def transfer(a: (L, R), b: Statement): (L, R) = (l.transfer(a._1, b), r.transfer(a._2, b))
  override def top = (l.top, r.top)
  override def bot = (l.bot, r.bot)
  def reduce(a: (L, R)): (L, R) = a
}

enum MapWithDefault[+L] {
  case Top
  case Bot
  // current lattice value, memory of lattice values at each code point
  case V(value: L, memory: Map[Statement, L])
}

sealed trait ValueLattice[+V]

sealed trait ValueLatticeWithTop[+V] extends ValueLattice[V]
sealed trait ValueLatticeWithBot[+V] extends ValueLattice[V]
sealed trait CompleteValueLattice[+V] extends ValueLattice[V]

case class Val[V](value: V)
    extends ValueLattice[V]
    with CompleteValueLattice[V]
    with ValueLatticeWithTop[V]
    with ValueLatticeWithBot[V] {
  override def toString = value.toString
}
case object Top extends ValueLatticeWithTop[Nothing] with CompleteValueLattice
case object Bot extends ValueLatticeWithBot[Nothing] with CompleteValueLattice

class DomainRepr[L] extends AbstractDomain[L] {
  override def join(a: L, b: L): L = throw Exception("")
  override def transfer(a: L, b: Statement): L = throw Exception("")
  override def top: L = ???
  override def bot: L = ???
}

class ConstantDomain[L] extends AbstractDomain[CompleteValueLattice[L]] {
  /* constant: join(a, b) -> top */
  def top = Top
  def bot = Bot

  override def join(a: CompleteValueLattice[L], b: CompleteValueLattice[L]) = {
    (a, b) match {
      case (Top, _)                   => Top
      case (_, Top)                   => Top
      case (Bot, v @ Val(_))          => v
      case (v @ Val(_), Bot)          => v
      case (Val(a), Val(b)) if a == b => Val(a)
      case (Val(a), Val(b)) if a != b => Top
      case (Bot, Bot)                 => Bot
    }
  }

  def toFlatEl(a : CompleteValueLattice[L]) : analysis.FlatElement[L] = {
    a match {
      case Top => analysis.Top
      case Bot => analysis.Bottom
      case Val(v) => FlatEl(v)
    }
  }

  override def transfer(c: CompleteValueLattice[L], s: Statement) = c
}

class ValueDomain[L](val d: AbstractDomain[L] = ConstantDomain()) extends AbstractDomain[CompleteValueLattice[L]] {
  /* Adds top/bot to an abstract domain d */
  def top = Top
  def bot = Bot

  override def join(a: CompleteValueLattice[L], b: CompleteValueLattice[L]) = {
    (a, b) match {
      case (Top, _)          => Top
      case (_, Top)          => Top
      case (Bot, Bot)        => Bot
      case (Bot, v @ Val(_)) => v
      case (v @ Val(_), Bot) => v
      case (Val(a), Val(b))  => Val(d.join(a, b))
    }
  }

  private def unwrap(v: L) = v match {
    case x if x == d.bot => Bot
    case x if x == d.top => Top
    case x               => Val(x)
  }

  override def transfer(a: CompleteValueLattice[L], b: Statement) = {
    a match {
      case Val(x) => unwrap(d.transfer(x, b))
      case Top    => Top
      case Bot    => unwrap(d.transfer(d.bot, b))
    }
  }

}

class PowerSetDomain[L](val d: AbstractDomain[L]) extends AbstractDomain[ValueSet[L]] {
  /* Powerset reprsenting the disjunction of values */
  // val d = summon[AbstractDomain[L]]
  override def top = ValueSet.Top()
  override def isTop(v: ValueSet[L]) = (v == ValueSet.Top())
  override def isBot(v: ValueSet[L]) = v match {
    case ValueSet.Elem(v) => v.isEmpty
    case _                => false
  }
  override def bot = ValueSet.Elem(HashSet())

  def unwrap(v: ValueSet[L]) = {
    v match {
      case ValueSet.Elem(v) if v.exists(d.isTop) => ValueSet.Top()
      case _                                     => v
    }

  }

  override def transfer(a: ValueSet[L], b: Statement): ValueSet[L] = {
    unwrap(a.flatMap(c => d.transfer(c, b)))
  }

  override def join(a: ValueSet[L], b: ValueSet[L]): ValueSet[L] = {
    unwrap(a.merge(b))
  }
}

class BoundedPowerSetDomain[L](val valdomain: AbstractDomain[L], val bound: Option[Int] = None)
    extends PowerSetDomain[L](valdomain) {
  export ValueSet._

  override def widen(a: ValueSet[L], b: ValueSet[L]) = {
    val j = join(a, b)
    (bound, j) match {
      case (_, b) if isTop(b)                => Top()
      case (None, j)                         => j
      case (Some(i), Elem(k)) if k.size >= i => Top()
      case (Some(i), j)                      => j
    }
  }

}

// sealed trait ValueSet[V] {
//   def +(v: V): ValueSet[V]
//   def merge(v: ValueSet[V]): ValueSet[V]
//   def remove(v: V): ValueSet[V]
//   def toSet(v: V): Set[V]
// }

//case class SElem[V](sd: AbstractDomain[V], e: HashSet[V]) extends ValueSet[V] {
//  def +(v: V): ValueSet[V] = if (sd.isBot(v)) then SElem(sd, e - v) else SElem(sd, e - v)
//  def merge(v: ValueSet[V]): ValueSet[V] = v match {
//    case SElem(sd, v) => SElem(sd, v ++ e)
//    case _            => SElem(sd, e)
//  }
//  def toSet(v: V): Set[V]
//}

enum ValueSet[V] {
  case Top()
  case Elem(e: HashSet[V])
}

extension [V](v: ValueSet[V]) {
  def flatMap(f: V => V): ValueSet[V] = {
    v match {
      case ValueSet.Elem(st) => ValueSet.Elem(st.map(f))
      //if (r.exists(sd.isTop)) ValueSet.Top else ValueSet.Elem(sd, r)
      case ValueSet.Top() => ValueSet.Top()
    }
  }

  def +(el: V) = {
    v match {
      case ValueSet.Top()    => ValueSet.Top()
      case ValueSet.Elem(oe) => ValueSet.Elem(oe + el)
    }
  }

  def merge(v2: ValueSet[V]) = {
    (v, v2) match {
      case (ValueSet.Top(), _) => ValueSet.Top()
      case (_, ValueSet.Top()) => ValueSet.Top()
      case (ValueSet.Elem(ol), ValueSet.Elem(or)) => {
        ValueSet.Elem(ol ++ or)
      }
    }
  }
}

sealed trait ML[K, V]:
  def apply(x: K): V
  def get(x: K): Option[V]
  def +(kv: (K, V)): ML[K, V]
  def updated(k: K, v: V) = this + (k -> v)
  def removed(k: K): ML[K, V]
  def toMap: Map[K, V]
  def merge(x: ML[K, V]): ML[K, V]
  def map[B](f: V => B): Map[K, B]

sealed trait MapLattice[K, V] extends DomainRepr[ML[K, V]]

case class Elem[K, V](sd: AbstractDomain[V], m: HashMap[K, V]) extends ML[K, V]:
  def apply(x: K) = m.getOrElse(x, sd.bot)
  def get(x: K) = m.get(x)
  def removed(k: K): ML[K, V] = this + (k -> sd.bot)
  def +(kv: (K, V)) =
    if kv._2 == sd.bot then Elem(sd, m - kv._1) else Elem(sd, m + kv)

  def toMap = m.toMap.withDefaultValue(sd.bot)
  def merge(x: ML[K, V]) = x match {
    case Elem(sd, n) => Elem(sd, m.merged(n)((p, q) => (p._1, sd.join(p._2, q._2))))
    case _           => Elem(sd, m)
  }
  def map[B](f: V => B) = m.map((k, v) => (k, f(v))).withDefaultValue(f(sd.bot))

case class Bottom[K, V](sd: AbstractDomain[V]) extends ML[K, V]:
  def apply(x: K) = sd.bot
  def +(kv: (K, V)) =
    if kv._2 == sd.bot then Bottom(sd) else Elem(sd, HashMap(kv))
  def get(x: K) = None
  def removed(k: K): ML[K, V] = this + (k -> sd.bot)
  def toMap = Map().withDefaultValue(sd.bot)
  def merge(x: ML[K, V]) = x
  def map[B](f: V => B) = Map().withDefaultValue(f(sd.bot))

class AbstractMapDomain[K, V](val d: AbstractDomain[V]) extends AbstractDomain[ML[K, V]] {
  def top = ???
  def bot = Bottom(d)
  override def isTop(d: ML[K, V]) = false
  def toMap(v: ML[K, V]) = v.toMap
  def join(a: ML[K, V], b: ML[K, V]): ML[K, V] = a.merge(b)
  def transfer(a: ML[K, V], b: Statement): ML[K, V] = a
}

class CollectingDomain[V](val valdomain: AbstractDomain[V])
    extends ProductDomain[V, ML[CFGPosition, V]](valdomain, AbstractMapDomain[CFGPosition, V](valdomain)) {
  /*
   * Perform absint in the left abstract domain and keep a record in the right map domain
   */

  override def transfer(a: (V, ML[CFGPosition, V]), s: Statement): (V, ML[CFGPosition, V]) = {
    val v = valdomain.transfer(a._1, s)
    (v, a._2 + (s -> v))
  }

  def toMap(v: (V, ML[CFGPosition, V])) : Map[CFGPosition, V] = v._2.toMap
}
