package util.functional

import collection.immutable.LinearSeq
import collection.{SeqOps, IterableOps, Factory}

/**
 * This unapplier enables pattern matching on the /last/ element of
 * an iterable.
 *
 * Given an iterable, it either returns Some((init, last)) where
 * last is the last element and init is everything except the last
 * element, or None if the iterable is empty.
 *
 * Snoc is the reverse of cons, which is the usual +: operator.
 */
object Snoc {

  def unapply[A, CC[_], C](x: IterableOps[A, CC, C]): Option[(C, A)] = {
    if (x.isEmpty) then {
      None
    } else {
      Some((x.init, x.last))
    }
  }

}

/**
 * "Sequences" an iterable of iterables. That is, the order of the collections
 * is reversed. Given a CC[DD[T]], returns a DD[CC[T]] subject to certain semantics.
 *
 * For example, a common use is sequencing a List[Option[T]] into Option[List[T]].
 * This would returns Some if all the input options are Some, and it returns None
 * if any of the input options were None. This can be thought of as a notion of
 * "all succeeding".
 *
 * This should be invoked by first passing the companion object of the input's
 * inner collection type (equivalently, the outer type of the result), then passing
 * the input collection - as two separate function calls.
 *
 * For example,
 * ```
 * sequence(Option)(List(Some(1), None)) == None
 * sequence(Vector)(List(Vector(1, 2), Vector(3, 4))) == Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
 * ```
 */
def sequence[T, CC[U] <: IterableOps[U, CC, CC[U]], DD[V] <: IterableOps[V, DD, DD[V]]](
  ddf: Factory[Nothing, DD[Nothing]]
)(xs: CC[DD[T]]): DD[CC[T]] = {
  def dd0(): DD[Nothing] = ddf.newBuilder.result
  def dd(x: CC[T]): DD[CC[T]] = dd0().iterableFactory.newBuilder.addOne(x).result
  def cc0(): CC[T] = xs.iterableFactory.empty
  def cc(x: T): CC[T] = xs.iterableFactory.newBuilder.addOne(x).result

  val base: DD[CC[T]] = dd(cc0())
  xs.foldRight(base) { case (ys, rest) =>
    ys.flatMap((y: T) => rest.map((r: CC[T]) => cc(y) ++ r))
  }
}

def sequence[T, CC[U] <: IterableOps[U, CC, CC[U]]](x: Option.type)(xs: CC[Option[T]]): Option[CC[T]] =
  sequence(Either)(xs.map(_.toRight(()))).toOption

def sequence[T, L, CC[U] <: IterableOps[U, CC, CC[U]]](x: Either.type)(xs: CC[Either[L, T]]): Either[CC[L], CC[T]] = {
  xs.partitionMap(identity) match {
    case (left, _) if left.nonEmpty => Left(left)
    case (_, right) => Right(right)
  }
}
