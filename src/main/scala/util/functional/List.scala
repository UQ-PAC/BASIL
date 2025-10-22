package util.functional

import collection.{IterableOps, Factory}

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

  def unapply[A, CC[_], C <: Iterable[A]](x: IterableOps[A, CC, C]): Option[(C, A)] = {
    if (x.isEmpty) then {
      None
    } else {
      val (init, last) = x.splitAt(x.size - 1)
      Some((init, last.head))
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
 * For example,
 * ```
 * sequence(List(Some(1), None)) == None
 * sequence(List(Vector(1, 2), Vector(3, 4))) == Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
 * ```
 *
 * If passing an empty iterable as the argument, you will need a type annotation on the argument.
 */
def sequence[DD[V] <: IterableOps[V, DD, DD[V]], CC[U] <: IterableOps[U, CC, CC[U]], T](
  xs: CC[DD[T]]
)(implicit ddf: Factory[Nothing, DD[Nothing]]): DD[CC[T]] = {
  def dd0(): DD[Nothing] = ddf.newBuilder.result
  def dd(x: CC[T]): DD[CC[T]] = dd0().iterableFactory.newBuilder.addOne(x).result
  def cc0(): CC[T] = xs.iterableFactory.empty
  def cc(x: T): CC[T] = xs.iterableFactory.newBuilder.addOne(x).result

  val base: DD[CC[T]] = dd(cc0())
  xs.foldRight(base) { case (ys, rest) =>
    ys.flatMap((y: T) => rest.map((r: CC[T]) => cc(y) ++ r))
  }
}

def sequence[T, CC[U] <: IterableOps[U, CC, CC[U]]](xs: CC[Option[T]]): Option[CC[T]] =
  sequence(xs.map(_.toRight(()))).toOption

def sequence[T, L, CC[U] <: IterableOps[U, CC, CC[U]]](xs: CC[Either[L, T]]): Either[CC[L], CC[T]] = {
  xs.partitionMap(identity) match {
    case (left, _) if left.nonEmpty => Left(left)
    case (_, right) => Right(right)
  }
}

extension [A, CC[X] <: IterableOps[X, CC, CC[X]], C <: CC[A]](coll: IterableOps[A, CC, C])

  /**
   * Performs a left fold on the given iterable, using an *empty collection* as the base case for the fold.
   * The type of the empty collection is inferred from the type of the original iterable.
   */
  def foldLeft0(f: (A, A) => A)(implicit factory: Factory[Nothing, A]): A =
    coll.foldLeft[A](factory.newBuilder.result)(f)
