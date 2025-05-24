package util.functional

import collection.immutable.LinearSeq
import collection.{SeqOps, IterableOps}

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

def sequence[T](xs: List[Option[T]]): Option[List[T]] = {
  xs.foldRight[Option[List[T]]](Some(Nil)) {
    case (Some(x), Some(xs)) => Some(x :: xs)
    case _ => None
  }
}
