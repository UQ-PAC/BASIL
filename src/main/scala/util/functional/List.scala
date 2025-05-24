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


/**
 * Converts a list of options to an option of list. Returns Some if
 * all the options are Some, and returns None if any of the options
 * were None. This can be thought of as a notion of "all succeeding".
 *
 * Here, "list" may be any Seq type.
 */
def sequence[T, CC[U] <: SeqOps[U, CC, CC[U]]](xs: CC[Option[T]]): Option[CC[T]] = {
  def magic(x: Any): Nothing = throw new RuntimeException
  xs.foldRight[Option[CC[T]]](Some(xs.empty.map(magic))) {
    case (Some(x), Some(xs)) => Some(x +: xs)
    case _ => None
  }
}
