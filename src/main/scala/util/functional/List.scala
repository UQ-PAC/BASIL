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

def sequence2[T](xs: List[Vector[T]]): Vector[List[T]] = {
  xs.foldRight[Vector[List[T]]](Vector(Nil)) {
    case (ys, rest) => rest.flatMap(r => ys.map(_ +: r))
  }
}

def sequence3[T, CC[U] <: IterableOps[U, CC, CC[U]], DD[V] <: IterableOps[V, DD, DD[V]]](ddfactory: Factory[CC[T], DD[CC[T]]])(xs: CC[DD[T]]): DD[CC[T]] = {
  def DD0(): DD[CC[T]] = ddfactory.newBuilder.result
  def DD(x: CC[T]): DD[CC[T]] = DD0() ++ Iterable(x)
  def CC0(): CC[T] = xs.iterableFactory.empty
  def CC(x: T): CC[T] = CC0() ++ Iterable(x)

  val base: DD[CC[T]] = DD(CC0())
  xs.foldRight(base) {
    case (ys, rest) => rest.flatMap((r: CC[T]) => ys.map((y: T) => CC(y) ++ r))
  }
}

case class sequence4[DD[V] <: IterableOps[V, DD, DD[V]]](ddfactory: Factory[?, DD[Any]]) {
  private def magic(x: Any): Nothing = throw new RuntimeException

  def apply[T, CC[U] <: IterableOps[U, CC, CC[U]]](xs: CC[DD[T]]): DD[CC[T]] = {
    def DD0(): DD[CC[T]] = ddfactory.newBuilder.result.map(magic)
    def DD(x: CC[T]): DD[CC[T]] = DD0() ++ Iterable(x)
    def CC0(): CC[T] = xs.iterableFactory.empty
    def CC(x: T): CC[T] = CC0() ++ Iterable(x)

    val base: DD[CC[T]] = DD(CC0())
    xs.foldRight(base) {
      case (ys, rest) => rest.flatMap((r: CC[T]) => ys.map((y: T) => CC(y) ++ r))
    }
  }
}

def sequence5[T, CC[U] <: IterableOps[U, CC, CC[U]], DD[V] <: IterableOps[V, DD, DD[V]]](dd0: Factory[Nothing, DD[Nothing]])(xs: CC[DD[T]]): DD[CC[T]] = {
  def magic[A](x: Nothing): A = throw new RuntimeException
  def DD0(): DD[CC[T]] = dd0.newBuilder.result.map(magic)
  def DD(x: CC[T]): DD[CC[T]] = DD0().iterableFactory.newBuilder.addOne(x).result
  def CC0(): CC[T] = xs.iterableFactory.empty
  def CC(x: T): CC[T] = CC0().iterableFactory.newBuilder.addOne(x).result

  val base: DD[CC[T]] = DD(CC0())
  xs.foldRight(base) {
    case (ys, rest) => rest.flatMap((r: CC[T]) => ys.map((y: T) => CC(y) ++ r))
  }
}

def sequence5[T, CC[U] <: IterableOps[U, CC, CC[U]]](dd0: Option.type)(xs: CC[Option[T]]): Option[CC[T]] = {
  sequence5(List)(xs.map(_.toList)).headOption
}


// def sequence4[T, CC[U] <: Seq[U], DD[V] <: Seq[V]](xs: CC[DD[T]]): DD[CC[T]] = {
//   def magic(x: Any): Nothing = throw new RuntimeException
//   def dd(): DD[CC[T]] = DD()
//   def dd(x: CC[T]): DD[CC[T]] = DD(x)
//   def cc(): CC[T] = CC()
//   def cc(x: T): CC[T] = CC(x)
//
//   val base: DD[CC[T]] = dd(cc())
//   xs.foldRight(base) {
//     case (ys, rest) => rest.flatMap((r: CC[T]) => ys.map((y: T) => y +: r))
//   }
// }
