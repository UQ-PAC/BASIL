package ir.shrinking

import scala.annotation.tailrec

import ir.Program


def roundRobin[T](its: Iterator[Iterator[T]]): Iterator[T] = {
  if (its.isEmpty) {
    return Iterator()
  }

  val (it1, it2) = its.collect {
    case it if it.hasNext => it.splitAt(1)
  }.duplicate

  val heads = it1.map(_._1)
  val tails = it2.map(_._2)

  heads.flatten ++ roundRobin(tails)
}


/**
 * This is HARD.
 *
 * It's a very recursive problem and there's a lot of choices to make
 * at every step of the recursion which, I assume, will have inscrutable
 * effects on the effectiveness of the shrinking.
 */

class ShrinkBasil[T](val width: Int, val shrinkers: Iterable[Shrinker[T]]) {

  @tailrec
  final def shrink(predicate: T => Boolean)(programs: Iterable[T], depth: Int): List[T] = {
    println("" + depth + " " + programs)
    if (depth >= 100)
      throw new Exception("fdjsiaofdsa")

    val shrunk = for {
      p <- programs.iterator
      shrinker <- shrinkers.iterator
    } yield (shrinker.shrink(p).iterator.filter(predicate))

    val newPrograms = roundRobin(shrunk).take(width)

    if (newPrograms.nonEmpty) {
      shrink(predicate)((newPrograms ++: programs).take(width), depth + 1)
    } else {
      programs.toList
    }
  }
}
