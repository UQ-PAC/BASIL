package ir.shrinking

import scala.annotation.tailrec

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

  final def shrink(predicate: T => Boolean)(program: T): List[T] =
    shrink(predicate)(List(program), Vector(), 0)

  /**
   * This does a kind of stack-constrained DFS traversal.
   *
   * We need to keep the suffix separate because we do not want to repeatedly
   * shrink values once they are added to the suffix.
   */
  @tailrec
  final def shrink(predicate: T => Boolean)(programs: List[T], suffix: Vector[T], depth: Int): List[T] = {
    // println("" + depth + " " + programs)
    if (depth >= 100)
      throw new Exception("fdjsiaofdsa")

    val shrunk = for {
      p <- programs.iterator
      shrinker <- shrinkers.iterator
    } yield (shrinker.shrink(p).iterator.filter(predicate))

    val newPrograms = roundRobin(shrunk).take(width).toList
    val newSuffix = (newPrograms ++: suffix).take(width)

    if (newPrograms.nonEmpty) {
      shrink(predicate)(newPrograms, newSuffix, depth + 1)
    } else {
      newSuffix.toList
    }
  }
}
