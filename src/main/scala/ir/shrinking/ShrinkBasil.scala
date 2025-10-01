package ir.shrinking

import ir.Program

/**
 * This is HARD.
 *
 * It's a very recursive problem and there's a lot of choices to make
 * at every step of the recursion which, I assume, will have inscrutable
 * effects on the effectiveness of the shrinking.
 */

class ShrinkBasil(val width: Int, val shrinkers: Iterable[Shrinker[Program]]) {

  final def shrink(predicate: Program => Boolean)(p: Program): Iterable[Program] = {

    val shrunk =
      shrinkers.iterator.map(s => s.shrink(p).iterator.filter(predicate).toList).transpose.toList
        // .transpose
        // .flatten
        // .take(width)

    (shrink(predicate)(shrunk) ++ programs).take(width)
  }
}
