package util.functional
import scala.collection.mutable

case class Stats(hits: Long, misses: Long) {
  def hitRate: Double = hits.toDouble / (misses + hits)
}

def memoised[T, P](f: T => P, withStats: Boolean = false): (T => P, () => Stats) = {
  var hits: Long = 0
  var misses: Long = 0
  val cache = mutable.Map[T, P]()
  def memoFn(arg: T): P = {
    if (cache.contains(arg)) then {
      hits += 1
      cache(arg)
    } else {
      misses += 1
      val r = f(arg)
      cache(arg) = r
      r
    }
  }
  (memoFn, () => Stats(hits, misses))
}
