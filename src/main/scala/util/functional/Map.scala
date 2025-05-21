package util.functional

/**
 * Combines two maps, keeping keys which appear in at least one map.
 * If both maps have a value for a key, the two values are merged using the given function.
 */
def unionWith[K, V](a: Map[K, V], b: Map[K, V], f: (V, V) => V): Map[K, V] = {
  (a.keySet ++ b.keySet)
    .map(k =>
      val v = (a.get(k), b.get(k)) match {
        case (Some(x), Some(y)) => f(x, y)
        case (Some(x), _) => x
        case (_, Some(x)) => x
        case (None, None) => throw Exception("unreachable in unionWith. key must be in at least one map.")
      }
      k -> v
    )
    .toMap
}
