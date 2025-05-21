package util.functional

/**
 * Combines two maps, keeping keys which appear in at least one map.
 * If both maps have a value for a key, the two values are merged using the given function.
 */
def unionWith[K, V](a: Iterable[(K, V)], b: Iterable[(K, V)], f: (V, V) => V): Map[K, V] = {
  (a.toList ++ b.toList).groupMapReduce(_(0))(_(1))(f)
  // https://stackoverflow.com/a/54616613
}
