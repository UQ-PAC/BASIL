package util.functional

/**
 * Combines two maps, keeping keys which appear in at least one map.
 * If both maps have a value for a key, the two values are merged using the given function.
 */
def unionWith[K, V](a: Iterable[(K, V)], b: Iterable[(K, V)], f: (V, V) => V): Map[K, V] = {
  Iterable.concat(a, b).groupMapReduce(_(0))(_(1))(f)
  // https://stackoverflow.com/a/54616613
}

/**
 * Unapply matcher for [[scala.collection.immutable.Map]].
 * The match succeeds if the given map has exactly the keys specified.
 * If successful, the values corresponding to the keys will be returned as a list
 * in the order specified by the constructor.
 *
 * The inner class [[MapOf.WithOptional]] can be used to match certain keys which
 * are allowed to be missing.
 */
case class MapOf[K](keys: K*) {
  mapof =>

  type Map[K, V] = scala.collection.immutable.Map[K, V]
  type List[T] = scala.List[T]

  def unapply[V](x: Map[K, V]): Option[List[V]] =
    if (x.keySet.forall(keys.contains)) then {
      val values = keys.map(x.get(_)).toList
      util.functional.sequence(values)
    } else {
      None
    }

  case class WithOptional(optionalKeys: K*) {
    def unapply[V](x: Map[K, V]): Option[(List[V], List[Option[V]])] =
      val optionals = optionalKeys.map(x.get(_)).toList
      (x -- optionalKeys) match {
        case `mapof`(vals) => Some((vals, optionals))
        case _ => None
      }
  }
}
