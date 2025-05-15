package util

/**
 * A mixin for **case classes** to cache the hashCode the first time it is
 * computed. This can yield speedups for case classes which contain complex
 * structures (e.g., string list) and/or values which are frequently searched
 * in a hashed data structure.
 *
 * WARNING: this must /not/ be used for classes which have mutable fields.
 *          doing so would lead to inconsistencies between equals and hashcode,
 *          causing unpredictable (read: incorrect) behaviour in hashmap/set.
 */
trait CachedHashCode {
  this: Product =>

  // this is the default implementation of hashCode for case classes.
  // we need to call it explicitly because Scala will not generate a hashCode
  // if we have provided one ourself.
  private lazy val hash = scala.runtime.ScalaRunTime._hashCode(this)

  override def hashCode = hash
}
