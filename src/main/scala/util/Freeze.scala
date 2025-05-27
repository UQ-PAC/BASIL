package util

/**
 * A Freeze value is one which may be "frozen". It goes through two
 * stages:
 *
 * 1. When created from an initial value, a freeze value is unfrozen.
 *    In this stage, it may be modified by an update function T => T but
 *    it may not be read.
 * 2. The Freeze value is *frozen* with the freeze() method.
 * 3. Once frozen, the value can be read using .get, but it can no longer
 *    be modified.
 *
 * This class represents a value which may go through a long period
 * of initialisation / setup, but after that setup it becomes effectively
 * immutable. The use of separate update/get methods for each stage
 * makes it clear which stage it is currently in.
 *
 */
case class Freeze[T](private var x: T) {
  var frozen = false

  def update(f: T => T): Unit = {
    require(!frozen, "attempt to update already-frozen value")
    x = f(x)
  }

  def freeze() = {
    require(!frozen, "attempt to freeze already-frozen value")
    frozen = true
  }

  def get = {
    require(frozen, "attempt to get not-yet-ready freeze value")
    x
  }
}
