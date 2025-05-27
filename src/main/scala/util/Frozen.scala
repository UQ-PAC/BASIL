package util

case class Freeze[T](private var x: T) {
  var frozen = false

  def update(f: T => T): Unit = x = f(x)

  def freeze() = {
    require(!frozen, "attempt to freeze already-frozen value")
    frozen = true
  }

  def get = {
    require(frozen, "attempt to get not-yet-ready freeze value")
    x
  }
}
