package ir.shrinking

trait Shrinker[A] {
  def shrink(p: A): Iterable[A]
}
