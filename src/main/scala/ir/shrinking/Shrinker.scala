package ir.shrinking

trait Shrinker[A] {
  shrinker =>
  def shrink(p: A): IterableOnce[A]

  final def ifChanged[K](metric: A => K) = new Shrinker[A] {
    def shrink(p: A) = {
      val original = metric(p)
      shrinker.shrink(p).iterator.filter(metric(_) != original)
    }
  }
}

object Shrinker {
  def apply[A](f: A => IterableOnce[A]) = new Shrinker[A] {
    def shrink(p: A) = f(p)
  }

  def one[A](f: A => A) = new Shrinker[A] {
    def shrink(p: A) = List(f(p))
  }

  def option[A](f: A => Option[A]) = new Shrinker[A] {
    def shrink(p: A) = f(p)
  }
}
