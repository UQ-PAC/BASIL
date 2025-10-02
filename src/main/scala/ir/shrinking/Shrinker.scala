package ir.shrinking

trait Shrinker[A] {
  shrinker =>
  def shrink(p: A): IterableOnce[A]

  final def ifChanged(metric: A => Any) = new Shrinker[A] {
    def shrink(p: A) = {
      val original = metric(p)
      shrinker.shrink(p).iterator.filter(metric(_) != original)
    }
  }

  final def tap(f: A => Unit) = new Shrinker[A] {
    def shrink(p: A) = shrinker.shrink(p).iterator.map { x =>
      f(x)
      x
    }
  }

  final def map(f: A => A) = new Shrinker[A] {
    def shrink(p: A) = shrinker.shrink(p).iterator.map(f)
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
