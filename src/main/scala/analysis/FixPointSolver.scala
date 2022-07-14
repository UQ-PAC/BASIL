package analysis

trait LatticeSolver[T]:
  val lattice: Lattice[T]
  def analyze(): T

trait NaiveFixedPointSolver[T] extends LatticeSolver[T]:
  def fun(x: T): T

  def analyze(): T = {
    var x = lattice.bottom
    var t = x
    while
      t = x
      x = fun(x)
      x != t
    do ()
    x
  }
