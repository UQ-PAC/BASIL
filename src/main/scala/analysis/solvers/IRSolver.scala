package analysis.solvers

import analysis.*
import ir.*

type IRNode = Procedure | Block | Command

trait IRWorklistFixpointSolver[T, L <: Lattice[T]] extends WorklistFixpointSolver[IRNode, T, L] {

  def process(b: Block): Unit = {
    val xb = x(b)
    val yb = funsub(b, x)
    if (yb != xb) {
      x = x + (b -> yb)
      var prev = xb
      for (s <- b.statements) {
        val xs = x(s)
        val ys = transfer(s, prev)
        prev = xs
      }
      val j = b.jump
      val xj = x(j)
      val yj = transfer(j, prev)
      add(b.successorsIntra)
    }
  }

  def join(b: Block, o: Map[IRNode, T]): T = {
    val states = b.predecessorsIntra.map(o(_))
    if (states.size > 1) {
      states.foldLeft(lattice.sublattice.bottom)((acc, pred) => lattice.sublattice.lub(acc, pred))
    } else {
      states.head
    }
  }

}

trait IRPushDownWorklistFixpointSolver[T, L <: Lattice[T]] extends PushDownWorklistFixpointSolver[IRNode, T, L] {

  override def propagate(y: T, b: Block): Unit = {
    val xb = x(b)
    val t = lattice.sublattice.lub(xb, y)
    if (t != xb) {
      add(b)
      x += b -> t
    }
  }

  override def process(b: Block): Unit = {
    val xb = x(b)
    var y = transfer(b, xb)
    for (s <- b.statements) {
      val xs = x(s)
      val t = lattice.sublattice.lub(xb, y)
      x += s -> t
      y = transfer(s, xs)
    }
    val j = b.jump
    val xj = x(j)
    val t = lattice.sublattice.lub(xj, y)
    x += j -> t
    y = transfer(j, xj)
    for (succ <- b.successorsIntra) {
      propagate(y, succ)
    }
  }
}

