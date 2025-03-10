package analysis.solvers

import scala.collection.mutable

class OffsetUnionFindSolver[T] extends UnionFindSolver[T] {

  private val offsets = mutable.Map[Term[T], Int]()

  def size: Int = offsets.size

  def compress(remove: Term[T] => Boolean): Unit = {
    val removees = parent.keys.filter(remove)
    removees.foreach(parent.remove)
    removees.foreach(offsets.remove)
  }

  override def unify(t1: Term[T], t2: Term[T]): Unit =
    unify(t1, t2, 0)

  // offset is the offset at which
  def unify(t1: Term[T], t2: Term[T], offset: Int): Unit = {
    mkSet(t1)
    mkSet(t2)
    val (rep1, _) = findWithOffset(t1)
    val (rep2, _) = findWithOffset(t2)

    if (rep1 != rep2) {
      parent += t1 -> t2
      offsets += t1 -> offset
    }
  }

  def findWithOffset(t: Term[T]): (Term[T], Int) = {
    mkSet(t)
    if (parent(t) != t)
      val (par, offset) = findWithOffset(parent(t))
      parent += t -> par
      offsets += t -> (offsets(t) + offset)

    (parent(t), offsets(t))
  }

  /** Creates an equivalence class for the term `t`, if it does not exists already.
   */
  def mkSet(t: Term[T]): Unit = {
    if (!parent.contains(t))
      parent += (t -> t)
      offsets += (t -> 0)
  }

}
