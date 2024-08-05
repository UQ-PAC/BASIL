package analysis.solvers

import scala.collection.mutable

class DSAUnionFindSolver[A] {


  val parent = mutable.Map[Term[A], Term[A]]()
  val offsets = mutable.Map[Term[A], BigInt]()

  def unify(t1: Term[A], t2: Term[A], offset: BigInt): Unit = {
    mkSet(t1)
    mkSet(t2)
    val rep1 = find(t1)._1
    val rep2 = find(t2)._1

    if (rep1 == rep2) return

      (rep1, rep2) match {
        case (v1: Var[A], v2: Var[A]) =>
          mkUnion(v1, v2, offset)
        case (v1: Var[A], t2: Term[A]) =>
          mkUnion(v1, t2, offset)
        case (t1: Term[A], v2: Var[A]) =>
          mkUnion(v2, t1, offset)
        case (f1: Cons[A], f2: Cons[A]) if f1.doMatch(f2) =>
          mkUnion(f1, f2, offset)
          f1.args.zip(f2.args).foreach { case (a1, a2) =>
            unify(a1, a2, offset)
          }
        case (x, y) =>
          throw new UnificationFailure(s"Cannot unify $t1 and $t2 (with representatives $x and $y)")
      }
  }

  def find(t: Term[A]): (Term[A], BigInt) = {
    mkSet(t)
    if (parent(t) != t)
      val (par, offset) = find(parent(t))
      parent += t -> par
      offsets += t -> offsets(t).+(offset)

    (parent(t), offsets(t))
  }

  /** Perform the union of the equivalence classes of `t1` and `t2`, such that `t2` becomes the new canonical element.
   * We assume `t1` and `t2` to be distinct canonical elements. This implementation does not use
   * [[https://en.wikipedia.org/wiki/Disjoint-set_data_structure union-by-rank]].
   */
  def mkUnion(t1: Term[A], t2: Term[A], offset: BigInt): Unit =
    parent += t1 -> t2
    offsets += t1 -> offset

  /** Creates an equivalence class for the term `t`, if it does not exists already.
   */
  def mkSet(t: Term[A]): Unit =
    if (!parent.contains(t))
      parent += (t -> t)
      offsets += (t -> 0)

  /** Returns the solution of the solver. Note that the terms in the solution have not yet been closed, i.e. they may
   * contain constraint variables.
   *
   * @return
   * a map associating to each variable the representative of its equivalence class
   */
  def solution(): Map[Var[A], Term[A]] =
    // for each constraint variable, find its canonical representative (using the variable itself as default)
    parent.keys.collect { case v: Var[A] => (v, find(v)._1) }.toMap.withDefault(v => v)


}
