package analysis.solvers

import analysis.data_structure_analysis.Node
import scala.collection.mutable

class DSAUnionFindSolver extends UnionFindSolver[UniTerm] {
  override val parent = mutable.Map[Term[UniTerm], Term[UniTerm]]()
  private val offsets = mutable.Map[DSAUniTerm, BigInt]()

  override def unify(t1: Term[UniTerm], t2: Term[UniTerm]): Unit =
    unify(t1.asInstanceOf[DSAUniTerm], t2.asInstanceOf[DSAUniTerm], 0)

  // offset is the offset at which
  def unify(t1: DSAUniTerm, t2: DSAUniTerm, offset: BigInt): Unit = {
    mkSet(t1)
    mkSet(t2)
    val (rep1, _) = findWithOffset(t1)
    val (rep2, _) = findWithOffset(t2)

    if (rep1 != rep2) {

      /** Perform the union of the equivalence classes of `t1` and `t2`, such that `t2` becomes the new canonical
        * element. We assume `t1` and `t2` to be distinct canonical elements. This implementation does not use
        * [[https://en.wikipedia.org/wiki/Disjoint-set_data_structure union-by-rank]].
        */
      parent += t1 -> t2
      offsets += t1 -> offset
    }
  }

  def findWithOffset(t: DSAUniTerm): (DSAUniTerm, BigInt) = {
    mkSet(t)
    if (parent(t) != t)
      val (par, offset) = findWithOffset(parent(t).asInstanceOf[DSAUniTerm])
      parent += t -> par
      offsets += t -> (offsets(t) + offset)

    (parent(t).asInstanceOf[DSAUniTerm], offsets(t))
  }

  /** Creates an equivalence class for the term `t`, if it does not exists already.
    */
  private def mkSet(t: DSAUniTerm): Unit = {
    if (!parent.contains(t))
      parent += (t -> t)
      offsets += (t -> 0)
  }

}

/** Terms used in unification.
  */
sealed trait UniTerm

/** A term variable in the solver
  */
case class DSAUniTerm(node: Node) extends Var[UniTerm] {

  override def toString: String = s"Term{$node}"
}
