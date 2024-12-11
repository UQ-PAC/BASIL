package analysis.solvers

import analysis.data_structure_analysis.CoolNode

import scala.collection.mutable

class CoolDSAUnionFindSolver extends UnionFindSolver[CoolUniTerm] {
  private val parent = mutable.Map[DSACoolUniTerm, DSACoolUniTerm]()
  private val offsets = mutable.Map[DSACoolUniTerm, Int]()

  override def unify(t1: Term[CoolUniTerm], t2: Term[CoolUniTerm]): Unit =
    unify(t1.asInstanceOf[DSACoolUniTerm], t2.asInstanceOf[DSACoolUniTerm], 0)

  // offset is the offset at which
  def unify(t1: DSACoolUniTerm, t2: DSACoolUniTerm, offset: Int): Unit = {
    mkSet(t1)
    mkSet(t2)
    val (rep1, _) = findWithOffset(t1)
    val (rep2, _) = findWithOffset(t2)

    if (rep1 != rep2) {
      /** Perform the union of the equivalence classes of `t1` and `t2`, such that `t2` becomes the new canonical element.
       * We assume `t1` and `t2` to be distinct canonical elements. This implementation does not use
       * [[https://en.wikipedia.org/wiki/Disjoint-set_data_structure union-by-rank]].
       */
      parent += t1 -> t2
      offsets += t1 -> offset
    }
  }

  def findWithOffset(t: DSACoolUniTerm): (DSACoolUniTerm, Int) = {
    mkSet(t)
    if (parent(t) != t)
      val (par, offset) = findWithOffset(parent(t))
      parent += t -> par
      offsets += t -> (offsets(t) + offset)

    (parent(t), offsets(t))
  }


  /** Creates an equivalence class for the term `t`, if it does not exists already.
   */
  private def mkSet(t: DSACoolUniTerm): Unit = {
    if (!parent.contains(t))
      parent += (t -> t)
      offsets += (t -> 0)
  }

}

/** Terms used in unification.
 */
sealed trait CoolUniTerm

/** A term variable in the solver
 */
case class DSACoolUniTerm(node: CoolNode) extends Var[CoolUniTerm] {

  override def toString: String = s"Term{$node}"
}

