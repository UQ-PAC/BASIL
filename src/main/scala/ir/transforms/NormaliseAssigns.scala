package ir.transforms

import ir.*
import ir.cilvisitor.*

/** Normalise [[ir.SimulAssign]] statements with only one assignment
 *  into a [[ir.LocalAssign]].
 */
class NormaliseAssigns extends CILVisitor {

  override def vstmt(s: Statement) = s match {
    case SimulAssign(Seq(l -> r), lbl) => ChangeTo(List(LocalAssign(l, r, lbl)))
    case _ => SkipChildren()
  }

  def apply(x: Program) = visit_prog(this, x)
}
