package astnodes.exp.`var`

import astnodes.exp.Expr

trait Var extends Expr {
  override def subst(v: Var, w: Var): Var = if (v == this) w else this
}
