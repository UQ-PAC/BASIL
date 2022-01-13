package astnodes.exp.`var`

import astnodes.exp.Expr
import astnodes.pred

/** Variable
 *
 *  Variables can be registers (e.g. R1, SP, #31) or loads from memory (e.g. mem[10])
 */
trait Var extends Expr {
  override def subst(v: Expr, w: Expr): Expr = if (v == this) w else this
  def toGamma: pred.Var | pred.MemLoad
}
