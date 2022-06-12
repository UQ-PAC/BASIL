package astnodes.exp.variable

import astnodes.exp.Expr
import astnodes.sec

/** Variable
 *
 *  Variables can be registers (e.g. R1, SP, #31) or loads from memory (e.g. mem[10])
 */
trait Variable extends Expr {
  override def subst(v: Variable, w: Variable): Variable = if (v == this) w else this
  def toGamma: sec.SecVar | sec.SecMemLoad
}

