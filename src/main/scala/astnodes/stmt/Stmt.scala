package astnodes.stmt

import astnodes.exp.Expr
import astnodes.exp.variable.Variable

/** Generic instruction fact
  */
trait Stmt(val pc: String, val labelVisible: Boolean = false) {
  def subst(v: Variable, w: Variable): Stmt

  def toBoogieString: String = toString

  def labelString: String = if (labelVisible) {
    "label" + pc + ": "
  } else {
    ""
  }

  //def withVisibleLabel: Stmt
}