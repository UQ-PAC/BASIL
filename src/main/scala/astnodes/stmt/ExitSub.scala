package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.Var

// TODO do we need this?
class ExitSub(override val pc: String) extends Stmt(pc) {
  override def toString = "return;"

  override def subst(v: Expr, w: Expr): Stmt = this
}
