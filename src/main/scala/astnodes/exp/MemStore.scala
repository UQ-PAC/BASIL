package astnodes.exp
import astnodes.exp.`var`.{Register, Var}

import java.util
import java.util.Collections

/** This is only used by the parser
  * @param loc
  * @param expr
  */
case class MemStore(loc: Expr, expr: Expr, override val size: Some[Int]) extends Expr {
  override def vars: List[Register] = ???
  override def subst(v: Var, w: Var): Expr = ???
}
