package astnodes.exp
import astnodes.exp.variable.*

/** This is only used by the parser
  *
  * @param loc
  * @param expr
  */
case class MemStore(loc: Expr, expr: Expr, override val size: Some[Int]) extends Expr {
  override def vars: List[Register] = ???
  override def subst(v: Variable, w: Variable): Expr = ???
  override def fold(old: Expr, sub: Expr): Expr = this
}
