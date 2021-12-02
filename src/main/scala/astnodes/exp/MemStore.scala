package astnodes.exp
import java.util
import java.util.Collections

/**
  * This is only used by the parser
  * @param loc
  * @param expr
  */
case class MemStore(loc: Expr, expr: Expr) extends Expr {
  override def vars: List[Var] = ??? // loc.vars ++ expr.vars
  override def getChildren: util.List[Expr] = ??? // Collections.singletonList(expr) // TODO
  override def replace(oldExp: Expr, newExp: Expr): Unit = ???
}
