package facts.exp

import facts.Fact
import java.util

/** Abstract class of an expression fact
  */
trait Expr extends Fact {
  def vars: List[Var]
}

/** Define custom methods for List[Expr]
 */
extension (exprs: List[Expr]) {
  def conjunct: Expr = exprs match {
    case expr :: rest => BinOp("&&", expr, rest.conjunct)
    case expr :: Nil => expr
    case Nil => Bool.True
  }
}

