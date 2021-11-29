package facts.exp

import facts.Fact
import java.util

/** Abstract class of an expression fact
  */
trait Expr extends Fact {
  def vars: List[Var]
}
