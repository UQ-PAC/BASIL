package facts.exp

import java.util
import java.util.Objects

/** Variable fact
  */
class Var(var name: String) extends Expr {
  def getName = name
  override def toString = String.format("%s", name)
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}

  override def vars: List[Var] = List(this)
  
  def toGamma = Var("Gamma_" + name)
}
