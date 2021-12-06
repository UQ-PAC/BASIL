package astnodes.exp

import java.util
import java.util.Objects
import astnodes.pred

/** Variable fact
  */
class Var(var name: String) extends Expr {
  def getName = name
  override def toString = String.format("%s", name)
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}

  override def vars: List[Var] = List(this)
  
  def toGamma = pred.Var(name, true, false)
}
