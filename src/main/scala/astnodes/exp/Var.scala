package astnodes.exp

import java.util
import java.util.Objects
import astnodes.pred

/** Variable fact
  */
case class Var(name: String) extends Expr {
  override def toString = name
  override def getChildren = new util.ArrayList[Expr]
  override def replace(oldExp: Expr, newExp: Expr) = {}

  override def vars: List[Var] = List(this)
  
  def toGamma = pred.Var(name, true)
}
