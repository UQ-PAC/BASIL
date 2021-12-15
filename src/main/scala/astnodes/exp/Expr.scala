package astnodes.exp

import java.util

/** Abstract class of an expression fact
  */
trait Expr {
  def vars: List[Var | MemLoad] // TODO rework this
  
  // TODO can we remove these
  def getChildren: util.List[Expr]
  def replace(oldExpr: Expr, newExpr: Expr): Unit

  def toBoogieString = toString
  
  def size: Option[Int]
}

