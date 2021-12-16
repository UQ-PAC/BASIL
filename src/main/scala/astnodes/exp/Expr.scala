package astnodes.exp

import java.util

/** Abstract class of an expression fact
  */
trait Expr {
  def vars: List[Var]
  
  // TODO can we remove these
  def getChildren: util.List[Expr]
  def replace(oldExpr: Expr, newExpr: Expr): Unit
  
  def toBoogieString = toString
  
  def contains(expr: Expr): Boolean = {
    if (getChildren.size() > 0) {
      getChildren.forEach(child => {
        return child.contains(expr)
      })
    } else {
      if (expr == this) {
        return true
      }
    }
    
    return false
  }
}

