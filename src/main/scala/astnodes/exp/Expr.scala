package astnodes.exp

import astnodes.exp.`var`.{MemLoad, Register, Var}

import java.util

/** Expression
 */
trait Expr {
  /*  All of the variables in a given expression */
  def vars: List[Var] 

  /* Substitute a given variable for another variable */
  def subst(v: Var, w: Var): Expr
  def toBoogieString = toString

  /* 
   * The size of output of the given expression.
   *
   * Note: for binary operators in some cases the input and output sizes will not match.
   */
  def size: Option[Int]
  
  // def contains(expr: Expr): Boolean = {
  //   if (vars.size > 0) {
  //     vars.foreach(child => {
  //       return child.contains(expr)
  //     })
  //   } else {
  //     if (expr == this) {
  //       return true
  //     }
  //   }
    
  //   return false
  // }
}

