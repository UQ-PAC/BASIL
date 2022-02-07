package astnodes.stmt

import astnodes.Label
import astnodes.exp.Expr
import astnodes.exp.`var`.Var

import java.util

/** Generic instruction fact
  */
// TODO make var
trait Stmt(var label: Label) {
  // main
  def subst(v: Var, w: Var): Stmt
  def toBoogieString = toString

  // TODO redefine this without side effects
  def copy(label: Label) = {
    this.label = label
    this
  }
}
