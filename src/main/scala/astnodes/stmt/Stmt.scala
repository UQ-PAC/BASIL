package astnodes.stmt

import astnodes.Label
import astnodes.exp.Expr
import astnodes.exp.`var`.Var

import java.util

/** Generic instruction fact
  */
// TODO convert to trait when everything is in scala
trait Stmt(val pc: String) {
  private var label = new Label(pc)
  def getLabel = label
  def setLabel(label: Label) = this.label = label

  def subst(v: Expr, w: Expr): Stmt
  
  def toBoogieString = toString
}
