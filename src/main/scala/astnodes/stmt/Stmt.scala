package astnodes.stmt

import astnodes.Label
import astnodes.exp.Expr

import java.util

/** Generic instruction fact
  */
// TODO convert to trait when everything is in scala
abstract class Stmt(val pc: String) {
  private var label = new Label(pc)
  def getLabel = label
  def setLabel(label: Label) = this.label = label

  def getChildren: util.List[Expr]
  def replace(oldExpr: Expr, newExpr: Expr): Unit
  
  def toBoogieString = toString
}
