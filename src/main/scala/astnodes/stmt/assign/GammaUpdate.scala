package astnodes.stmt.assign

import astnodes.exp.{Expr, `var`}
import astnodes.pred.MemLoad
import astnodes.pred.Pred
import astnodes.pred.Var
import astnodes.stmt.Stmt

import scala.jdk.CollectionConverters.*
import java.util

case class GammaUpdate (val lhs: Var | MemLoad, pred: Pred) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toBoogieString} := ${pred.toBoogieString};"

  // TODO
  override def subst(v: `var`.Var, w: `var`.Var): Stmt = this
}
