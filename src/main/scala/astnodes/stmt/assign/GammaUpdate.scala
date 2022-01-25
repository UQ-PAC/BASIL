package astnodes.stmt.assign

import astnodes.exp.{Expr, `var`}
import astnodes.stmt.Stmt

import scala.jdk.CollectionConverters.*
import java.util
import astnodes.Label

import astnodes.sec.{Sec, SecVar, SecMemLoad}

case class GammaUpdate (val lhs: SecVar | SecMemLoad, sec: Sec) extends Stmt(Label("NA")) {

  override def toBoogieString: String = s"${lhs.toString} := ${sec.toString};"

  // TODO
  override def subst(v: `var`.Var, w: `var`.Var): Stmt = this
}
