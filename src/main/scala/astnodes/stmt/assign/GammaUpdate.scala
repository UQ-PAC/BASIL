package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.variable.Variable
import astnodes.sec.{Sec, SecMemLoad, SecVar}
import astnodes.stmt.Stmt

case class GammaUpdate (lhs: SecVar | SecMemLoad, sec: Sec) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toString} := ${sec.toString};"

  // TODO
  override def subst(v: Variable, w: Variable): Stmt = this
}
