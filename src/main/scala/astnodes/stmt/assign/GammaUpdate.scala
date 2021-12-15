package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.pred.MemLoad
import astnodes.pred.Pred
import astnodes.pred.Var
import astnodes.stmt.Stmt

import java.util

case class GammaUpdate (lhs: Var | MemLoad, pred: Pred) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toBoogieString} := ${pred.toBoogieString};"

  // These can be ignored as they are only ever used before GammaUpdates are generated
  override def getChildren: util.List[Expr] = ???
  override def replace(oldExpr: Expr, newExpr: Expr): Unit = ???
}
