package astnodes.stmt

import astnodes.exp.Expr
import astnodes.pred.Pred
import java.util

class Assert(pc: String, var pred: Pred) extends Stmt(pc) {
  override def replace(oldExp: Expr, newExp: Expr) = {
// TODO
  }
  override def getChildren = null
}
