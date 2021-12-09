package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.Var
import astnodes.stmt.Stmt

// TODO check if we need override pc
/** Load fact
  */
case class RegisterAssign(override val pc: String, val lhsExp: Var, val expr: Expr) extends Assign(pc, lhsExp, expr) {
  override def getLhs: Var = lhsExp

}
