package facts.stmt.Assign

import astnodes.exp.Expr
import astnodes.exp.Var
import facts.stmt.Stmt

// TODO check if we need override pc
/** Load fact
  */
case class RegisterAssign(override val pc: String, val lhsExp: Var, val expr: Expr) extends Assign(pc, lhsExp, expr) {}
