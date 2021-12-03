package facts.stmt.Assign

import astnodes.exp.{BinOp, Var, Expr, MemLoad}

/** Store fact
  */
case class MemAssign(override val pc: String, val memExp: MemLoad, val rhsExp: Expr) extends Assign (pc, memExp, rhsExp)  {
}
