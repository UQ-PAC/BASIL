package astnodes.stmt.assign

import astnodes.exp.{BinOp, BinOperator, Expr, Extract, Literal, MemLoad, Var}

/** Store fact
  */
case class MemAssign(override val pc: String, val memExp: MemLoad, val rhsExp: Expr) extends Assign (pc, memExp, rhsExp)  {

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String =
    (0 to lhs.size.get/8 - 1).map(n => {
      val newMemExp = BinOp(BinOperator.Addition, memExp.exp, Literal(n.toString))
      s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhsExp).toBoogieString}"
    }).mkString("; ") + s";     // $pc"


}
