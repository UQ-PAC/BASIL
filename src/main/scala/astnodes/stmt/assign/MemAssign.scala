package astnodes.stmt.assign

<<<<<<< HEAD
import astnodes.exp.`var`.{MemLoad, Var}
import astnodes.exp.{BinOp, BinOperator, Expr, Extract, Literal}
import astnodes.stmt.Stmt

/** Store fact
  */
case class MemAssign(override val pc: String, override val lhs: MemLoad, override val rhs: Expr) extends Assign (pc, lhs, rhs) with Stmt(pc)  {
=======
import astnodes.exp.{BinOp, BinOperator, Expr, Extract, Literal, MemLoad, Var}

/** Store fact
  */
case class MemAssign(override val pc: String, val memExp: MemLoad, val rhsExp: Expr) extends Assign (pc, memExp, rhsExp)  {
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String =
    (0 to lhs.size.get/8 - 1).map(n => {
<<<<<<< HEAD
      val newMemExp = BinOp(BinOperator.Addition, lhs.exp, Literal(n.toString))
      s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhs).toBoogieString}"
    }).mkString("; ") + s";     // $pc"

=======
      val newMemExp = BinOp(BinOperator.Addition, memExp.exp, Literal(n.toString))
      s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhsExp).toBoogieString}"
    }).mkString("; ") + s";     // $pc"


>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
}
