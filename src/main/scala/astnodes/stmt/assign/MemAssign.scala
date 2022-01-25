package astnodes.stmt.assign

import astnodes.exp.`var`.{MemLoad, Var}
import astnodes.exp.{BinOp, BinOperator, Expr, Extract, Literal}
import astnodes.stmt.Stmt
import astnodes.Label

/** Store fact
  */
case class MemAssign(val pc: String, override val lhs: MemLoad, override val rhs: Expr)
    extends Assign(pc, lhs, rhs)
    with Stmt(Label(pc)) {

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String =
    (0 to lhs.size.get / 8 - 1)
      .map(n => {
        val newMemExp = BinOp(BinOperator.Addition, lhs.exp, Literal(n.toString))
        s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhs).toBoogieString}"
      })
      .mkString("; ") + s";     // $pc"

}
