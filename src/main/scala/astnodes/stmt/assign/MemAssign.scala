package astnodes.stmt.assign

import astnodes.exp.{BinOp, BinOperator, Expr, Extract}
import astnodes.exp.variable.{MemLoad, Variable}
import astnodes.exp.*
import astnodes.stmt.Stmt

/** Store fact
  */
case class MemAssign(override val pc: String, override val lhs: MemLoad, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(pc) with MaybeNonConstantMemAssign {

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String =
    (0 until lhs.size.get / 8).map(n => {
      val newMemExp = BinOp(BinOperator.Addition, lhs.exp, Literal(n.toString))
      s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhs).toBoogieString}"
    }).mkString("; ") + s";     // $pc"

}

trait MaybeNonConstantMemAssign
class NonConstantMemAssign extends MaybeNonConstantMemAssign