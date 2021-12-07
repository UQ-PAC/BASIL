package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.stmt.Stmt
import java.util

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */
abstract class Assign (override val pc: String, var lhs: Expr, var rhs: Expr) extends Stmt(pc) {
  def getLhs = lhs
  def getRhs = rhs

  override def toString = String.format("%s%s := %s;", getLabel, lhs, rhs)
  override def toBoogieString: String = f"$getLabel$lhs := ${rhs.toBoogieString};"
  override def getChildren = util.Arrays.asList(lhs, rhs)
  override def replace(oldExp: Expr, newExp: Expr) = {
    if (lhs == oldExp) lhs = newExp
    if (rhs == oldExp) rhs = newExp
  }
}
