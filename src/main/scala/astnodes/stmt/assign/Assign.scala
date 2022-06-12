package astnodes.stmt.assign

import astnodes.exp.{Expr, Literal}
import astnodes.exp.variable.{MemLoad, Register, Variable}
import astnodes.stmt.Stmt

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */
trait Assign(override val pc: String, val lhs: Variable, val rhs: Expr) extends Stmt with MaybeNonConstantAssign {
  override def toString: String = String.format("%s%s := %s;", labelString, lhs, rhs)

  override def subst(v: Variable, w: Variable): Stmt = lhs.subst(v,w) match {
    case lhsRes: MemLoad => MemAssign(pc, lhsRes, rhs.subst(v,w))
    case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = rhs.subst(v,w))
  }

  // this would be nicer to have per type instead
  def fold(oldExpr: Expr, newExpr: Expr): Assign = {
    lhs match {
      case lhsRes: MemLoad =>
        val newLhs = if (!lhsRes.onStack) {
          lhsRes.fold(oldExpr, newExpr).asInstanceOf[MemLoad]
        } else {
          lhsRes
        }
        MemAssign(pc, newLhs, rhs.fold(oldExpr, newExpr))
      case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs.fold(oldExpr, newExpr))
      case _ => this
    }
  }
}

trait MaybeNonConstantAssign
class NonConstantAssign extends MaybeNonConstantAssign
