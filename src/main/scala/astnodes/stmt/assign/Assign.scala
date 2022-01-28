package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.`var`.{MemLoad, Var, Register}
import astnodes.stmt.Stmt

import java.util

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */
trait Assign (pc: String, val lhs: Var, val rhs: Expr) extends Stmt {
  override def toString = String.format("%s%s := %s;", label, lhs, rhs)

  override def subst(v: Expr, w: Expr): Stmt = lhs.subst(v,w) match {
    case lhsRes: MemLoad => MemAssign(pc, lhsRes, rhs.subst(v,w))
    case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = rhs.subst(v,w)),
  }

  def getLhs: Var = lhs
  def getRhs: Expr = rhs

  def fold(oldExpr: Expr, newExpr: Expr): Assign = {
    var updatedRhs : Expr = null

    if (rhs.equals(oldExpr)) {
      updatedRhs = newExpr
    } else {
      updatedRhs = rhs.fold(oldExpr, newExpr)
    }

    lhs match {
      case lhsRes: MemLoad => MemAssign(pc, if (!lhsRes.onStack) lhsRes.fold(oldExpr, newExpr).asInstanceOf[MemLoad] else lhsRes, rhs = updatedRhs)
      case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = updatedRhs)
    }
  }
}
