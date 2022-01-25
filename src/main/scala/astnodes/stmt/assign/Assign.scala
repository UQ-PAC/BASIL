package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.`var`.{MemLoad, Var, Register}
import astnodes.stmt.Stmt

import java.util

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */
trait Assign(pc: String, val lhs: Var, val rhs: Expr) extends Stmt {
  override def toString = String.format("%s%s := %s;", label, lhs, rhs)

  override def subst(v: Var, w: Var): Stmt = lhs.subst(v, w) match {
    case lhsRes: MemLoad  => MemAssign(pc, lhsRes, rhs.subst(v, w))
    case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = rhs.subst(v, w))
  }
}
