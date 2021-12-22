package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}
import astnodes.stmt.Stmt

// TODO check if we need override pc
/** Load fact
  */
<<<<<<< HEAD
case class RegisterAssign(override val pc: String, override val lhs: Register, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(pc) {
  override def toBoogieString: String = s"$getLabel${lhs.toBoogieString} := ${rhs.toBoogieString};    // $pc"

  // Otherwise is flag (e.g. #1)
  def isRegister = lhs.name.charAt(0) == 'R'
=======
case class RegisterAssign(override val pc: String, val lhsExp: Var, val expr: Expr) extends Assign(pc, lhsExp, expr) {
  override def getLhs: Var = lhsExp
  override def toBoogieString: String = s"$getLabel${lhs.toBoogieString} := ${rhs.toBoogieString};    // $pc"

  // Otherwise is flag (e.g. #1)
  def isRegister = lhsExp.name.charAt(0) == 'R'
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
}
