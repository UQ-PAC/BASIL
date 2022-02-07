package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}
import astnodes.stmt.Stmt
import astnodes.Label

// TODO check if we need override pc
/** Load fact
  */
case class RegisterAssign(val pc: String, override val lhs: Register, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(Label(pc)) {
  override def toBoogieString: String = s"$label${lhs.toBoogieString} := ${rhs.toBoogieString};    // $pc"

  // Otherwise is flag (e.g. #1)
  def isRegister = lhs.name.charAt(0) == 'R'
  
  def isStackPointer = this.isRegister && lhs.name.substring(1).equals("31")
  def isFramePointer = this.isRegister && lhs.name.substring(1).equals("29")
  def isLinkRegister = this.isRegister && lhs.name.substring(1).equals("30")
}
