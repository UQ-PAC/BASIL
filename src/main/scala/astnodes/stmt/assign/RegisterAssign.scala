package astnodes.stmt.assign

import astnodes.exp.Expr
import astnodes.exp.variable.{Register, Variable}
import astnodes.stmt.Stmt

// TODO check if we need override pc
/** Load fact
  */
case class RegisterAssign(override val pc: String, override val lhs: Register, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(pc) {
  override def toBoogieString: String = s"$labelString${lhs.toBoogieString} := ${rhs.toBoogieString};    // $pc"

  // Otherwise is flag (e.g. #1)
  def isRegister: Boolean = lhs.name.charAt(0) == 'R'
  
  def isStackPointer: Boolean = this.isRegister && lhs.name.substring(1).equals("31")
  def isFramePointer: Boolean = this.isRegister && lhs.name.substring(1).equals("29")
  def isLinkRegister: Boolean = this.isRegister && lhs.name.substring(1).equals("30")
}
