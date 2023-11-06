package bap

sealed trait BAPJump

case class BAPDirectCall(
    target: String,
    returnTarget: Option[String],
    line: String,
    instruction: String
) extends BAPJump

case class BAPIndirectCall(
    target: BAPVar,
    returnTarget: Option[String],
    line: String,
    instruction: String
) extends BAPJump

case class BAPGoTo(target: String, condition: BAPExpr, line: String, instruction: String) extends BAPJump

sealed trait BAPStatement

sealed trait BAPAssign(lhs: BAPVariable, rhs: BAPExpr, line: String, instruction: String) extends BAPStatement {
  override def toString: String = String.format("%s := %s;", lhs, rhs)
}

/** Memory store
  */
case class BAPMemAssign(lhs: BAPMemory, rhs: BAPStore, line: String, instruction: String, address: Option[Int] = None)
    extends BAPAssign(lhs, rhs, line, instruction)

case class BAPLocalAssign(lhs: BAPVar, rhs: BAPExpr, line: String, instruction: String, address: Option[Int] = None)
    extends BAPAssign(lhs, rhs, line, instruction)
