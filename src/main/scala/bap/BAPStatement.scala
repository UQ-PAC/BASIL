package bap

sealed trait BAPJump {
  val line: String
  val instruction: String
}

case class BAPDirectCall(
    target: String,
    returnTarget: Option[String],
    override val line: String,
    override val instruction: String
) extends BAPJump

case class BAPIndirectCall(
    target: BAPVar,
    returnTarget: Option[String],
    override val line: String,
    override val instruction: String
) extends BAPJump

case class BAPGoTo(target: String, condition: BAPExpr, override val line: String, override val instruction: String) extends BAPJump

sealed trait BAPStatement

/** Memory store
  */
case class BAPMemAssign(lhs: BAPMemory, rhs: BAPStore, line: String, instruction: String, address: Option[BigInt] = None) extends BAPStatement {
  override def toString: String = String.format("%s := %s;", lhs, rhs)
}

case class BAPLocalAssign(lhs: BAPVar, rhs: BAPExpr, line: String, instruction: String, address: Option[BigInt] = None) extends BAPStatement {
  override def toString: String = String.format("%s := %s;", lhs, rhs)
}
