package bap

trait BAPJump

case class BAPDirectCall(target: String, condition: BAPExpr, returnTarget: Option[String], line: String, instruction: String) extends BAPJump

case class BAPIndirectCall(target: BAPLocalVar, condition: BAPExpr, returnTarget: Option[String], line: String, instruction: String) extends BAPJump

case class BAPGoTo(target: String, condition: BAPExpr, line: String, instruction: String) extends BAPJump

trait BAPStatement

case class BAPSkip(line: String, instruction: String) extends BAPStatement {
  override def toString: String = "skip;"
}

trait BAPAssign(lhs: BAPVariable, rhs: BAPExpr, line: String, instruction: String) extends BAPStatement {
  override def toString: String = String.format("%s := %s;", lhs, rhs)
}

/** Memory store
  */
case class BAPMemAssign(lhs: BAPMemory, rhs: BAPStore, line: String, instruction: String) extends BAPAssign(lhs, rhs, line, instruction)

case object BAPMemAssign {
  def init(lhs: BAPMemory, rhs: BAPStore, line: String, instruction: String): BAPMemAssign = {
    if (rhs.memory.name == "stack") {
      BAPMemAssign(lhs.copy(name = "stack"), rhs, line, instruction)
    } else {
      BAPMemAssign(lhs, rhs, line, instruction)
    }
  }
}

case class BAPLocalAssign(lhs: BAPLocalVar, rhs: BAPExpr, line: String, instruction: String) extends BAPAssign(lhs, rhs, line, instruction)
