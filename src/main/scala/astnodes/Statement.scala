package astnodes
import boogie._

trait Statement {
  def modifies: Set[Memory] = Set()
  def locals: Set[LocalVar] = Set()
  def calls: Set[String] = Set() // names of functions called by this statement
}

class DirectCall(var target: String, var condition: Expr, var returnTarget: Option[String], var line: String, var instruction: String) extends Statement {
  override def calls: Set[String] = Set(target)
  override def locals: Set[LocalVar] = condition.locals

  override def toString: String = String.format("call %s(); with return %s", target, returnTarget.getOrElse("none"))
}

class IntrinsicCall(var target: String, var condition: Expr, var returnTarget: Option[String], var line: String, var instruction: String) extends Statement {
  override def calls: Set[String] = Set(target)
  override def locals: Set[LocalVar] = condition.locals
}

class IndirectCall(var target: LocalVar, var condition: Expr, var returnTarget: Option[String], var line: String, var instruction: String) extends Statement {
  override def locals: Set[LocalVar] = condition.locals + target

  override def toString: String = String.format("call %s(); with return %s", target, returnTarget.getOrElse("none"))
}

class GoTo(var target: String, var condition: Expr, var line: String, var instruction: String) extends Statement {
  override def locals: Set[LocalVar] = condition.locals

  override def toString: String = String.format("goto %s;", target)
}

class Skip(var line: String, var instruction: String) extends Statement {
  override def toString: String = "skip;"
}

trait Assign(lhs: Variable, rhs: Expr, line: String, instruction: String) extends Statement {
  override def toString: String = String.format("%s := %s;", lhs, rhs)
}

/** Memory store
  */
class MemAssign(var lhs: Memory, var rhs: Store, var line: String, var instruction: String) extends Assign(lhs, rhs, line, instruction) {
  override def modifies: Set[Memory] = Set(lhs)
  override def locals: Set[LocalVar] = rhs.locals
}

object MemAssign {
  def init(lhs: Memory, rhs: Store, line: String, instruction: String): MemAssign = {
    if (rhs.memory.name == "stack") {
      MemAssign(lhs.copy(name = "stack"), rhs, line, instruction)
    } else {
      MemAssign(lhs, rhs, line, instruction)
    }
  }
}

class LocalAssign(var lhs: LocalVar, var rhs: Expr, var line: String, var instruction: String) extends Assign(lhs, rhs, line, instruction) {
  override def locals: Set[LocalVar] = rhs.locals + lhs
}
