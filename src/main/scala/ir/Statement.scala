package ir

trait Statement {
  def modifies: Set[Memory] = Set()
  def locals: Set[Variable] = Set()
}

class LocalAssign(var lhs: Variable, var rhs: Expr) extends Statement {
  override def locals: Set[Variable] = rhs.locals + lhs
}

class MemoryAssign(var lhs: Memory, var rhs: MemoryStore) extends Statement {
  override def modifies: Set[Memory] = Set(lhs)
  override def locals: Set[Variable] = rhs.locals
}

trait Jump {
  def modifies: Set[Memory] = Set()
  def locals: Set[Variable] = Set()
  def calls: Set[Procedure] = Set()
}

class GoTo(var target: Block, var condition: Option[Expr]) extends Jump {
  override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  }
}

class DirectCall(var target: Procedure, var condition: Option[Expr], var returnTarget: Option[Block]) extends Jump {
  override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  }
  override def calls: Set[Procedure] = Set(target)
}

class IndirectCall(var target: Variable, var condition: Option[Expr], var returnTarget: Option[Block]) extends Jump {
  override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals + target
    case None => Set(target)
  }
}