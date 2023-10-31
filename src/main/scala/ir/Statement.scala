package ir

trait Command

trait Statement extends Command {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def acceptVisit(visitor: Visitor): Statement = throw new Exception(
    "visitor " + visitor + " unimplemented for: " + this
  )
}

class LocalAssign(var lhs: Variable, var rhs: Expr, val label: Option[String] = None) extends Statement {
  //override def locals: Set[Variable] = rhs.locals + lhs
  override def modifies: Set[Global] = lhs match {
    case r: Register => Set(r)
    case _           => Set()
  }
  override def toString: String = s"$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitLocalAssign(this)
}


object LocalAssign:
  def apply(lhs: Variable, rhs: Expr, label: Option[String])  = new LocalAssign(lhs, rhs, label)
  def unapply(l: LocalAssign) : Option[(Variable, Expr, Option[String])] = Some(l.lhs, l.rhs, l.label)

class MemoryAssign(var lhs: Memory, var rhs: MemoryStore, val label: Option[String] = None) extends Statement {
  override def modifies: Set[Global] = Set(lhs)
  //override def locals: Set[Variable] = rhs.locals
  override def toString: String = s"$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitMemoryAssign(this)
}

object MemoryAssign:
  def apply(lhs: Memory, rhs: MemoryStore, label: Option[String]) = new MemoryAssign(lhs, rhs, label)
  def unapply(m: MemoryAssign) : Option[(Memory, MemoryStore, Option[String])] = Some(m.lhs, m.rhs, m.label)

case object NOP extends Statement {
  override def toString: String = "<NOP>"
  override def acceptVisit(visitor: Visitor): Statement = this
}

class Assume(var body: Expr, var comment: Option[String]) extends Statement {
  override def toString: String = s"assume $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssume(this)
}

object Assume:
  def apply(body: Expr, comment: Option[String]) = new Assume(body, comment)
  def unapply(a: Assume) : Option[(Expr, Option[String])] = Some(a.body, a.comment)


class Assert(var body: Expr, var comment: Option[String]) extends Statement {
  override def toString: String = s"assert $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssert(this)
}

object Assert:
  def apply(body: Expr, comment: Option[String]) : Assert = new Assert(body, comment)
  def unapply(a: Assert) : Option[(Expr, Option[String])] = Some(a.body, a.comment)

trait Jump extends Command {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def calls: Set[Procedure] = Set()
  def acceptVisit(visitor: Visitor): Jump = throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}
class GoTo(var target: Block, var condition: Option[Expr]) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  override def toString: String = s"GoTo(${target.label}, $condition)"

  override def acceptVisit(visitor: Visitor): Jump = visitor.visitGoTo(this)
}

object GoTo:
  def apply(target: Block, condition: Option[Expr]) = new GoTo(target, condition)

  def unapply(g: GoTo) : Option[(Block, Option[Expr])]= Some(g.target, g.condition)

class DirectCall(var target: Procedure, var condition: Option[Expr], var returnTarget: Option[Block], val address: Option[Int] = None) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  override def calls: Set[Procedure] = Set(target)
  override def toString: String = s"DirectCall(${target.name}, $condition, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitDirectCall(this)
}


object DirectCall:
  def apply(target: Procedure, condition: Option[Expr], returnTarget: Option[Block], address: Option[Int] = None)
  = new DirectCall(target, condition, returnTarget, address)

  def unapply(i: DirectCall): Option[(Procedure, Option[Expr], Option[Block], Option[Int])] = Some(i.target, i.condition, i.returnTarget, i.address)


class IndirectCall(var target: Variable, var condition: Option[Expr], var returnTarget: Option[Block],
                   val address: Option[Int] = None) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals + target
    case None => Set(target)
  } */
  override def toString: String = s"IndirectCall($target, $condition, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitIndirectCall(this)
}

object IndirectCall:
  def apply(target: Variable, condition: Option[Expr], returnTarget: Option[Block], address: Option[Int] = None)
    = new IndirectCall(target, condition, returnTarget, address)

  def unapply(i: IndirectCall): Option[(Variable, Option[Expr], Option[Block], Option[Int])] = Some(i.target, i.condition, i.returnTarget, i.address)