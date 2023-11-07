package ir

sealed trait Command {
  val label: Option[String]
  var parent: Block

  def labelStr: String = label match {
    case Some(s) => s"$s: "
    case None => ""
  }
}

sealed trait Statement extends Command {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def acceptVisit(visitor: Visitor): Statement = throw new Exception(
    "visitor " + visitor + " unimplemented for: " + this
  )
}

class LocalAssign(var lhs: Variable, var rhs: Expr, var parent: Block, override val label: Option[String] = None) extends Statement {
  //override def locals: Set[Variable] = rhs.locals + lhs
  override def modifies: Set[Global] = lhs match {
    case r: Register => Set(r)
    case _           => Set()
  }
  override def toString: String = s"$labelStr$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitLocalAssign(this)
}

object LocalAssign:
  def unapply(l: LocalAssign): Option[(Variable, Expr, Option[String])] = Some(l.lhs, l.rhs, l.label)

class MemoryAssign(var lhs: Memory, var rhs: MemoryStore,  var parent: Block, override val label: Option[String] = None) extends Statement {
  override def modifies: Set[Global] = Set(lhs)
  //override def locals: Set[Variable] = rhs.locals
  override def toString: String = s"$labelStr$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitMemoryAssign(this)
}

object MemoryAssign:
  def unapply(m: MemoryAssign): Option[(Memory, MemoryStore, Option[String])] = Some(m.lhs, m.rhs, m.label)

case class NOP(override val label: Option[String] = None, var parent: Block) extends Statement {
  override def toString: String = s"$labelStr"
  override def acceptVisit(visitor: Visitor): Statement = this
}

class Assert(var body: Expr, var parent: Block, var comment: Option[String] = None, override val label: Option[String] = None) extends Statement {
  override def toString: String = s"${labelStr}assert $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssert(this)
}

object Assert:
  def unapply(a: Assert): Option[(Expr, Option[String], Option[String])] = Some(a.body, a.comment, a.label)

class Assume(var body: Expr,  var parent: Block, var comment: Option[String] = None, override val label: Option[String] = None) extends Statement {
  override def toString: String = s"${labelStr}assume $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssume(this)
}

object Assume:
  def unapply(a: Assume): Option[(Expr, Option[String], Option[String])] = Some(a.body, a.comment, a.label)

trait Jump extends Command {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def calls: Set[Procedure] = Set()
  def acceptVisit(visitor: Visitor): Jump = throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

class GoTo(var target: Block, var parent: Block, var condition: Option[Expr], override val label: Option[String] = None) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  override def toString: String = s"${labelStr}GoTo(${target.label}, $condition)"

  override def acceptVisit(visitor: Visitor): Jump = visitor.visitGoTo(this)
}

object GoTo:
  def unapply(g: GoTo): Option[(Block, Option[Expr], Option[String])] = Some(g.target, g.condition, g.label)

class NonDetGoTo(var targets: Seq[Block], var parent: Block, override val label: Option[String] = None) extends Jump {
  override def toString: String = s"${labelStr}NonDetGoTo(${targets.map(_.label).mkString(", ")})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitNonDetGoTo(this)
}

class DirectCall(var target: Procedure, var returnTarget: Option[Block], var parent: Block, override val label: Option[String] = None) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  override def calls: Set[Procedure] = Set(target)
  override def toString: String = s"${labelStr}DirectCall(${target.name}, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitDirectCall(this)
}

object DirectCall:
  def unapply(i: DirectCall): Option[(Procedure, Option[Block], Option[String])] = Some(i.target, i.returnTarget, i.label)

class IndirectCall(var target: Variable, var parent: Block, var returnTarget: Option[Block], override val label: Option[String] = None) extends Jump {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals + target
    case None => Set(target)
  } */
  override def toString: String = s"${labelStr}IndirectCall($target, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitIndirectCall(this)
}

object IndirectCall:
  def unapply(i: IndirectCall): Option[(Variable, Option[Block], Option[String])] = Some(i.target, i.returnTarget, i.label)