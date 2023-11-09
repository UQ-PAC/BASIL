package ir
import intrusiveList.IntrusiveListElement
import collection.mutable 


sealed trait Command {
  val label: Option[String]
  var parent: Block

  def labelStr: String = label match {
    case Some(s) => s"$s: "
    case None => ""
  }
  
}

sealed trait Statement extends Command with IntrusiveListElement {
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
  def unapply(l: LocalAssign): Option[(Variable, Expr, Block, Option[String])] = Some(l.lhs, l.rhs, l.parent, l.label)

class MemoryAssign(var lhs: Memory, var rhs: MemoryStore,  var parent: Block, override val label: Option[String] = None) extends Statement {
  override def modifies: Set[Global] = Set(lhs)
  //override def locals: Set[Variable] = rhs.locals
  override def toString: String = s"$labelStr$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitMemoryAssign(this)
}

object MemoryAssign:
  def unapply(m: MemoryAssign): Option[(Memory, MemoryStore, Block, Option[String])] = Some(m.lhs, m.rhs, m.parent,  m.label)

case class NOP(override val label: Option[String] = None, var parent: Block) extends Statement {
  override def toString: String = s"$labelStr"
  override def acceptVisit(visitor: Visitor): Statement = this
}

class Assert(var body: Expr, var parent: Block, var comment: Option[String] = None, override val label: Option[String] = None) extends Statement {
  override def toString: String = s"${labelStr}assert $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssert(this)
}

object Assert:
  def unapply(a: Assert): Option[(Expr, Block, Option[String], Option[String])] = Some(a.body, a.parent, a.comment, a.label)

class Assume(var body: Expr,  var parent: Block, var comment: Option[String] = None, override val label: Option[String] = None) extends Statement {
  override def toString: String = s"${labelStr}assume $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssume(this)
}

object Assume:
  def unapply(a: Assume): Option[(Expr, Block, Option[String], Option[String])] = Some(a.body, a.parent, a.comment, a.label)

sealed trait Jump extends Command {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def calls: Set[Procedure] = Set()
  def acceptVisit(visitor: Visitor): Jump = throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}


sealed trait GoTo extends Jump

sealed class DetGoTo (private var _target: Block, var parent: Block, var condition: Option[Expr], override val label: Option[String] = None) extends GoTo {
 _target.incomingJumps.add(parent)

  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */

 def target: Block = _target

 def replaceTarget(newTarget: Block): Block = {
   _target.incomingJumps.remove(parent)
   newTarget.incomingJumps.add(parent)
   _target = newTarget
   _target
 }

  override def toString: String = s"${labelStr}GoTo(${_target.label}, $condition)"

  override def acceptVisit(visitor: Visitor): Jump = visitor.visitGoTo(this)
}

object DetGoTo:
  def unapply(g: DetGoTo): Option[(Block, Block, Option[Expr], Option[String])] = Some(g.target, g.parent, g.condition, g.label)

class NonDetGoTo private (private var _targets: mutable.Set[Block], var parent: Block, override val label: Option[String]) extends GoTo {
  _targets.foreach(_.incomingJumps.add(parent))


  def this(targets: IterableOnce[Block], parent: Block, label: Option[String] = None) = this(mutable.Set.from(targets), parent, label)

  def targets: Set[Block] = _targets.toSet

  def addAllTargets(t: Iterable[Block]): Unit = {
    t.foreach(addTarget(_))
  }

  def addTarget(t: Block): Unit = {
    if (_targets.add(t)) {
      t.incomingJumps.add(parent)
    }
  }

  def removeTarget(t: Block): Unit = {
    // making the assumption that blocks only contain the same outgoing edge once
    //  e.g. We don't have two edges going to the same block under different conditions
    if (_targets.remove(t)) {
      t.incomingJumps.remove(parent)
    }
  }


  override def toString: String = s"${labelStr}NonDetGoTo(${targets.map(_.label).mkString(", ")})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitNonDetGoTo(this)
}


sealed trait Call extends Jump 

class DirectCall(var target: Procedure, var returnTarget: Option[Block], var parent: Block, override val label: Option[String] = None) extends Call {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  override def calls: Set[Procedure] = Set(target)
  override def toString: String = s"${labelStr}DirectCall(${target.name}, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitDirectCall(this)
}

object DirectCall:
  def unapply(i: DirectCall): Option[(Procedure, Option[Block], Block, Option[String])] = Some(i.target, i.returnTarget, i.parent, i.label)

class IndirectCall(var target: Variable, var parent: Block, var returnTarget: Option[Block], override val label: Option[String] = None) extends Call {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals + target
    case None => Set(target)
  } */
  override def toString: String = s"${labelStr}IndirectCall($target, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitIndirectCall(this)
}

object IndirectCall:
  def unapply(i: IndirectCall): Option[(Variable, Block, Option[Block], Option[String])] = Some(i.target, i.parent, i.returnTarget, i.label)
