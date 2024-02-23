package ir
import boogie.{BMapVar, GammaStore}
import intrusivelist.IntrusiveListElement

import collection.mutable

/*
  To support the state-free IL iteration in CFG order all Commands must be classes with a unique object ref.
*/

sealed trait Command extends HasParent[Block] {
  val label: Option[String]

  def labelStr: String = label match {
    case Some(s) => s"$s: "
    case None => ""
  }

}

sealed trait Statement extends Command, HasParent[Block], IntrusiveListElement[Statement] {
  def modifies: Set[Global] = Set()
  def acceptVisit(visitor: Visitor): Statement = throw new Exception(
    "visitor " + visitor + " unimplemented for: " + this
  )
}

// invariant: rhs contains at most one MemoryLoad
class Assign(var lhs: Variable, var rhs: Expr, override val label: Option[String] = None) extends Statement {
  override def modifies: Set[Global] = lhs match {
    case r: Register => Set(r)
    case _           => Set()
  }
  override def toString: String = s"$labelStr$lhs := $rhs"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssign(this)
}

object Assign:
  def unapply(l: Assign): Option[(Variable, Expr, Option[String])] = Some(l.lhs, l.rhs, l.label)

// invariant: index and value do not contain MemoryLoads
class MemoryAssign(var mem: Memory, var index: Expr, var value: Expr, var endian: Endian, var size: Int, override val label: Option[String] = None) extends Statement {
  override def modifies: Set[Global] = Set(mem)
  override def toString: String = s"$labelStr$mem[$index] := MemoryStore($value, $endian, $size)"
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitMemoryAssign(this)
}

object MemoryAssign:
  def unapply(m: MemoryAssign): Option[(Memory, Expr, Expr, Endian, Int, Option[String])] = Some(m.mem, m.index, m.value, m.endian, m.size, m.label)

class NOP(override val label: Option[String] = None) extends Statement {
  override def toString: String = s"NOP $labelStr"
  override def acceptVisit(visitor: Visitor): Statement = this
}

class Assert(var body: Expr, var comment: Option[String] = None, override val label: Option[String] = None) extends Statement {
  override def toString: String = s"${labelStr}assert $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssert(this)
}

object Assert:
  def unapply(a: Assert): Option[(Expr, Option[String], Option[String])] = Some(a.body, a.comment, a.label)

  /**
   * checkSecurity is true if this is a branch condition that we want to assert has a security level of low before branching
   * */
class Assume(var body: Expr, var comment: Option[String] = None, override val label: Option[String] = None, var checkSecurity: Boolean = false) extends Statement {

  override def toString: String = s"${labelStr}assume $body" + comment.map(" //" + _)
  override def acceptVisit(visitor: Visitor): Statement = visitor.visitAssume(this)
}

object Assume:
  def unapply(a: Assume): Option[(Expr, Option[String], Option[String], Boolean)] = Some(a.body, a.comment, a.label, a.checkSecurity)

sealed trait Jump extends Command, HasParent[Block]  {
  def modifies: Set[Global] = Set()
  //def locals: Set[Variable] = Set()
  def calls: Set[Procedure] = Set()
  def acceptVisit(visitor: Visitor): Jump = throw new Exception("visitor " + visitor + " unimplemented for: " + this)
}

class GoTo private (private var _targets: mutable.Set[Block], override val label: Option[String]) extends Jump {

  def this(targets: Iterable[Block], label: Option[String] = None) = this(mutable.Set.from(targets), label)

  def this(target: Block) = this(mutable.Set(target), None)

  def targets: Set[Block] = _targets.toSet

  def addAllTargets(t: Iterable[Block]): Unit = {
    t.foreach(addTarget)
  }

  def addTarget(t: Block): Unit = {
    if (_targets.add(t)) {
      t.addIncomingJump(this)
    }
  }

  override def linkParent(b: Block): Unit = {
    _targets.foreach(_.removeIncomingJump(this))
  }

  override def unlinkParent(): Unit = {
    targets.foreach(_.removeIncomingJump(this))
  }


  def removeTarget(t: Block): Unit = {
    // making the assumption that blocks only contain the same outgoing edge once
    //  e.g. We don't have two edges going to the same block under different conditions
    if (_targets.remove(t)) {
      t.removeIncomingJump(this)
    }
  }

  override def toString: String = s"${labelStr}NonDetGoTo(${targets.map(_.label).mkString(", ")})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitGoTo(this)
}

object GoTo:
  def unapply(g: GoTo): Option[(Set[Block], Option[String])] = Some(g.targets, g.label)


sealed trait Call extends Jump

class DirectCall(val target: Procedure, var returnTarget: Option[Block],  override val label: Option[String] = None) extends Call {
  override def calls: Set[Procedure] = Set(target)
  override def toString: String = s"${labelStr}DirectCall(${target.name}, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitDirectCall(this)

  override def linkParent(p: Block): Unit = {
    target.addCaller(this)
  }

  override def unlinkParent(): Unit = target.removeCaller(this)
}

object DirectCall:
  def unapply(i: DirectCall): Option[(Procedure,  Option[Block], Option[String])] = Some(i.target, i.returnTarget, i.label)

class IndirectCall(var target: Variable, var returnTarget: Option[Block], override val label: Option[String] = None) extends Call {
  override def toString: String = s"${labelStr}IndirectCall($target, ${returnTarget.map(_.label)})"
  override def acceptVisit(visitor: Visitor): Jump = visitor.visitIndirectCall(this)
}

object IndirectCall:
  def unapply(i: IndirectCall): Option[(Variable, Option[Block], Option[String])] = Some(i.target, i.returnTarget, i.label)
