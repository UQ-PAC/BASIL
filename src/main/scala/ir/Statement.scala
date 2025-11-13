package ir
import util.assertion.*
import util.intrusive_list.IntrusiveListElement

import scala.util.Random

import collection.immutable.SortedMap
import collection.immutable.ListSet
import collection.mutable

/*
  To support the state-free IL iteration in CFG order all Commands must be classes with a unique object ref.
 */

/** A Statement or Jump.
  *
  * Note that some commands have optional labels. For example, jump destinations.
  *
  * **Important:** When implementing this trait, the subclass must implement `label`
  * (i.e. it should declare a `var label` with no override keyword), and the subclass *may*
  * override `comment`. If overriding `comment`, an override keyword is required and the
  * getter and setter should be overriden simultaneously (e.g., with an `override var`).
  */
sealed trait Command extends HasParent[Block] with DeepEquality {
  var label: Option[String]

  // XXX: hack to allow overriding a "mutable" "variable"
  def comment: Option[String] = _comment
  def comment_=(x: Option[String]) = _comment = x
  private var _comment: Option[String] = None

  def labelStr: String = label match {
    case Some(s) => s"$s: "
    case None => ""
  }

  /**
   * Modifies this [[Command]] in-place with the given comment.
   */
  def setComment(comment: Option[String]): this.type =
    this.comment = comment
    this

  /**
   * Modifies this [[Command]] in-place with the given comment.
   */
  def setLabel(label: Option[String]): this.type =
    this.label = label
    this
}

sealed trait Statement extends Command, IntrusiveListElement[Statement] {
  def modifies: Set[Global] = Set()
  def predecessor: Option[Command] = parent.statements.prevOption(this)
  def successor: Command = parent.statements.nextOption(this).getOrElse(parent.jump)
}

sealed trait Assign extends Statement {
  def assignees: Set[Variable]
}

sealed trait SingleAssign extends Assign {
  def lhs: Variable
  override def assignees = Set(lhs)
}

class MemoryAssign(var lhs: Variable, var rhs: Expr, var label: Option[String] = None)
    extends SingleAssign
    with Command {

  override def modifies: Set[Global] = lhs match
    case r: GlobalVar => Set(r)
    case _ => Set()

  override def toString: String = s"$labelStr$lhs := $rhs"

  def deepEquals(o: Object): Boolean = o match {
    case MemoryAssign(l, r, lbl) if l == lhs && r == rhs && lbl == label => true
    case _ => false
  }
}

object MemoryAssign {
  def unapply(l: MemoryAssign): Some[(Variable, Expr, Option[String])] = Some(l.lhs, l.rhs, l.label)
}

class LocalAssign(var lhs: Variable, var rhs: Expr, var label: Option[String] = None)
    extends SingleAssign
    with Command {
  override def modifies: Set[Global] = lhs match {
    case r: GlobalVar => Set(r)
    case _ => Set()
  }
  override def toString: String = s"$labelStr$lhs := $rhs"

  def deepEquals(o: Object) = o match {
    case LocalAssign(l, r, lbl) if l == lhs && r == rhs && lbl == label => true
    case o: LocalAssign =>
      println("" + lhs + " <> " + o.lhs)
      println("" + rhs + " <> " + o.rhs)
      println("" + label + " <> " + o.label)
      println("" + comment + " <> " + o.comment)
      println(this)
      println(o)
      false
    case o => false
  }
}

class SimulAssign(var assignments: Vector[(Variable, Expr)], var label: Option[String] = None)
    extends Assign
    with Command {
  override def modifies: Set[Global] = assignments.collect { case (r: Global, _) =>
    r
  }.toSet

  def assignees = assignments.map(_._1).toSet
  override def toString: String = {
    val assignList = assignments
      .map { case (lhs, rhs) =>
        lhs.toString + " := " + rhs
      }
      .mkString(", ")
    labelStr + assignList
  }

  override def deepEquals(o: Object): Boolean = o match {
    case SimulAssign(otherAssings) => otherAssings == assignments
    case _ => false
  }

}

object SimulAssign {
  def unapply(l: SimulAssign | LocalAssign): Some[(Seq[(Variable, Expr)], Option[String])] = l match {
    case LocalAssign(lhs, rhs, label) => Some(Seq(lhs -> rhs), label)
    case s: SimulAssign => Some((s.assignments, s.label))
  }
}

object LocalAssign {
  def unapply(l: LocalAssign): Some[(Variable, Expr, Option[String])] = Some(l.lhs, l.rhs, l.label)
}

class MemoryStore(
  var mem: Memory,
  var index: Expr,
  var value: Expr,
  var endian: Endian,
  var size: Int,
  var label: Option[String] = None
) extends Statement
    with Command {
  override def modifies: Set[Global] = Set(mem)
  override def toString: String = s"$labelStr$mem[$index] := MemoryStore($value, $endian, $size)"
  override def deepEquals(o: Object) = o match {
    case MemoryStore(m, i, v, e, s, l) => m == mem && i == index && v == value && e == endian && s == size && l == label
    case _ => false
  }
}

object MemoryStore {
  def unapply(m: MemoryStore): Some[(Memory, Expr, Expr, Endian, Int, Option[String])] =
    Some(m.mem, m.index, m.value, m.endian, m.size, m.label)
}

class MemoryLoad(
  var lhs: Variable,
  var mem: Memory,
  var index: Expr,
  var endian: Endian,
  var size: Int,
  var label: Option[String] = None
) extends SingleAssign
    with Command {
  override def modifies: Set[Global] = lhs match {
    case r: GlobalVar => Set(r)
    case _ => Set()
  }
  override def toString: String = s"$labelStr$lhs := MemoryLoad($mem, $index, $endian, $size)"
  override def deepEquals(o: Object) = o match {
    case MemoryLoad(l, m, ind, en, sz, lbl) =>
      l == lhs && m == mem && ind == index && en == endian && sz == size && lbl == label
    case _ => false
  }
}

object MemoryLoad {
  def unapply(m: MemoryLoad): Some[(Variable, Memory, Expr, Endian, Int, Option[String])] =
    Some(m.lhs, m.mem, m.index, m.endian, m.size, m.label)
}

/**
 * A no-operation statement. Subclasses of this class are used to mark atomic regions.
 *
 * **Important:** Subclasses are warned against having a constructor parameter whose name
 * matches a field (e.g, `label`). The private constructor parameter will shadow this
 * class's field in all methods of the subclass.
 */
class NOP(var label: Option[String] = None) extends Statement with Command {
  override def toString: String = s"NOP $labelStr"
  override def deepEquals(o: Object) = o match {
    case NOP(x) => x == label
    case _ => false
  }
}
object NOP {
  def unapply(x: NOP) = Some(x.label)
}

class AtomicStart(_label: Option[String] = None) extends NOP(_label) {
  override def toString: String = s"AtomicStart $labelStr"
}

class AtomicEnd(_label: Option[String]) extends NOP(_label) {
  override def toString: String = s"AtomicEnd $labelStr"
}

class Assert(var body: Expr, override var comment: Option[String] = None, var label: Option[String] = None)
    extends Statement
    with Command {
  override def toString: String = s"${labelStr}assert $body" + comment.map(" //" + _)
  override def deepEquals(o: Object) = o match {
    case Assert(b, c, l) => b == body && c == comment && l == label
  }
}

object Assert {
  def unapply(a: Assert): Some[(Expr, Option[String], Option[String])] = Some(a.body, a.comment, a.label)
}

/** Assumptions express control flow restrictions and other properties that can be assumed to be true.
  *
  * For example, an `if (C) S else T` statement in C will eventually be translated to IR with a non-deterministic goto
  * to two blocks, one with `assume C; S` and the other with `assume not(C); T`.
  *
  * checkSecurity is true if this is a branch condition that we want to assert has a security level of low before
  * branching
  */
class Assume(
  var body: Expr,
  override var comment: Option[String] = None,
  var label: Option[String] = None,
  var checkSecurity: Boolean = false
) extends Statement
    with Command {
  override def toString: String = s"${labelStr}assume $body" + comment.map(" // " + _)
  override def deepEquals(o: Object) = o match {
    case Assume(b, c, l, sec) => b == body && c == comment && l == label && sec == checkSecurity
    case _ => false
  }
}

object Assume {
  def unapply(a: Assume): Some[(Expr, Option[String], Option[String], Boolean)] =
    Some(a.body, a.comment, a.label, a.checkSecurity)
}

sealed trait Jump extends Command {
  def modifies: Set[Global] = Set()
}

class Unreachable(var label: Option[String] = None) extends Jump with Command {
  /* Terminate / No successors / assume false */

  override def deepEquals(o: Object) = o match {
    case Unreachable(l) => label == l
    case _ => false
  }
}

class Return(var label: Option[String] = None, var outParams: SortedMap[LocalVar, Expr] = SortedMap())
    extends Jump
    with Command {
  override def toString = s"Return(${outParams.mkString(",")})"
  override def deepEquals(o: Object): Boolean = o match {
    case Return(lbl, param) => lbl == label && param.toList == outParams.toList
    case _ => false
  }
}

object Unreachable {
  def unapply(u: Unreachable): Some[Option[String]] = Some(u.label)
}

object Return {
  def unapply(r: Return): Some[(Option[String], SortedMap[LocalVar, Expr])] = Some((r.label, r.outParams))
}

class GoTo private (_targets: mutable.LinkedHashSet[Block], var label: Option[String]) extends Jump with Command {

  def this(targets: Iterable[Block], label: Option[String] = None) = this(mutable.LinkedHashSet.from(targets), label)

  def this(target: Block) = this(mutable.Set(target), None)

  def internalShuffleTargets(): Unit = {
    val shuffled = Random.shuffle(_targets.toList)
    _targets.clear()
    _targets ++= shuffled
  }

  def targets: Set[Block] = _targets.to(ListSet)

  override def deepEquals(o: Object): Boolean = o match {
    case GoTo(tgts, lbl) => tgts.map(_.label).toSet == targets.map(_.label).toSet && lbl == label
    case _ => false
  }

  def addAllTargets(t: Iterable[Block]): Unit = {
    t.foreach(addTarget)
  }

  def addTarget(t: Block): Unit = {
    if (_targets.add(t)) {
      t.addIncomingJump(this)
    }
  }

  override def linkParent(b: Block): Unit = {
    _targets.foreach(_.addIncomingJump(this))
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
    debugAssert(!_targets.contains(t))
    debugAssert(!t.incomingJumps.contains(this))
  }

  def replaceTarget(oldBlock: Block, newBlock: Block): Unit = {
    removeTarget(oldBlock)
    addTarget(newBlock)
  }

  override def toString: String = s"${labelStr}GoTo(${targets.map(_.label).mkString(", ")})"
}

object GoTo {
  def unapply(g: GoTo): Some[(Set[Block], Option[String])] = Some(g.targets, g.label)
}

sealed trait Call extends Statement {
  def returnTarget: Option[Command] = successor match {
    case h: Unreachable => None
    case o => Some(o)
  }
}

class DirectCall(
  val target: Procedure,
  var label: Option[String] = None,
  var outParams: SortedMap[LocalVar, Variable] = SortedMap(), // out := formal
  var actualParams: SortedMap[LocalVar, Expr] = SortedMap() // formal := actual
) extends Call
    with Assign
    with Command {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals
    case None => Set()
  } */
  def calls: Set[Procedure] = Set(target)
  override def toString: String =
    s"${labelStr}${outParams.values.map(_.name).mkString(",")} := DirectCall(${target.name})(${actualParams.values.mkString(",")})"

  def assignees: Set[Variable] = outParams.values.toSet

  override def linkParent(p: Block): Unit = {
    super.linkParent(p)
    target.addCaller(this)
  }

  override def unlinkParent(): Unit = {
    super.unlinkParent()
    target.removeCaller(this)
  }

  override def deepEquals(o: Object): Boolean = o match {
    case dc @ DirectCall(tgt, out, actual, l) =>
      tgt.name == target.name && l == label && out == outParams && actual == actualParams
    case _ => false
  }

}

object DirectCall {
  def unapply(i: DirectCall): Some[(Procedure, Map[LocalVar, Variable], Map[LocalVar, Expr], Option[String])] =
    Some(i.target, i.outParams, i.actualParams, i.label)
}

class IndirectCall(var target: Variable, var label: Option[String] = None) extends Call with Command {
  /* override def locals: Set[Variable] = condition match {
    case Some(c) => c.locals + target
    case None => Set(target)
  } */
  override def toString: String = s"${labelStr}IndirectCall($target)"
  override def deepEquals(o: Object): Boolean = o match {
    case IndirectCall(t, l) => t == target && l == label
    case _ => false
  }
}

object IndirectCall {
  def unapply(i: IndirectCall): Some[(Variable, Option[String])] = Some(i.target, i.label)
}
