package analysis.data_structure_analysis

import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure, Program}
import specification.{ExternalFunction, SymbolTableEntry}
import util.{DSALogger, IRContext}
import java.io.File

import scala.collection.{SortedSet, mutable}

enum DSAPhase {
  case Pre, Local, BU, TD
}

/**
 * Integer Interval with top and bottom
 * start (s) <= end (e)
 * For purposes of overlapping and size the interval is inclusive of s and exclusive of e
 */
enum Interval extends Offsets {
  case Top
  case Bot
  case Value(s: Int, e: Int)

  this match
    case Interval.Value(s, e) => assert(s <= e, "start of interval should be less than its end")
    case _ =>

  override def toString: String =
    this match
      case Interval.Top => "Top"
      case Interval.Bot => "Bot"
      case Interval.Value(start, end) => s"${start}_$end"

  def start: Option[Int] =
    this match
      case Interval.Value(s, e) => Some(s)
      case _ => None

  def end: Option[Int] =
    this match
      case Interval.Value(s, e) => Some(e)
      case _ => None

  def size: Option[Int] =
    this match
      case Interval.Value(start, end) => Some(end - start)
      case _ => None

  def move(func: Int => Int): Interval =
    this match
      case Interval.Value(start, end) => Value(func(start), func(end))
      case x => x

  def isEmpty: Boolean = this.size.contains(0)

  def growTo(size: Int): Interval =
    this match
      case Interval.Top => Interval.Top
      case Interval.Value(start, end) => Interval(start, math.max(end, start + size))
      case _ => this

  def contains(offset: Int): Boolean =
    this match
      case Interval.Top => true
      case Interval.Value(start, end) => start <= offset && (end > offset || end == start)
      case _ => false

  def contains(interval: Interval): Boolean =
    (this, interval) match
      case (Interval.Top, _) => true
      case (_, Interval.Top) => false // this is not top
      case (a, b) if a == b => true
      case (Interval.Value(s, e), _) if s == e => false
      case (Interval.Value(start1, end1), Interval.Value(start2, end2)) =>
        start1 <= start2 && (end1 > end2 || (start2 < end2 && end1 >= end2))
      case _ => false

  def isOverlapping(other: Interval): Boolean =
    (this, other) match
      case (Interval.Top, _) => true
      case (_, Interval.Top) => true
      case (a, b) if a == b => true
      case (Interval.Value(s1, e1), Interval.Value(s2, e2)) =>
        (s2 < e1 && s2 >= s1) || (s1 < e2 && s1 >= s2)


  def join(other: Interval): Interval = {
    (this, other) match
      case (Interval.Top, _) => Interval.Top
      case (_, Interval.Top) => Interval.Top
      case (Interval.Bot, x) => x
      case (x, Interval.Bot) => x
      case (Interval.Value(start1, end1), Interval.Value(start2, end2)) =>
        Interval(math.min(start1, start2), math.max(end1, end2))
  }

  override def toOffsets: Set[Int] = {
    this match
      case Interval.Value(s, e) => Set(s to e: _*)
      case _ => throw Exception("Attempted to retrieve offsets from top/bot")
  }

  override def toIntervals: Set[Interval] = {
    this match
      case Interval.Bot => Set.empty
      case x => Set(x)
  }

}

object Interval {
  def apply(start: Int, end: Int) = {
    require(start <= end, "start of interval should be less than it's end")
    Interval.Value(start, end)
  }
  def join(interval1: Interval, interval2: Interval): Interval = interval1.join(interval2)
  implicit def orderingByTuple[T <: Interval]: Ordering[T] =
    Ordering.by {
      case Interval.Value(start, end) =>
        (start, end)
      case _ => (Int.MinValue, Int.MinValue)
    }
}

class DSFlag {
  var collapsed = false
  var function = false
  var stack = false
  var heap = false
  var global = false
  var unknown = false
  var escapes = false
  var read = false
  var modified = false
  var incomplete = false
  var foreign = false
  var merged = false

  def join(other: DSFlag): Unit =
    collapsed = collapsed || other.collapsed
    stack = other.stack || stack
    heap = other.heap || heap
    global = other.global || global
    unknown = other.unknown || unknown
    read = other.read || read
    escapes = escapes || other.escapes
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    foreign = other.foreign && foreign
    merged = true
    function = function || other.function
}

def joinFlags(pointers: Iterable[IntervalCell]): DSFlag = {
  val flag = DSFlag()
  pointers.foreach(c => flag.join(c.node.flags))
  flag
}

def computeDSADomain(proc: Procedure, context: IRContext): Set[Procedure] = {
  var domain: Set[Procedure] = Set(proc) ++ (context.program.procedures.filter(f =>
    context.funcEntries.map(_.name).filter(!_.startsWith("_")).contains(f.procName)
  ))

  val stack: mutable.Stack[Procedure] = mutable.Stack()
  stack.pushAll(domain.flatMap(_.calls))

  // calculate the procedures used in the program
  while (stack.nonEmpty) {
    val current = stack.pop()
    domain += current
    stack.pushAll(current.calls.diff(domain))
  }

  domain
}

trait DSACell
