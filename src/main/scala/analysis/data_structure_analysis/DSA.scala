package analysis.data_structure_analysis

import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure, Program}
import specification.{ExternalFunction, SymbolTableEntry}
import util.{DSALogger, IRContext}
import java.io.File

import scala.collection.{SortedSet, mutable}

trait Counter(val init: Int = 0) {
  private var counter = init
  def increment(by: Int = 1): Int = {
    counter += by
    counter
  }

  def decrement(by: Int = 1): Int = {
    counter -= by
    counter
  }

  def get: Int = counter

  def reset(): Unit = counter = init
}

enum DSAPhase {
  case Pre, Local, BU, TD
}

enum Interval(val start: Option[Int], val end: Option[Int]) {
  case Top extends Interval(None, None)
  case Value(s: Int, e: Int) extends Interval(Some(s), Some(e))

  override def toString: String =
    this match
      case Interval.Top => "Top"
      case Interval.Value(start, end) => s"$start-${end - 1}"

  def size: Option[Int] =
    this match
      case Interval.Top => None
      case Interval.Value(start, end) => Some(end - 1 - start)

  def move(func: Int => Int): Interval =
    this match
      case Interval.Top => Interval.Top
      case Interval.Value(start, end) => Value(func(start), func(end))

  def isEmpty: Boolean = this.size.contains(0)

  def growTo(size: Int): Interval =
    this match
      case Interval.Top => Interval.Top
      case Interval.Value(start, end) => Interval(start, math.max(end, start + size))

  def contains(offset: Int): Boolean =
    this match
      case Interval.Top => true
      case Interval.Value(start, end) => start <= offset && end > offset

  def contains(interval: Interval): Boolean =
    (this, interval) match
      case (Interval.Top, _) => true
      case (_, Interval.Top) => false // this is not top
      case (Interval.Value(start1, end1), Interval.Value(start2, end2)) =>
        start1 <= start2 && end1 >= end2

  def isOverlapping(other: Interval): Boolean =
    (this, other) match
      case (Interval.Top, _) => true
      case (_, Interval.Top) => true
      case (Interval.Value(start1, end1), Interval.Value(start2, end2)) =>
        !(start1 >= end2 || start2 >= end1)

  def join(other: Interval): Interval = {
    (this, other) match
      case (Interval.Top, _) => Interval.Top
      case (_, Interval.Top) => Interval.Top
      case (Interval.Value(start1, end1), Interval.Value(start2, end2)) =>
        Interval(math.min(start1, start2), math.max(end1, end2))
  }
}

object Interval {
  def apply(start: Int, end: Int) = Interval.Value(start, end)
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
    modified = other.modified || modified
    incomplete = other.incomplete || incomplete
    foreign = other.foreign && foreign
    merged = true
    function = function || other.function
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
