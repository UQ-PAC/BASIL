package analysis.data_structure_analysis

import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver, UnionFindSolver}
import ir.{Expr, Procedure, Program}
import specification.{ExternalFunction, SymbolTableEntry}
import util.IRContext

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

case class Interval(start: Int, end: Int) {
  require(start < end)

  override def toString: String = s"$start-${end-1}"
  def size: Int = end - start
  def move(func: Int => Int): Interval = Interval(func(start), func(end))
  def isEmpty: Boolean = this.size == 0
  def growTo(size: Int): Interval = Interval(start, math.max(end, start + size))
  def contains(offset: Int): Boolean = start <= offset && end > offset
  def contains(interval: Interval): Boolean =
    start <= interval.start && end >= interval.end
  def isOverlapping(other: Interval): Boolean = !(start >= other.end || other.start >= end)
  def join(other: Interval): Interval = {
    //require(isOverlapping(other), "Expected overlapping Interval for a join")
    Interval(math.min(start, other.start), math.max(end, other.end))
  }
}

object Interval {
  def join(interval1: Interval, interval2: Interval): Interval = interval1.join(interval2)

  implicit def orderingByTuple[T <: Interval]: Ordering[T] =
    Ordering.by(i => (i.start, i.end))
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
  var domain: Set[Procedure] = Set(proc)
  val stack: mutable.Stack[Procedure] = mutable.Stack()
  stack.pushAll(proc.calls)

  // calculate the procedures used in the program
  while (stack.nonEmpty) {
    val current = stack.pop()
    domain += current
    stack.pushAll(current.calls.diff(domain))
  }

  domain ++ (context.program.procedures.filter(f => context.funcEntries.map(_.name).filter(!_.startsWith("_")).contains(f.procName)))
}

trait DSACell