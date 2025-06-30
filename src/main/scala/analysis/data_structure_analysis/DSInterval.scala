package analysis.data_structure_analysis

import util.assertion.*

/**
 * Integer Interval with top and bottom
 * start (s) <= end (e)
 * For purposes of overlapping and size the interval is inclusive of s and exclusive of e
 */
enum DSInterval extends Offsets {
  case Top
  case Bot
  case Value(s: Int, e: Int)

  this match
    case DSInterval.Value(s, e) => debugAssert(s <= e, "start of interval should be less than its end")
    case _ =>

  override def toString: String =
    this match
      case DSInterval.Top => "Top"
      case DSInterval.Bot => "Bot"
      case DSInterval.Value(start, end) => s"${start}_$end"

  def start: Option[Int] =
    this match
      case DSInterval.Value(s, e) => Some(s)
      case _ => None

  def end: Option[Int] =
    this match
      case DSInterval.Value(s, e) => Some(e)
      case _ => None

  def size: Option[Int] =
    this match
      case DSInterval.Value(start, end) => Some(end - start)
      case _ => None

  def move(func: Int => Int): DSInterval =
    this match
      case DSInterval.Value(start, end) => Value(func(start), func(end))
      case x => x

  def isEmpty: Boolean = this.size.contains(0)

  def growTo(size: Int): DSInterval =
    this match
      case DSInterval.Top => DSInterval.Top
      case DSInterval.Value(start, end) => DSInterval(start, math.max(end, start + size))
      case _ => this

  def contains(offset: Int): Boolean =
    this match
      case DSInterval.Top => true
      case DSInterval.Value(start, end) => start <= offset && (end > offset || (end == start && offset == end))
      case _ => false

  def contains(interval: DSInterval): Boolean =
    (this, interval) match
      case (DSInterval.Top, _) => true
      case (_, DSInterval.Top) => false // this is not top
      case (a, b) if a == b => true
      case (DSInterval.Value(s, e), _) if s == e => false
      case (DSInterval.Value(start1, end1), DSInterval.Value(start2, end2)) =>
        start1 <= start2 && (end1 > end2 || (start2 < end2 && end1 >= end2))
      case _ => false

  def isOverlapping(other: DSInterval): Boolean =
    (this, other) match
      case (DSInterval.Top, _) => true
      case (_, DSInterval.Top) => true
      case (a, b) if a == b => true
      case (DSInterval.Value(s1, e1), DSInterval.Value(s2, e2)) =>
        (s2 < e1 && s2 >= s1) || (s1 < e2 && s1 >= s2)
      case _ => false

  def join(other: DSInterval): DSInterval = {
    (this, other) match
      case (DSInterval.Top, _) => DSInterval.Top
      case (_, DSInterval.Top) => DSInterval.Top
      case (DSInterval.Bot, x) => x
      case (x, DSInterval.Bot) => x
      case (DSInterval.Value(start1, end1), DSInterval.Value(start2, end2)) =>
        DSInterval(math.min(start1, start2), math.max(end1, end2))
  }

  override def toOffsets: Set[Int] = {
    this match
      case DSInterval.Value(s, e) => Set(s to e: _*)
      case _ => throw Exception("Attempted to retrieve offsets from top/bot")
  }

  override def toIntervals: Set[DSInterval] = {
    this match
      case DSInterval.Bot => Set.empty
      case x => Set(x)
  }

}

object DSInterval {
  def apply(start: Int, end: Int) = {
    require(start <= end, "start of interval should be less than it's end")
    DSInterval.Value(start, end)
  }
  def join(interval1: DSInterval, interval2: DSInterval): DSInterval = interval1.join(interval2)
  implicit def orderingByTuple[T <: DSInterval]: Ordering[T] =
    Ordering.by {
      case DSInterval.Value(start, end) =>
        (start, end)
      case _ => (Int.MinValue, Int.MinValue)
    }
}
