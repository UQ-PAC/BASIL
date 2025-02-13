package util

import scala.collection.immutable.{LazyList, Seq}
import scala.collection.{IterableOps, Factory}
import scala.collection.{ AbstractIterator, AbstractView, BuildFrom }
import scala.collection.generic.IsSeq

/**
 * A `Twine` is a lazy list of strings. This allows efficient concatenation and indentation
 * without allocating and copying strings.
 *
 * NOTE: Conventions:
 * - A `Twine` should be constructed such that when concatenated, it produces
 *   the desired string (e.g., with x.mkString).
 * - If a string within a `Twine` is to contain a newline, the newline should appear
 *   only at the end of the string. Each string in a twine should contain at most
 *   one newline character.
 */
type Twine = LazyList[String]

/**
 * Indents the given iterable of strings, using the given prefix.
 *
 * The iterable does not need to be a `Twine`, but it is assumed to conform
 * to the `Twine` conventions. In particular, newlines must occur as the last character
 * in a string.
 *
 * This function has the desirable properties of being (1) efficient and (2) compositional.
 * (1) means that strings are never copied or appended to each other. Only lists are
 * manipulated, and even then, they are lazy and generated on-demand. (2) means that it is
 * easy to indent multiple times by calling this function multiple times. This is in comparison
 * to, for example, a method which requires an indent level parameter to be passed to
 * recursive subcalls.
 */
def indent(ss: Iterable[String], prefix: String = "  "): Twine =
  ss.take(1).to(LazyList) ++
    ss.sliding(2).map(_.toSeq).flatMap {
      case Seq(_) => LazyList()
      case Seq(prev,s) => if (prev.endsWith("\n")) LazyList(prefix, s) else LazyList(s)
      case x => throw new AssertionError(s"sliding(2) returned unexpected length: ${x.length}")
    }

/**
 * Indents a nested structure, placing the indented `elems` between `head` and `tail`,
 * and separating them by `sep` and `newline`.
 *
 * The produced string will follow approximately this pattern:
 *
 *     head newline INDENT( elem1 sep newline elem2 sep newline elem3 ...) newline tail
 *
 * where INDENT(...) indicates that the ... is to be indented. With the default `sep`
 * and `newline`, this will produce something like this:
 *
 *     head
 *       elem1,
 *       elem2,
 *       elem3
 *     tail
 *
 * If `headSep` is set, the function will place a separator after `head`
 * and before the first newline, i.e.:
 *
 *     head sep newline INDENT( elem1 sep newline elem2 ...) newline tail
 *
 * In all cases, if `elems` is empty, no newlines will be inserted and the returned twine is simply
 *
 *     head tail
 */
def indentNested(head: String, elems: Iterable[Twine], tail: String, newline: String = "\n", sep: String = ",", headSep: Boolean = false): Twine =

  // this implements the described functionality by preceding all elements with \n,
  // then preceding all elements aside from the first with `sep`.
  // if `headSep` is given, `sep` is prefixed to the first element as well.
  val offset = if (headSep) 1 else 0
  def makeElem(x: Iterable[String], i: Int): Iterable[String] =
    val s = (if (offset + i > 0) Iterable(sep) else Iterable.empty)
    s ++ Iterable(newline) ++ x

  val indented = indent(elems.zipWithIndex.flatMap(makeElem.tupled))

  val body = if (indented.isEmpty) indented else (indented :+ newline)

  head +: body :+ tail


// from https://stackoverflow.com/a/40073137, itself from the scala.runtime source.
object StringEscape {
  def quote (s: String): String = "\"" + escape(s) + "\""
  def escape(s: String): String = s.flatMap(escapedChar)

  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
                  else              String.valueOf(ch)
  }
}

// copied, with apologies, from: https://docs.scala-lang.org/overviews/core/custom-collection-operations.html
extension [Repr](coll: Repr)(using seq: IsSeq[Repr])

  /**
   * Intersperses a given element amongst an iterable. For example,
   *
   *     "foo".intersperse(' ') == "f o o"
   *
   * The returned value will have the same type as the original iterable.
   */
  def intersperse[B >: seq.A, That](sep: B)(using bf: BuildFrom[Repr, B, That]): That =
    val seqOps = seq(coll)
    bf.fromSpecific(coll)(new AbstractView[B]:
      def iterator = new AbstractIterator[B]:
        val it = seqOps.iterator
        var intersperseNext = false
        def hasNext = intersperseNext || it.hasNext
        def next() =
          val elem = if intersperseNext then sep else it.next()
          intersperseNext = !intersperseNext && it.hasNext
          elem
    )
