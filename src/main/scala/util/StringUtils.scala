package util

import scala.collection.immutable.{LazyList, Seq}
import scala.collection.{IterableOps, Factory}
import scala.collection.{AbstractIterator, AbstractView, BuildFrom}
import scala.collection.generic.IsSeq

import scala.util.matching.Regex
import scala.reflect.NameTransformer

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
 * Converts the given string to a Twine, splitting at newlines as needed.
 */
def stringToTwine(x: String): Twine =
  x.split("\n", -1).intersperse("\n").to(LazyList)

/**
 * Indents the given iterable of strings, using the given prefix.
 *
 * The iterable does not need to be a `Twine`, but it is assumed to conform
 * to the `Twine` conventions. In particular, newlines must occur as the last character
 * in a string.
 *
 * This function has the desirable properties of being both (1) efficient and (2) compositional.
 * (1) means that strings are never copied or appended to each other. Only lists are
 * manipulated and, even then, they are lazy and generated on-demand. (2) means that it is
 * easy to indent multiple times by calling this function multiple times. This is in comparison
 * to, for example, a method which requires an indent level parameter to be passed to
 * recursive subcalls.
 */
def indent(ss: Iterable[String], prefix: String = "  "): Twine =
  (Iterable("") ++ ss)
    .sliding(2)
    .to(LazyList)
    .flatMap(x =>
      x.toSeq match {
        case Seq(_) => Seq.empty
        case Seq(prev, s) => if (prev.endsWith("\n")) Seq(prefix, s) else Seq(s)
        case x => throw new AssertionError(s"sliding(2) returned unexpected length: ${x.length}")
      }
    )

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
def indentNested(
  head: String,
  elems: Iterable[Twine],
  tail: String,
  newline: String = System.lineSeparator(),
  sep: String = ",",
  headSep: Boolean = false
): Twine =

  // this implements the described functionality by preceding all elements with \n,
  // then preceding all elements aside from the first with `sep`.
  // if `headSep` is given, `sep` is prefixed to the first element as well.
  val offset = if (headSep) 1 else 0
  def makeElem(x: Iterable[String], i: Int): Iterable[String] =
    val s = (if (offset + i > 0) stringToTwine(sep) else Iterable.empty)
    s ++ Iterable(newline) ++ x

  val body = indent(elems.zipWithIndex.flatMap(makeElem.tupled))

  if (body.isEmpty)
    LazyList(head, tail)
  else
    head #:: body #::: (newline #:: LazyList(tail))

object StringEscape {

  /**
   * Quotes the given string, returning a string of a string literal.
   * When evaluated by Scala, the literal would return the original string.
   */
  def quote(s: String): String = "\"" + escape(s) + "\""
  def escape(s: String): String = s.flatMap(escapedChar)

  // from https://stackoverflow.com/a/40073137, itself from the scala.runtime source.
  // https://github.com/scala/scala/blob/bada209e96c659d74e20e6ec48ad6888ee19f4c3/src/reflect/scala/reflect/internal/Constants.scala#L270
  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"' => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ =>
      if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else String.valueOf(ch)
  }

  /**
   * Escapes the given string for use as a variable identifier.
   * The returned string should only have letters, characters, and underscore.
   *
   * Note: the returned string might still start with a digit and as such, could
   * be unsuitable for some uses.
   *
   * Examples:
   *
   *     StringEscape.escapeForVariableName("$::") == "DollarColonColon"
   *     StringEscape.escapeForVariableName("0a_b") == "0a_b"
   *
   */
  def escapeForVariableName(s: String) = {
    // https://www.scala-lang.org/api/2.13.3/scala/reflect/NameTransformer$.html
    val transformed = NameTransformer.encode(s.replaceAll("\\$", "Dollar").replaceAll("\\.", "Dot"))
    dollarRe.replaceAllIn(transformed, m => m.group(1).toUpperCase)
  }

  private val dollarRe = new Regex("""\$([a-z])""")

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
    bf.fromSpecific(coll)(
      new AbstractView[B]:
        def iterator = new AbstractIterator[B]:
          val it = seqOps.iterator
          var intersperseNext = false
          def hasNext = intersperseNext || it.hasNext
          def next() =
            val elem = if intersperseNext then sep else it.next()
            intersperseNext = !intersperseNext && it.hasNext
            elem
    )
