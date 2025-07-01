package util

import scala.collection.generic.IsSeq
import scala.collection.{AbstractIterator, AbstractView, BuildFrom}
import scala.reflect.NameTransformer
import scala.util.matching.Regex

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
