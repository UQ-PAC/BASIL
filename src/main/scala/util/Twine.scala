package util.twine

/**
 * A [[Twine]] is a tree representation of a string.
 *
 * This lets us operate on strings without constantly needing to re-create
 * and copy entirely new strings. Using the subclasses within this trait,
 * we can do operations like concatenate and indent very efficiently.
 *
 * The Twine is only converted to a string once, using [[Twine#mkString]],
 * when all operations are done and the final string needs to be emitted.
 */
sealed trait Twine {

  /**
   * Formats the current Twine into a string.
   */
  def mkString: String =
    build(StringBuilder()).result()

  /**
   * Appends the current Twine into the given [[StringBuilder]], with optional
   * additional options.
   */
  def build(sb: StringBuilder, indent: String = "  ", newline: String = System.lineSeparator()): StringBuilder = {

    /**
     * Returns `true` the first time that once() is called,
     * then returns `false` for every subsequent call.
     */
    case class Once(private var first: Boolean = true) {
      def once() = {
        val x = first
        first = false
        x
      }
    }

    // we need to insert newlines /between/ elements of Lines.
    // however, the insertion of newlines should be deferred until the next
    // Str. because if we have Lines(Lines(Str("x"))), this should not
    // insert multiple newlines.
    var doNewline = Once()

    // indentation should be added on the first line of the full Twine,
    // but the first line should have no preceding newline. this is important
    // when the Twine begines with an Indent.
    val firstEver = Once()

    def helper(tw: Twine, ind: String): Unit = tw match {
      case Indent(tw) => helper(tw, ind + indent)
      case Str(s) if s.nonEmpty =>
        if (doNewline.once())
          if (!firstEver.once())
            sb ++= newline
          sb ++= ind
        sb ++= s
      case Str(_) => ()
      case Lines(lines) =>
        var firstInLine = Once()
        // a newline is inserted /between/ elements in Lines. we do
        // this by prefixing all lines with newline aside from the first.
        //
        // furthermore, newline and indent should only be inserted upon
        // reaching a non-empty literal string. multiple Lines nodes placed within
        // each other should not insert additional newlines or indentation.
        lines.foreach { case l =>
          val first = firstInLine.once()
          if (!first)
            doNewline = Once()
          helper(l, ind)

          // if no newline was introduced by this list element, we should manually
          // add one if needed. this allows blank lines to be produced by placing
          // Twine.empty within Lines.
          if (!first && doNewline.once())
            sb ++= newline
        }
      case Concat(tws) => tws.foreach(helper(_, ind))
    }

    helper(this, "")
    sb
  }

  def +:(s: String) = Twine(s, this)
}

/**
 * Indents each line within the given Twine. Lines are those defined by [[Lines]].
 *
 * Indent width is specified later, when the Twine is eventually converted
 * to a string.
 */
case class Indent(tw: Twine) extends Twine

/**
 * A simple string component. This must not contain any newline characters,
 * either [[Lines]] or [[Twine.lines]] must be used for building multi-line Twines.
 */
case class Str(s: String) extends Twine {
  require(!s.contains('\n'), "Twine's Str constructor cannot be used for multi-line strings. Use Lines instead.")

  /**
   * Overrides the case class's toString to display the contained string
   * with quotes and escapes (i.e., like a Scala string literal).
   */
  override def toString = s"Str(${util.StringEscape.quote(s)})"
}

/**
 * Multiple lines. Each component of the given iterable is joined by a newline
 * (and any necessary indentation). Newlines are used as *separators*, not terminators.
 */
case class Lines(ss: Iterable[Twine]) extends Twine

/**
 * Concatenates the given twines, placing them directly adjacent to each other.
 */
case class Concat(tws: Iterable[Twine]) extends Twine

/**
 * Provides useful methods for constructing nd manipulating Twines
 * (e.g., [[Twine.empty]], [[Twine.lines]], [[Twine.indentNested]]).
 * Can be used with the [[Twine.apply]] method to convert a number of strings to a Twine and concatenate
 * them.
 */
object Twine {

  /**
   * The empty twine.
   */
  val empty = Concat(Nil)

  /**
   * Coerces a String to a Twine, or returns the given Twine.
   */
  def apply(x: String | Twine): Twine = x match {
    case x: String => Str(x)
    case x: Twine => x
  }

  /**
   * Concatenates the given parts into a Twine.
   */
  def apply(parts: Seq[String | Twine]): Twine = {
    if (parts.isEmpty) {
      empty
    } else if (parts.length == 1) {
      Twine(parts.head)
    } else {
      Concat(parts.map(Twine(_)))
    }
  }

  /**
   * Concatenates the given parts into a Twine.
   */
  @scala.annotation.targetName("applyMany")
  def apply(parts: (String | Twine)*): Twine =
    apply(parts)

  /**
   * Indents the given twine.
   */
  def indent(tw: Twine) = Indent(tw)

  /**
   * Joins the given parts into a multi-line Twine,
   * separated by line separators.
   */
  def lines(parts: (String | Twine)*) =
    Lines(parts.map(Twine(_)))

  def shallowIsMultiline(tw: Twine) = tw match {
    case _: Lines | _: Indent => true
    case Concat(parts) =>
      parts.exists {
        case _: Lines | _: Indent => true
        case _ => false
      }
    case _: Str => false
  }

  /**
  * Indents a nested structure, placing the indented `elems` between `head` and `tail`,
  * and separating them by `sep` and `newline`.
  *
  * The produced string will follow approximately this pattern:
  *
  * ```
  * head newline INDENT( elem1 sep newline elem2 sep newline elem3 ...) newline tail
  * ```
  *
  * where INDENT(...) indicates that the ... is to be indented. With the default `sep`
  * and `newline`, this will produce something like this:
  *
  * ```
  * head
  *   elem1,
  *   elem2,
  *   elem3
  * tail
  * ```
  *
  * If `headSep` is set, the function will place a separator after `head`
  * and before the first newline, i.e.:
  *
  * ```
  * head sep newline INDENT( elem1 sep newline elem2 ...) newline tail
  * ```
  *
  * In all cases, if `elems` is empty, no newlines will be inserted and the returned twine is simply
  *
  * ```
  * head tail
  * ```
  *
  * If `trySingleLine` is omitted or true, then single-item elements will attempt to
  * be displayed in a single line. This is subject to the single-element being a
  * non-multiline Twine.
  */
  def indentNested(
    head: String,
    elems: Iterable[Twine],
    tail: String,
    sep: String = ",",
    headSep: Boolean = false,
    trySingleLine: Boolean = true
  ): Twine = {
    val len = elems.iterator.length

    if (len == 0) {
      Str(head + tail)
    } else if (len == 1 && trySingleLine && !shallowIsMultiline(elems.head)) {
      val gap = if headSep then Str(sep + " ") else empty
      Twine(head, gap, elems.head, tail)
    } else {
      val first = if headSep then Str(head + sep) else Str(head)
      Lines(
        List(
          first,
          Indent(Lines(elems.zipWithIndex.map {
            case (x, i) if i == len - 1 => x
            case (x, _) => Twine(x, sep)
          })),
          Str(tail)
        )
      )
    }
  }
}
