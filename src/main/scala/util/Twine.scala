package util.twine

sealed trait Twine {

  def mkString: String =
    build(StringBuilder()).result()

  def build(sb: StringBuilder, indent: String = "  ", newline: String = System.lineSeparator()): StringBuilder = {

    // we need to insert newlines /between/ elements of Lines.
    // however, the insertion of newlines should be deferred until the next
    // Str. because if we have Lines(Lines(Str("x"))), this should not
    // insert multiple newlines.
    var doNewline = false

    def helper(tw: Twine, ind: String): Unit = tw match {
      case Indent(tw) => helper(tw, ind + indent)
      case Str(s) =>
        if (doNewline)
          doNewline = false
          sb ++= newline
          sb ++= ind
        sb ++= s
      case Lines(lines) =>
        var first = true
        lines.foreach { case l =>
          if (!first)
            doNewline = true
          first = false
          helper(l, ind)
        }
      case Concat(tws) => tws.foreach(helper(_, ind))
    }

    helper(this, "")
    sb
  }

  def +:(s: String) = Twine(s, this)
}

case class Indent(tw: Twine) extends Twine
case class Str(s: String) extends Twine {
  override def toString = s"Str(${util.StringEscape.quote(s)})"
}
case class Lines(ss: Iterable[Twine]) extends Twine
case class Concat(tws: Iterable[Twine]) extends Twine

object Twine {
  val empty = Concat(Nil)

  def apply(x: String | Twine): Twine = x match {
    case x: String => Str(x)
    case x: Twine => x
  }

  def apply(parts: Seq[String | Twine]): Twine = {
    if (parts.isEmpty) {
      empty
    } else if (parts.length == 1) {
      Twine(parts.head)
    } else {
      Concat(parts.map(Twine(_)))
    }
  }

  @scala.annotation.targetName("applyMany")
  def apply(parts: (String | Twine)*): Twine =
    apply(parts)

  def indent(tw: Twine) = Indent(tw)

  def lines(parts: (String | Twine)*): Twine =
    Lines(parts.map(Twine(_)))

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
    sep: String = ",",
    headSep: Boolean = false
  ): Twine = {
    val len = elems.iterator.length

    val sepTwine = Str(sep)
    if (elems.isEmpty) {
      Str(head + tail)
    } else {
      val first = if headSep then Str(head + sep) else Str(head)
      Lines(
        List(
          first,
          Indent(Lines(elems.zipWithIndex.map {
            case (x, i) if i == len - 1 => x
            case (x, _) => Twine(x, sepTwine)
          })),
          Str(tail)
        )
      )
    }
  }
}
