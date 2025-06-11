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
        lines.foreach {
          case l =>
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

  def +:(s: String) = Concat(List(Str(s), this))
}

case class Indent(tw: Twine) extends Twine
case class Str(s: String) extends Twine {
  override def toString = s"Str(${util.StringEscape.quote(s)})"
}
case class Lines(ss: Iterable[Twine]) extends Twine
case class Concat(tws: Iterable[Twine]) extends Twine

object Twine {

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
      Lines(List(
        first,
        Indent(Lines(
          elems.zipWithIndex.map {
            case (x, i) if i == len - 1 => x
            case (x, _) => Concat(List(x, sepTwine))
          }
        )),
        Str(tail)
      ))
    }
  }
}

