package util

import scala.util.matching.Regex
import scala.collection.mutable

object UniversalIndenter {

  enum TokenType {
    case Literal
    case Separator
    case Open
    case Close
  }

  case class Config(tokens: Iterable[(Regex, TokenType)], maxWidth: Int, indentWidth: Int)

  protected case class State(
    start: Int, // index of beginning of open token
    openLength: Int,
    closeLength: Int,
    ty: TokenType,
    var children: List[State] // child tokens
  ) {
    def add(s: State) = {
      children = s +: children
      s
    }
  }

  // given Ordering[State] with {
  //   def compare(x: State, y: State) = scala.runtime.signum(scala.runtime.RichInt(x.start - y.start))
  // }
}

/**
 * Implements a universal text indenter. That is, an indenter which
 * is language and syntax agnostic. It works simply by matching opening,
 * closing, and separating tokens and breaking lines where the length between
 * the open and closing tokens exceeds the maximum line width.
 */
class UniversalIndenter(config: UniversalIndenter.Config) {
  import UniversalIndenter.*

  var tokenPositions: mutable.HashMap[(Regex, TokenType), Option[State]] =
    config.tokens.map(x => (x(0), x(1)) -> None).to(mutable.HashMap)

  var stringPos = 0
  var string: CharSequence = null
  var currentToken: State = null

  def nextToken() =
    for (((re, ty), st) <- tokenPositions) {
      if (st.fold(-1)(_.start) <= stringPos) {
        tokenPositions((re, ty)) =
          re.findFirstMatchIn(string).map(m => currentToken.add(State(m.start, m.end - m.start, -1, ty, List())))
      }
      println()
    }

  def indent(s: String): Iterator[String] = {
    string = s
    for ((re, ty) <- config.tokens) {}

    Iterator()
  }
}
