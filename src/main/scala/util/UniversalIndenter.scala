package util

import java.util.regex.{Pattern, Matcher, MatchResult}
import scala.util.matching.{Regex}
import scala.collection.mutable

object UniversalIndenter {

  enum TokenType {
    case Literal
    case Separator
    case Open
    case Close
  }

  case class Config(
    tokens: Iterable[(Pattern, TokenType)],
    maxWidth: Int,
    indentWidth: Int
  )

  protected case class State(
    parent: Option[State], // nullable
    start: Int, // index of beginning of open token
    ty: TokenType,
    openLength: Int,
    var bodyLength: Int = -1,
    var closeLength: Int = -1,
    var multiline: Boolean = false,
    var children: Vector[State] = Vector.empty // child tokens
  ) {

    def add(s: State) = {
      children = children :+ s
      s
    }

    override def toString: String = {
      val sb = StringBuilder()
      sb.append(s"State(<parent>, $start, $openLength, $bodyLength, $closeLength, $ty, List(")
      if (children.nonEmpty) {
        sb.append("\n")
      }
      for (child <- children) {
        sb.append("  ")
        sb.append(child.toString)
        sb.append(",\n")
      }
      sb.append("))")
      sb.toString
    }

  }

  object State {
    val orderByStart: Ordering[State] = Ordering.by(_.start)
  }
}

/**
 * Implements a universal text indenter. That is, an indenter which
 * is language and syntax agnostic. It works simply by matching opening,
 * closing, and separating tokens and breaking lines where the length between
 * the open and closing tokens exceeds the maximum line width.
 */
class UniversalIndenter(config: UniversalIndenter.Config) {
  import UniversalIndenter.*

  var tokenPositions: mutable.HashMap[(Pattern, TokenType), Option[State]] =
    config.tokens.map(x => (x(0), x(1)) -> None).to(mutable.HashMap)

  var stringPos = 0
  var string: CharSequence = null
  var currentToken: State = State(
    None,
    0,
    TokenType.Open,
    0,
  )

  def newToken(parent: State, ty: TokenType, m: MatchResult): State = {
    State(
      Some(parent),
      m.start,
      ty,
      m.end - m.start,
    )
  }

  def getLiteralToken(next: Option[State]): Option[State] = {

    val nextStart = next.fold(string.length - stringPos)(_.start)
    if (nextStart > stringPos) {
      val literalToken = State(
        Some(currentToken),
        stringPos,
        TokenType.Literal,
        nextStart - stringPos
      )
      Some(literalToken)
    } else {
      None
    }
  }

  def advanceToken() = {
    for (((re, ty), st) <- tokenPositions) {
      if (st.fold(-1)(_.start) < stringPos) {
        val matcher = re.matcher(string)

        val result = if (matcher.find(stringPos)) {
          val parent = ty match {
            case TokenType.Close => currentToken.parent.getOrElse(currentToken)
            case _ => currentToken
          }
          Some(newToken(parent, ty, matcher.toMatchResult))
        } else {
          None
        }

        tokenPositions((re, ty)) = result
      }
    }

    val next = tokenPositions.values.flatten.minOption(State.orderByStart)
    val lit = getLiteralToken(next)
    lit.map(currentToken.add(_))
    next.map(currentToken.add(_))
    currentToken = next.orElse(lit).getOrElse(throw Exception("advancing token returned neither literal nor token"))
    stringPos += currentToken.openLength
    println(currentToken.parent)
  }

  def indent(s: String): Iterator[String] = {
    string = s
    advanceToken()
    advanceToken()

    Iterator()
  }
}
