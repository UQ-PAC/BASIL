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
      // println(s"adding $s to $this")
      children = children :+ s
      s
    }

    private val newline = raw"\n".r

    override def toString: String = {
      val sb = StringBuilder()
      val p = parent match {
        case None => "None"
        case Some(_) => "<parent>"
      }
      sb.append(s"State($p, $start, $ty, $openLength, $bodyLength, $closeLength, $multiline, List(")
      if (children.nonEmpty) {
        sb.append("\n")
      }
      for (child <- children) {
        sb.append("  ")
        sb.append(newline.replaceAllIn(child.toString, "\n  "))
        sb.append(",\n")
      }
      sb.append("))")
      sb.toString
    }

    def parentOrSelf = parent.getOrElse(this)

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

  val tokenPositions: mutable.HashMap[(Pattern, TokenType), Option[State]] =
    config.tokens.map(x => (x(0), x(1)) -> None).to(mutable.HashMap)

  var stringPos = 0
  var lineLength = 0
  var sinceLastSep = 0
  var string: CharSequence = null
  var currentOpen: State = State(
    None,
    0,
    TokenType.Open,
    0,
  )

  def newToken(ty: TokenType, m: MatchResult): State = {
    State(
      None,
      m.start,
      ty,
      m.end - m.start,
    )
  }

  def getLiteralToken(next: Option[State]): Option[State] = {

    val nextStart = next.fold(string.length - stringPos)(_.start)
    if (nextStart > stringPos) {
      val literalToken = State(
        Some(currentOpen),
        stringPos,
        TokenType.Literal,
        nextStart - stringPos
      )
      Some(literalToken)
    } else {
      None
    }
  }

  def becomeMultiline(token: State): Unit = {
    if (token.multiline) return;
    token.multiline = true
    token.parent.foreach(becomeMultiline)
  }

  def advanceToken() = {
    for (((re, ty), st) <- tokenPositions) {
      if (st.fold(-1)(_.start) < stringPos) {
        val matcher = re.matcher(string)

        val result = if (matcher.find(stringPos)) {
          Some(newToken(ty, matcher.toMatchResult))
        } else {
          None
        }

        tokenPositions((re, ty)) = result
      }
    }

    val next = tokenPositions.values.flatten.minOption(State.orderByStart).map(x => {
      val parent = x.ty match {
        case TokenType.Close => currentOpen.parentOrSelf
        case _ => currentOpen
      }
      x.copy(parent=Some(parent))
    })
    val lit = getLiteralToken(next)
    lit.map(x => x.parent.get.add(x))
    next.map(x => x.parent.get.add(x))
    val lastToken = next.orElse(lit).getOrElse(throw Exception("advancing token returned neither literal nor token"))
    // println(lit)
    // println(next)
    // println()

    val newDistance = lastToken.start + lastToken.openLength - stringPos
    stringPos += newDistance

    if (lastToken.ty == TokenType.Separator) {
      sinceLastSep = 0
    } else if (lastToken.ty == TokenType.Open) {
      sinceLastSep = lastToken.openLength
    } else {
      sinceLastSep += newDistance
    }

    if (lineLength + newDistance > config.maxWidth) {
      becomeMultiline(currentOpen)
      lineLength = sinceLastSep
    }

    if (lastToken.ty == TokenType.Open) {
      currentOpen = lastToken
    } else if (lastToken.ty == TokenType.Close) {
      currentOpen = lastToken.parentOrSelf
    }
  }

  def tokenise(): State = {
    while (stringPos < string.length) {
      advanceToken()
    }
    currentOpen
  }

  def print(s: State, multiline: Boolean, depth: Int = -1): Unit = {
    s.ty match {
      case TokenType.Close if multiline => {
        Console.out.print("\n" + "  " * depth)
      }
      case _ => ()
    }
    Console.out.print(string.subSequence(s.start, s.start + s.openLength))
    // TODO: an Open or a Separator should both insert newline + indent after them
    s.ty match {
      case TokenType.Separator | TokenType.Open if multiline => {
        val d = depth + (if (s.ty == TokenType.Open) then 1 else 0)
        Console.out.print("\n" + "  " * d)
      }
      case _ => ()
    }

    s.ty match {
      case TokenType.Open => {
        for (x <- s.children) {
          print(x, x.multiline, depth + 1)
          x.ty match {
            case TokenType.Separator if multiline => {
              Console.out.print("\n" + "  " * (depth + 1))
            }
            case _ => ()
          }
        }
      }
      case _ => ()
    }
  }

  def indent(s: String): Iterator[String] = {
    println(s)
    string = s
    tokenise()
    println(currentOpen)
    print(currentOpen, currentOpen.multiline)

    Iterator()
  }
}
