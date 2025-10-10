package util

import org.antlr.v4.runtime.misc.{Interval, ParseCancellationException}

import scala.util.{Failure, Success, Try}

/**
 * Executes the given computation, catching and pretty-printing any Antlr
 * [[org.antlr.v4.runtime.InputMismatchException]] which it might throw.
 *
 * Antlr parse errors are printed along with the relevant input line,
 * if possible.
 */
def catchAntlrParseErrors[T](body: => T) = {
  try {
    body
  } catch {
    case e: ParseCancellationException =>
      val extra = e.getCause match {
        case mismatch: org.antlr.v4.runtime.InputMismatchException =>
          Try {
            val token = mismatch.getOffendingToken
            val line = token.getLine()
            val col = token.getCharPositionInLine()
            assert(token.getStartIndex() >= 0)
            val lineStartIndex = token.getStartIndex() - token.getCharPositionInLine()

            val lineText = token.getInputStream().getText(new Interval(lineStartIndex, token.getStopIndex()))

            val vocabulary = mismatch.getRecognizer().getVocabulary()
            val expected = mismatch.getExpectedTokens().toString(vocabulary)

            val tokenName = vocabulary.getDisplayName(token.getType())
            val sourceName = token.getTokenSource().getSourceName()

            f"""
            Antlr parse error

            encountered token: $tokenName%s ${token.getText()}%s
            expected token(s): $expected%s

            $sourceName%s:$line%d:${col + 1}%d (truncated)
            $line%4d | $lineText
                   ${" " * col}^ unexpected token
            """.strip.stripLeading
          }
        case o => Failure(new Exception())
      }

      extra match {
        case Success(extra) => throw new ParseCancellationException(extra, e.getCause)
        case Failure(_) => throw e
      }
  }
}
