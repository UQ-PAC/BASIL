package util.boogie_interaction
import util.Logger

import scala.sys.process.*

enum BoogieResultKind {
  case Verified(count: Int = -1, errors: Int = -1)
  case Timeout
  case Unknown(string: String)
  case AssertionFailed

  def isVerified = this.isInstanceOf[BoogieResultKind.Verified]
}

case class BoogieResult(kind: BoogieResultKind, errors: Array[BoogieError]) {
  override def toString() = {
    s"Boogie result: $kind\n"
      + errors.flatMap(_.formattedAssertionSnippet).mkString("\n")
  }
}

case class BoogieLine(number: Int, text: String, isError: Boolean) {

  /**
   * Print this line in the format:
   *
   *     line number | line content
   *
   *  If this line reprsents a failing assertion (isError) then:
   *
   *   > line number | line content
   */
  def prettyPrint(numPadding: Integer = (number / 10 + 1)) = {
    val padded = number.toString().reverse.padTo(7, ' ').reverse
    val carat = if isError then " > " else "   "
    s"$carat ${number} | ${text}"
  }
}

/**
 * Information about a line containing a failing assertion.
 *
 * @param fileName
 *  The Boogie file the assertion is from
 * @param line
 *  the line number containing the assertion
 * @param errorSnippet
 *  List of surrounding lines in the Boogie file
 */
case class BoogieError(fileName: String, line: Int, errorSnippet: Option[List[BoogieLine]]) {
  def formattedAssertionSnippet: Option[String] = {
    for {
      x <- errorSnippet
      lineNoPadding = (x.map(_.number).max / 10) + 1
      header = s"Failing assertion: $fileName:$line\n"
    } yield (header + x.map(_.prettyPrint(lineNoPadding)).mkString("\n"))
  }
}

def parseVerifyMessage(m: String) = {
  val x = m.trim.split(" ").toList
  x match {
    case List(
          "Boogie",
          "program",
          "verifier",
          "finished",
          "with",
          verif: String,
          "verified,",
          errors: String,
          "errors"
        ) =>
      Some(BoogieResultKind.Verified(verif.toInt, errors.toInt))
    case _ if m.trim == "Boogie program verifier finished with 0 errors" => Some(BoogieResultKind.Verified(-1, 0))
    case _ => None
  }
}

/*
 * Parse the output of the boogie tool and return a symbolic structure representing
 * the verification decision, and a list of errors.
 *
 * Assumes boogie is invoked with [/printVerifiedProceduresCount:0].
 */
def parseOutput(boogieStdout: String): BoogieResult = {
  val verified = parseVerifyMessage(boogieStdout)
  val verif = parseVerifyMessage
  val proveFailed = boogieStdout.contains("could not be proved")
  val timedOut = boogieStdout.strip().contains("timed out")

  val errors = parseErrors(boogieStdout)

  val kind = if (timedOut) then {
    BoogieResultKind.Timeout
  } else if (verified.isDefined) then {
    verified.get
  } else if (proveFailed) then {
    BoogieResultKind.AssertionFailed
  } else {
    BoogieResultKind.Unknown(boogieStdout)
  }

  BoogieResult(kind, errors)
}

/**
 * Get a list of assertion failures from the boogie output.
 */
def parseErrors(boogieStdoutMessage: String, snippetContext: Int = 3): Array[BoogieError] = {
  val lines = boogieStdoutMessage.split('\n')
  lines.collect {
    case l
        if (
          l.endsWith(": Error: this assertion could not be proved") || l
            .contains("this is the postcondition that could not be proved")
        ) => {
      val b = l.trim()
      val parts = b.split("\\(").flatMap(_.split("\\)")).flatMap(_.split(","))
      val fname = parts(0)
      val line = Integer.parseInt(parts(1))
      val col = parts(2)

      val lines = util.readFromFile(fname).toArray

      val lineOffset = line - 1

      val beginLine = Integer.max(0, lineOffset - snippetContext)
      val endLine = Integer.min(lines.length, lineOffset + snippetContext)

      val boogieLines = (beginLine to endLine).map(x => {
        val isError = x == lineOffset
        val text = lines(x)
        val lineNo = x + 1
        BoogieLine(lineNo, text, isError)
      })

      val errorLines = (beginLine to endLine).map(x => {
        val carat = if x == lineOffset then " > " else "   "
        s"$carat ${x + 1} | ${lines(x)}"
      })

      val errorSnippet = errorLines.mkString("\n").trim
      BoogieError(fname, line, Some(boogieLines.toList))
    }
  }
}

def boogieBatchQuery(
  boogieFileNames: Iterable[String],
  proc: Option[String] = None,
  timeout: Int = 30,
  solver2Timeout: Option[Int] = None
) = {
  val procSelect = proc.toSeq.flatMap(p => Seq("/proc", p))
  val x = solver2Timeout.map(s => s"/proverOpt:C:combined_solver.solver2_timeout=$s").toSeq
  val boogieCmd =
    Seq("boogie", "/timeLimit", timeout.toString) ++ x ++ boogieFileNames ++ procSelect
  Logger.debug(s"Batch proving ${boogieCmd.mkString(" ")}")
  val output = boogieCmd.!!

  val res = parseOutput(output)
  Logger.debug(res)
  res
}

def getSMTSplitsFromFile(fname: String) = {
  val loadFile = util.readFromFile(fname)

  var queryStack = List[String]()
  var queryList = List[String]()
  var currentQuery = ""

  var first = true
  var depth = 0
  var initialSetup = ""
  var check_sat = false

  for (l <- loadFile) {
    val ln = l.trim.stripPrefix("(").stripSuffix(")").split(" ").toList
    ln match {
      case "check-sat" :: Nil => {
        currentQuery = currentQuery + "\n" + l

        check_sat = true
      }
      case "push" :: n :: Nil if (n.toInt != 1) => {
        throw Exception("Sequential push")
      }
      case "push" :: n :: Nil if (n.toInt == 1) => {
        if (depth > 0) {
          throw Exception("Sequential push")
        }
        if (first) {
          first = false
          initialSetup = currentQuery + "\n"
          currentQuery = initialSetup
        }
        depth += 1
      }
      case "pop" :: n :: Nil if (n.toInt != 1) => {
        throw Exception("Sequential pop")
      }
      case "pop" :: n :: Nil if (n.toInt == 1) => {
        check_sat = false
        depth -= 1
        if (depth != 0) {
          throw Exception("Sequential pop")
        }
        queryList = currentQuery :: queryList
        currentQuery = initialSetup
      }
      case _ => {
        currentQuery = currentQuery + "\n" + l
      }
    }
  }

  // if (check_sat) {
  //   queryList = currentQuery :: queryList
  // }
  queryList.reverse
}
