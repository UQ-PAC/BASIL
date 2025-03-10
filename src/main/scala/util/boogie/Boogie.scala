package util.boogie_interaction

enum BoogieResultKind {
  case Verified
  case Timeout
  case Unknown(string: String)
  case AssertionFailed
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

/*
 * Parse the output of the boogie tool and return a symbolic structure representing
 * the verification decision, and a list of errors.
 *
 * Assumes boogie is invoked with [/printVerifiedProceduresCount:0].
 */
def parseOutput(boogieStdout: String): BoogieResult = {
  println(boogieStdout)
  val verified = boogieStdout.strip().equals("Boogie program verifier finished with 0 errors")
  val proveFailed = boogieStdout.contains("could not be proved")
  val timedOut = boogieStdout.strip().contains("timed out")

  val errors = parseErrors(boogieStdout)

  val kind = if (timedOut) then {
    BoogieResultKind.Timeout
  } else if (verified) then {
    BoogieResultKind.Verified
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
