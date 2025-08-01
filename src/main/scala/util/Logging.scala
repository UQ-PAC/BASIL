package util
import sourcecode.{FileName, Line, Name}

import java.io.{File, PrintStream}
import scala.io.AnsiColor

import collection.mutable.HashSet

enum LogLevel(val id: Int):
  case DEBUG extends LogLevel(0)
  case INFO extends LogLevel(1)
  case WARN extends LogLevel(2)
  case ERROR extends LogLevel(3)
  case OFF extends LogLevel(4)

class GenericLogger(
  val name: String,
  val defaultLevel: LogLevel = LogLevel.INFO,
  defaultOutput: => PrintStream = Console.out,
  var ANSIColour: Boolean = true
) {

  val logTrace = RingTrace[String](50, "Logger: " + name)
  OnCrash.register(logTrace)

  private var _output = () => defaultOutput
  def output: PrintStream = _output()

  val children: HashSet[GenericLogger] = HashSet()

  import LogLevel.*

  private var level: LogLevel = defaultLevel

  def getLevel() = level

  private def setColour(value: Boolean, setChildren: Boolean = false): Unit = {
    ANSIColour = value
    if (setChildren) {
      for (c <- children) {
        c.setColour(value, setChildren)
      }
    }
  }

  def disableColour(setChildren: Boolean = false) = setColour(false, setChildren)
  def enableColour(setChildren: Boolean = false): Unit = setColour(true, setChildren)

  // WARNING: if using this private method, be very aware of the by-name "=>" PrintStream parameter.
  private def deriveLoggerInternal(sname: String, stream: => PrintStream): GenericLogger = {
    val l = GenericLogger(name + "." + sname, level, stream, ANSIColour)
    children.add(l)
    l
  }

  def deriveLogger(name: String, stream: PrintStream): GenericLogger =
    deriveLoggerInternal(name, stream)

  def deriveLogger(name: String, stream: () => PrintStream): GenericLogger =
    deriveLoggerInternal(name, stream())

  def deriveLogger(name: String, file: File): GenericLogger =
    deriveLogger(name, PrintStream(file))

  def deriveLogger(name: String): GenericLogger = deriveLoggerInternal(name, output)

  def setOutput(stream: PrintStream) = _output = () => stream
  def setOutput(streamProducer: () => PrintStream) = _output = streamProducer

  def writeToFile(file: File, content: => String)(implicit line: Line, filepos: FileName, name: Name) = {
    if (level.id < LogLevel.OFF.id) {
      this.debug(s"Writing $file")(line, filepos, name)
      val l = deriveLogger(file.getName(), file)
      l.print(content)
      l.close()
      children.remove(l)
    }
  }

  def print(s: String) = {
    if (level.id < LogLevel.OFF.id) {
      output.print(s)
    }
  }

  def println(s: String) = {
    if (level.id < LogLevel.OFF.id) {
      output.println(s)
    }
  }

  def close() = {
    output.close()
  }

  def flush() = {
    output.flush()
  }

  private def writeLog(logLevel: LogLevel, arg: => Any, line: Line, file: FileName, name: Name): Unit = {

    if (level.id <= logLevel.id) {
      val colour =
        if (!ANSIColour) then ""
        else
          logLevel match
            case DEBUG => AnsiColor.RESET
            case INFO => AnsiColor.GREEN
            case WARN => AnsiColor.YELLOW
            case ERROR => AnsiColor.RED
            case OFF => ???

      val showPosition = (logLevel, level) match
        case (_, DEBUG) => true
        case (ERROR, _) => true
        case (WARN, _) => true
        case (INFO, _) => false
        case (DEBUG, _) => false
        case (OFF, _) => ???

      val positionText = s" [${name.value}@${file.value}:${line.value}]"
      val position = if showPosition then positionText else ""

      val resetColour = if !ANSIColour then "" else AnsiColor.RESET
      val space = "  "
      val prefix = s"[$colour$logLevel${resetColour}]$space"
      val prefixNoCol = s"[$logLevel]$space"
      val text = arg.toString().replace("\n", "\n " + (" " * (logLevel.toString).length()) + "  " + space)

      logTrace.add(s"$prefixNoCol $text$positionText")
      output.println(s"$prefix $text$position")
    }
  }

  def warn(arg: => Any)(implicit line: Line, file: FileName, name: Name): Unit = {
    writeLog(LogLevel.WARN, arg, line, file, name)
  }

  def error(arg: => Any)(implicit line: Line, file: FileName, name: Name): Unit = {
    writeLog(LogLevel.ERROR, arg, line, file, name)
  }

  def debug(arg: => Any)(implicit line: Line, file: FileName, name: Name): Unit = {
    writeLog(LogLevel.DEBUG, arg, line, file, name)
  }

  def info(arg: => Any)(implicit line: Line, file: FileName, name: Name): Unit = {
    writeLog(LogLevel.INFO, arg, line, file, name)
  }

  def setLevel(logLevel: LogLevel, setChildren: Boolean = true): GenericLogger = {
    level = logLevel
    if (setChildren) {
      for (c <- children) {
        c.setLevel(logLevel, setChildren)
      }
    }
    this
  }

  def findLoggerByName(s: String): Option[GenericLogger] = allLoggers.find(_.name == s)

  def allLoggers: Iterable[GenericLogger] = {
    (for {
      c <- children
      cc <- c.allLoggers
    } yield (cc))
      ++ Seq(this)
  }

}

// doesn't work with `mill run`
def isAConsole = System.console() != null

val Logger = GenericLogger("log", LogLevel.DEBUG, Console.out, isAConsole).setLevel(LogLevel.INFO, true)
val StaticAnalysisLogger = Logger.deriveLogger("analysis")
val SimplifyLogger = Logger.deriveLogger("simplify")
val DebugDumpIRLogger = Logger.deriveLogger("debugdumpir").setLevel(LogLevel.OFF)
val AnalysisResultDotLogger = Logger.deriveLogger("analysis-results-dot").setLevel(LogLevel.OFF)
val VSALogger = StaticAnalysisLogger.deriveLogger("vsa")
val MRALogger = StaticAnalysisLogger.deriveLogger("mra").setLevel(LogLevel.INFO)
val SteensLogger = StaticAnalysisLogger.deriveLogger("steensgaard")
val ProcedureSummariesLogger = StaticAnalysisLogger.deriveLogger("procedure-summaries")
// DSA Loggers
val DSALogger = Logger.deriveLogger("DSA").setLevel(LogLevel.WARN)
val ConstGenLogger = DSALogger.deriveLogger("Constraint Gen", Console.out).setLevel(LogLevel.OFF)
val SVALogger = DSALogger.deriveLogger("SVA").setLevel(LogLevel.OFF)
val IntervalDSALogger = DSALogger.deriveLogger("SadDSA", Console.out).setLevel(LogLevel.OFF)
val condPropDebugLogger = SimplifyLogger.deriveLogger("inlineCond")
val StackLogger = Logger.deriveLogger("Stack").setLevel(LogLevel.OFF)
val tvLogger = Logger.deriveLogger("TranslationValidation").setLevel(LogLevel.ERROR)
