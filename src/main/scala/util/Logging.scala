package logging 
import sourcecode.Line, sourcecode.File
import scala.io.AnsiColor

enum LogLevel(val id: Int): 
  case DEBUG extends LogLevel(0) 
  case INFO extends LogLevel(1) 
  case WARNING extends LogLevel(2) 
  case ERROR extends LogLevel(3) 



object Logger: 
  private var level: LogLevel = LogLevel.DEBUG
  import LogLevel.*

  def write(logLevel: LogLevel, arg: String, line: sourcecode.Line, file: sourcecode.File) = {

    val colour = logLevel match
    case DEBUG => AnsiColor.RESET
    case INFO => AnsiColor.GREEN
    case WARNING => AnsiColor.YELLOW
    case ERROR => AnsiColor.RED

    if (level.id <= logLevel.id) {
      println(s"[${colour}${logLevel}${AnsiColor.RESET}] ${arg} (${file.value}:${line.value})")
    }
  }

  def  warn(arg: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    write(LogLevel.WARNING, arg, line, file)
  }

  def  error(arg: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    write(LogLevel.ERROR, arg, line, file)
  }

  def  debug(arg: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    write(LogLevel.DEBUG, arg, line, file)
  }


  def info(arg: String)(implicit line: sourcecode.Line, file: sourcecode.File) = {
    write(LogLevel.INFO, arg, line, file)
  }

  def setLevel(logLevel: LogLevel) = {
    level = logLevel;
  }


