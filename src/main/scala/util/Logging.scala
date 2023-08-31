package util
import sourcecode.Line, sourcecode.FileName
import scala.io.AnsiColor
import java.io.File as JFile 

enum LogLevel(val id: Int): 
  case DEBUG extends LogLevel(0) 
  case INFO extends LogLevel(1) 
  case WARN extends LogLevel(2) 
  case ERROR extends LogLevel(3) 


object Logger: 
  private var level: LogLevel = LogLevel.DEBUG
  import LogLevel.*

  def write(logLevel: LogLevel, arg: Any, line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {

    val colour = logLevel match
    case DEBUG => AnsiColor.RESET
    case INFO => AnsiColor.GREEN
    case WARN => AnsiColor.YELLOW
    case ERROR => AnsiColor.RED

    if (level.id <= logLevel.id) {
      System.err.println(s"[$colour$logLevel${AnsiColor.RESET}]\t $arg [${name.value}@${file.value}:${line.value}]")
    }
  }

  def warn(arg: Any)(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {
    write(LogLevel.WARN, arg, line, file, name)
  }

  def error(arg: Any)(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {
    write(LogLevel.ERROR, arg, line, file, name)
  }

  def debug(arg: Any)(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {
    write(LogLevel.DEBUG, arg, line, file, name)
  }

  def info(arg: Any)(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): Unit = {
    write(LogLevel.INFO, arg, line, file, name)
  }

  def setLevel(logLevel: LogLevel): Unit = {
    level = logLevel
  }


