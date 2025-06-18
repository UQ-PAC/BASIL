package util
import java.time.LocalDateTime

object OnCrash {

  var logs = Map[String, () => String]()

  def register(name: String, printer: () => String): Unit = {
    logs = logs + (name -> printer)
  }

  def register[T](trace: RingTrace[T]): Unit = {
    register(trace.name, () => trace.toString)
  }

  def dumpLog(): String = {
    val fname = "BasilCrashLog-" + LocalDateTime.now() + ".log"

    val log = logs
      .map((name, logger) => {
        name + "\n" + "-".repeat(name.length) + "\n" + logger()
      })
      .mkString("\n\n")

    writeToFile(log, fname)
    Logger.error(s"Wrote crash dump to $fname")
    log
  }

}
