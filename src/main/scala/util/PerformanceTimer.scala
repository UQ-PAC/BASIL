package util
import scala.collection.mutable
import scala.collection

case class PerformanceTimer(timerName: String = "", logLevel: LogLevel = LogLevel.DEBUG) {
  private var lastCheckpoint: Long = System.currentTimeMillis()
  private var end: Long = 0
  private val checkpoints: mutable.Map[String, Long] = mutable.HashMap()

  def checkPoint(name: String): Long = {
    val delta = elapsed()
    lastCheckpoint = System.currentTimeMillis()
    checkpoints.put(name, delta)
    logLevel match {
      case LogLevel.DEBUG => Logger.debug(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      case LogLevel.INFO  => Logger.info(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      case LogLevel.WARN  => Logger.warn(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      case LogLevel.ERROR => Logger.error(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      case _              => ???
    }
    delta
  }
  def elapsed(): Long = {
    System.currentTimeMillis() - lastCheckpoint
  }

  def checkPoints(): scala.collection.Map[String, Long] = checkpoints

}
