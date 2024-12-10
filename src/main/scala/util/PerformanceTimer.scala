package util
import scala.collection.mutable
import scala.collection
import sourcecode.Line, sourcecode.FileName

case class PerformanceTimer(timerName: String = "") {
  private var lastCheckpoint: Long = System.currentTimeMillis()
  private var end: Long = 0
  private val checkpoints = mutable.ArrayBuffer[(String, String, Long)]()

  /* reset timer */
  def reset() = {
    lastCheckpoint = System.currentTimeMillis()
  }

  /** reset timer, log and return elapsed */
  def checkPoint(name: String)(implicit line: sourcecode.Line, file: sourcecode.FileName): Long = {
      val delta = elapsed()
      lastCheckpoint = System.currentTimeMillis()
      checkpoints.append((name, s"${file.value}:${line.value}", delta))
      Logger.info(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      delta
  }
  private def elapsed() :  Long = {
      System.currentTimeMillis() - lastCheckpoint
  }

  def checkPoints() = checkpoints.toList

}
