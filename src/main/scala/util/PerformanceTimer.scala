package util
import scala.collection.mutable
import scala.collection


case class PerformanceTimer(timerName: String = "") {
  private var lastCheckpoint: Long = System.currentTimeMillis()
  private var end: Long = 0
  private val checkpoints: mutable.Map[String, Long] = mutable.HashMap()

  def checkPoint(name: String): Long = {
      val delta = elapsed()
      lastCheckpoint = System.currentTimeMillis()
      checkpoints.put(name, delta)
      Logger.info(s"PerformanceTimer $timerName [$name]: ${delta}ms")
      delta
  }
  private def elapsed() :  Long = {
      System.currentTimeMillis() - lastCheckpoint
  }

  def checkPoints(): scala.collection.Map[String, Long] = checkpoints

}
