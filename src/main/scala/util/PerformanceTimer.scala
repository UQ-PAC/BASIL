package util
import java.util.concurrent.atomic.AtomicLong
import scala.collection
import scala.collection.mutable

case class RegionTimer(name: String) {
  private val total: AtomicLong = AtomicLong(0)

  def within[T](body: => T): T = {
    val begin = System.currentTimeMillis()
    val result = body
    val finish = System.currentTimeMillis()
    val _ = total.addAndGet(finish - begin)
    result
  }

  def getTotal(): Long = total.get

  override def toString() = {
    s"$name : ${getTotal()} (ms)"
  }
}

case class PerformanceTimer(
  timerName: String = "",
  logLevel: LogLevel = LogLevel.DEBUG,
  logger: GenericLogger = Logger
) {
  private var lastCheckpoint: Long = System.currentTimeMillis()
  private var end: Long = 0
  private val checkpoints: mutable.Map[String, Long] = mutable.HashMap()
  private var trace = RingTrace[(String, Long)](1000, s"timer:$timerName")

  OnCrash.register(trace)

  def checkPoint(name: String): Long = {
    val delta = elapsed()
    lastCheckpoint = System.currentTimeMillis()
    checkpoints.put(name, delta)
    trace.add((name, delta))
    logLevel match {
      case LogLevel.DEBUG => logger.debug(s"timer:$timerName [$name]: ${delta}ms")
      case LogLevel.INFO => logger.info(s"timer:$timerName [$name]: ${delta}ms")
      case LogLevel.WARN => logger.warn(s"timer:$timerName [$name]: ${delta}ms")
      case LogLevel.ERROR => logger.error(s"timer:$timerName [$name]: ${delta}ms")
      case _ => ???
    }
    delta
  }
  def elapsed(): Long = {
    System.currentTimeMillis() - lastCheckpoint
  }

  def checkPoints(): scala.collection.Map[String, Long] = checkpoints

}
