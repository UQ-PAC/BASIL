package util
import java.time.LocalDateTime

/**
 * Bounded list for storing a trace of at most [[bound]] items.
 */
class RingTrace[T](bound: Int, val name: String = "") {

  private var log = Vector[(String, T)]()

  def add(elem: T) = {
    val time = LocalDateTime.now().toString
    log = log.prepended((time, elem))
    log = log.take(bound)
  }

  def trace = log

  override def toString() =
    s"$name backtrace (most recent event last)\n    " + log.map((t, v) => s"$t : $v").mkString("\n    ")

}
