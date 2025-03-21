package util

class RingTrace[T](bound: Int, name: String = "") {

  private var log = Vector[T]()

  def add(elem: T) = {
    log = log.prepended(elem)
    log = log.take(bound)
  }

  def trace = log

  override def toString() = s"$name backtrace (most recent event last)\n" + log.mkString("\n")

}
