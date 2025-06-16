package ir

trait DeepEquality {
  val debug = true
  def deepEquals(o: Object): Boolean
  def deepEqualsDbg(o: Object): Boolean = {
    val r = deepEquals(o)
    if (!r) {
      println(s"$this != $o")
    }
    r
  }
}

trait DefaultDeepEquality extends DeepEquality {
  override def deepEquals(o: Object): Boolean = {
    o == this
  }
}
