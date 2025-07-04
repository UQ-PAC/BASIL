package ir

import scala.util.DynamicVariable

object DeepEquality {

  /**
   * If set, enables debug printing of failing [[DeepEquality#deepEqualsDbg]]
   * checks. This is a dynamic variable, so its value can be overridden for
   * a certain lexical scope, then it is returned to its previous value when
   * existing the scope.
   *
   * To temporarily set the value, use [[scala.util.DynamicVariable#withValue]].
   * See [[scala.util.DynamicVariable]] for more details.
   */
  val debug = DynamicVariable(false)
}

trait DeepEquality {
  def deepEquals(o: Object): Boolean
  def deepEqualsDbg(o: Object): Boolean = {
    val r = deepEquals(o)
    if (DeepEquality.debug.value && !r) {
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
