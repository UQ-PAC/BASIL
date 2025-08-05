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

trait DeepEqualityByFields extends DeepEquality {

  def deepEquals(o: Object) = o match {
    case x: DeepEqualityByFields =>
      deepEqualsVisit(Nil, deepEqualsConversion(), x.deepEqualsConversion()).size == 0
    case _ => false
  }

  override def deepEqualsDbg(o: Object) = o match {
    case x: DeepEqualityByFields =>
      val mismatching = deepEqualsVisit(Nil, deepEqualsConversion(), x.deepEqualsConversion())
      var ok = true
      if (DeepEquality.debug.value) {
        mismatching.foreach { mismatch =>
          ok = false
          println(s"... leading to $mismatch")
        }
      }
      ok
    case _ => false
  }

  protected def deepEqualsVisit(path: List[String], left: EqualityFields, right: EqualityFields): Iterator[List[String]] =
    val sentinel = ("(none)", None)
    left.iterator.zipAll(right, sentinel, sentinel).flatMap {
      case ((k1, v1), (k2, v2)) if k1 != k2 =>
        Some(path)
      case ((k, v1), (_, v2)) => (v1, v2) match {
        case (l: Option[?], r: Option[?]) => Option.unless(l == r)(path)
        case (l: DeepEqualityByFields, r: DeepEqualityByFields) =>
          deepEqualsVisit(k :: path, l.deepEqualsConversion(), r.deepEqualsConversion())
        case (l: DeepEquality, r: DeepEquality) =>
          Option.unless(l.deepEqualsDbg(r))(path)
        case _ => Some(path)
      }
    }

  type EqualityFields = Iterable[(String, DeepEquality | Option[Any])]

  def deepEqualsConversion(): EqualityFields

  protected def convertToMap[K](prefix: String, x: IterableOnce[K], key: K => String): EqualityFields =

    def convert(x: Any) = x match {
      case x: DeepEquality => x
      case x => Some(x)
    }

    val values = x.iterator.map(x => s"$prefix[${key(x)}]" -> convert(x)).toList.sortBy(_._1)
    (s"$prefix.length", Some(values.length)) +: values


}
