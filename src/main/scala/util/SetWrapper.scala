package util
import scala.collection.immutable.*

case class SetWrapper[T](v: List[T], els: Option[Set[T]] = None)
    extends AbstractSet[T]
//    with IterableFactoryDefaults[T, SetWrapper]
//    with SetOps[T, SetWrapper, SetWrapper[T]]
    {

  private lazy val elems = els.getOrElse(Set.from(v))

  def iterator: Iterator[T] = v.iterator
  def contains(e: T) = elems.contains(e)

  def excl(e: T) = {
    if (elems.contains(e)) {
      SetWrapper(v.filterNot(_.equals(e)), Some(elems - e))
    } else {
      this
    }
  }

  def incl(e: T) = {
    if (elems.contains(e)) {
      this
    } else {
      SetWrapper(e :: v, Some(elems + e))
    }
  }

}
