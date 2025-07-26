package util

/**
 * Mixin to derive a lexicographic ordering for a case class (specifically, defining the [[Ordered]] trait).
 * This requires existing [[Ordering]] instances for each of the case class's fields.
 */
trait ProductOrdered[T <: Product](using m: scala.deriving.Mirror.ProductOf[T], ord: Ordering[m.MirroredElemTypes])
    extends Ordered[T] {
  this: T =>

  private val ordering = Ordering.by((x: T) => Tuple.fromProductTyped(x))
  override def compare(other: T) = ordering.compare(this, other)
}
