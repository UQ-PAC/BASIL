package ir.dsl

import ir.*
import util.twine.Twine
import util.{StringEscape, intersperse}

/**
 * ToScala definition
 * ==================
 *
 * Trait supporting the conversion of objects to stringified Scala source code, suitable
 * for use verbatim in a .scala file. (In some cases, additional imports may be needed.)
 *
 * For example, the following Scala expressions evaluate to true:
 *
 *     "hi".toScala == "\"hi\""
 *     TrueLiteral.toScala == "TrueLiteral"
 *     123.toScala == "123"
 *     BigInt(123).toScala == "BigInt(\"123\")"
 *
 * The main functionality is available as a .toScala extension method which returns a String.
 * Alternatively, the `toScalaLines` method returns a Twine instead of a String, allowing for
 * more efficient manipulation and indentation. A toScala implementation will then
 * be automatically derived. See the definition of `Twine` for more details.
 *
 * NOTE: Classes seeking to implement ToScala should instead implement one of
 * ToScalaLines or ToScalaString, depending on whether they expect to return multiple
 * lines or only one line.
 */
sealed trait ToScala[-T]:
  extension (x: T)
    def toScala: String
    def toScalaLines: Twine

/**
 * Implements ToScala in terms of toScalaLines. For correct indentation, this *must*
 * be used if a class wants to return multi-line output for its ToScala.
 */
trait ToScalaLines[-T] extends ToScala[T]:
  extension (x: T)
    def toScalaLines: Twine
    final override def toScala = x.toScalaLines.mkString

/**
 * Implements ToScala in terms of toScala, for classes whose ToScala output always fits within
 * one line.
 */
trait ToScalaString[-T] extends ToScala[T]:
  extension (x: T)
    def toScala: String
    final override def toScalaLines: Twine = Twine(x.toScala)

/**
 * Companion object for ToScala, defining functions and classes to help with
 * constructing and deriving instances. The apply method can also be used to
 * summon an instance.
 */
object ToScala {
  export ToScalaDeriving.{Make, MakeString, derived, deriveWithExclusions}
  def apply[T](using inst: ToScala[T]) = inst
}

/**
 * Additional ToScala instances
 * ----------------------------
 * Implements ToScala for basic scalar types and derives instances for
 * collection types.
 */

given ToScalaString[Nothing] with
  extension (x: Nothing) def toScala: String = x
given ToScalaString[String] with
  extension (x: String) def toScala: String = StringEscape.quote(x)
given ToScalaString[Int] with
  extension (x: Int) def toScala: String = x.toString()
given ToScalaString[Boolean] with
  extension (x: Boolean) def toScala: String = x.toString()
given ToScalaString[BigInt] with
  extension (x: BigInt) def toScala: String = s"BigInt(${x.toString.toScala})"

given [T](using ToScala[T]): ToScalaLines[Seq[T]] with
  extension (x: Seq[T])
    def toScalaLines =
      Twine.indentNested("Seq(", x.view.map(_.toScalaLines), ")")

given [T](using ToScala[T]): ToScalaLines[Set[T]] with
  extension (x: Set[T])
    def toScalaLines =
      Twine.indentNested("Set(", x.view.map(_.toScalaLines), ")")

given [K, V](using ToScala[K], ToScala[V]): ToScalaLines[Map[K, V]] with
  extension (x: Map[K, V])
    def toScalaLines =
      val pairs = x.view.map { case (k, v) =>
        Twine(k.toScalaLines, " -> ", v.toScalaLines)
      }
      Twine.indentNested("Map(", pairs, ")")

given [T](using ToScala[T]): ToScalaString[Some[T]] with
  extension (x: Some[T])
    def toScala: String = x match
      case Some(x) => s"Some(${x.toScala})"

given ToScalaString[None.type] with
  extension (x: None.type) def toScala: String = "None"

given [T](using ToScala[T]): ToScala[Option[T]] = ToScala.derived

given [K, V](using ToScala[K])(using ToScala[V]): ToScalaString[(K, V)] with
  extension (x: (K, V))
    def toScala: String =
      val (a, b) = x
      s"${a.toScala} -> ${b.toScala}"
