package ir.dsl

import ir.*
import util.{Twine, StringEscape, indent, indentNested, intersperse}
import translating.{BasilIR, BasilIRExp}

import collection.immutable.{SortedMap}
import collection.immutable.{LazyList}
import collection.mutable
import collection.mutable.{LinkedHashSet}

/**
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
 * The main function provided is a .toScala extension method which returns a String.
 *
 * When defining ToScala[A], if the Scala source code will span multiple lines,
 * then `toScalaLines` should be implemented instead. This allows for more efficient
 * manipulation and indentation. See the definition of `Twine` for more details.
 *
 * In such cases, `toScalaLines` should be overriden and `toScalaUsingToScalaLines`
 * should be used to define `toScala`.
 *
 */
trait ToScala[-T]:
  protected def toScalaUsingToScalaLines(x: T): String = x.toScalaLines.mkString

  extension (x: T)
    def toScala: String

    // NOTE: default implementation does not check for newlines within the toScala output!
    // embedded newlines are not permitted by the Twine conventions.
    def toScalaLines: Twine = LazyList(x.toScala)


given ToScala[Block] with
  extension (x: Block)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      val commands = x.statements ++ LazyList(x.jump)
      indentNested(
        s"block(${x.label.toScala}",
        commands.to(LazyList).map(_.toScalaLines),
        ")",
        headSep = true
      )

given ToScala[Procedure] with
  extension (x: Procedure)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      indentNested(
        s"proc(${x.procName.toScala}",
        x.blocks.to(LazyList).map(_.toScalaLines),
        ")",
        headSep = true
      )

given ToScala[Program] with
  extension (x: Program)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      indentNested(
        "prog(",
        x.procedures.to(LazyList).map(_.toScalaLines),
        ")"
      )

given ToScala[String] with
  extension (x: String) override def toScala: String = StringEscape.quote(x)
given ToScala[Endian] with
  extension (x: Endian) override def toScala: String = "Endian." + x.toString()
given ToScala[Int] with
  extension (x: Int) override def toScala: String = x.toString()
given ToScala[Boolean] with
  extension (x: Boolean) override def toScala: String = x.toString()
given ToScala[BigInt] with
  extension (x: BigInt) override def toScala: String = s"BigInt(${x.toString.toScala})"


given [T](using ToScala[T]): ToScala[Seq[T]] with
  extension (x: Seq[T]) override def toScala: String = x match
    case Seq() => "Seq()"
    case Seq(x) => s"Seq(${x.toScala})"
    case _ => s"Seq(${x.map(_.toScala).mkString(", ")})"

given [T](using ToScala[T]): ToScala[LinkedHashSet[T]] with
  extension (x: LinkedHashSet[T]) override def toScala: String =
    s"LinkedHashSet(${x.map(_.toScala).mkString(", ")})"


given [T](using ToScala[T]): ToScala[Option[T]] with
  extension (x: Option[T]) override def toScala: String = x match
    case None => "None"
    case Some(x) => s"Some(${x.toScala})"

given [K,V](using ToScala[K])(using ToScala[V]): ToScala[SortedMap[K,V]] with
  extension (x: SortedMap[K,V]) override def toScala: String =
    val entries = x.map((a,b) => s"${a.toScala} -> ${b.toScala}").mkString(", ")
    s"SortedMap($entries)"
