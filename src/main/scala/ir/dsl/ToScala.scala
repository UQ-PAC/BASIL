package ir.dsl

import ir.*
import util.{Twine, StringEscape, indent, indentNested, intersperse}
import translating.{BasilIR, BasilIRExp}

import collection.immutable.{ListMap, SortedMap}
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
  def toScalaUsingToScalaLines(x: T): String = x.toScalaLines.mkString

  extension (x: T)
    def toScala: String

    // NOTE: default implementation does not check for newlines within the toScala output!
    // embedded newlines are not permitted by the Twine conventions.
    def toScalaLines: Twine = LazyList(x.toScala)


inline given ToScala[Block] with
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

object Internal {

  def procedureToScalaLinesWith(b: Block => Twine)(x: Procedure): Twine =
    indentNested(
      s"proc(${x.procName.toScala}",
      x.blocks.to(LazyList).map(b),
      ")",
      headSep = true
    )

  def programToScalaLinesWith(p: Procedure => Twine)(x: Program): Twine =
    indentNested(
      "prog(",
      x.procedures.to(LazyList).map(p),
      ")"
    )

}

class ToScalaWithSplitting {
  import ToScalaWithSplitting.*

  private var _decls: Map[String, Twine] = ListMap()

  def decls = _decls

  private def blockToScalaLines(x: Block): Twine =
    val name = blockName(x)
    _decls += name -> x.toScalaLines
    LazyList(name)

  protected def toScalaWithDecls[T](f: T => Twine)(name: String, x: T): Twine =

    // XXX: must be evaluated separately from the += operation!!
    // otherwise, new decls from blocks are overwritten
    val entry = (name -> f(x).force)

    _decls += entry

    // NOTE: scala compiler will error on duplicated names
    indentNested(
      "{",
      declsToScala(_decls) ++ Iterable(LazyList(name)),
      "}",
      sep = "\n"
    )

  protected def baseProcedureToScalaLines = Internal.procedureToScalaLinesWith(blockToScalaLines)
  protected def baseProgramToScalaLines = Internal.programToScalaLinesWith(baseProcedureToScalaLines)

  def toScalaLines(x: Procedure): Twine =
    toScalaWithDecls(baseProcedureToScalaLines)(x.name, x)

  def toScalaLines(x: Program): Twine =
    toScalaWithDecls(baseProgramToScalaLines)("program", x)
}

object ToScalaWithSplitting {

  def blockName(x: Block) = s"`block:${x.parent.name}.${x.label}`"

  def declsToScala(decls: Map[String, Twine]): Iterable[Twine] =
    decls.map((k,v) => s"def $k = " +: v)

  given ToScala[Program] with
    extension (x: Program)
      def toScala = toScalaUsingToScalaLines(x)
      override def toScalaLines = ToScalaWithSplitting().toScalaLines(x)

  given ToScala[Procedure] with
    extension (x: Procedure)
      def toScala = toScalaUsingToScalaLines(x)
      override def toScalaLines = ToScalaWithSplitting().toScalaLines(x)

}

given ToScala[Procedure] with
  extension (x: Procedure)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      Internal.procedureToScalaLinesWith(_.toScalaLines)(x)

given ToScala[Program] with
  extension (x: Program)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      Internal.programToScalaLinesWith(_.toScalaLines)(x)

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
