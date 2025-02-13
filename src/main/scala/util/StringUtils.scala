package util

import scala.collection.immutable.{LazyList, Seq}
import scala.collection.{IterableOps, Factory}
import scala.collection.{ AbstractIterator, AbstractView, BuildFrom }
import scala.collection.generic.IsSeq

type Twine = LazyList[String]

def indent(ss: Iterable[String], prefix: String = "  "): Twine =
  ss.take(1).to(LazyList) ++
    ss.sliding(2).map(_.toSeq).flatMap {
      case Seq(_) => LazyList()
      case Seq(prev,s) => if (prev.endsWith("\n")) LazyList(prefix, s) else LazyList(s)
      case x => throw new AssertionError(s"sliding(2) returned unexpected length: ${x.length}")
    }

def indentNested(head: String, elems: Iterable[Iterable[String]], tail: String, newline: String = "\n", sep: String = ",", hasPreceding: Boolean = false): Twine =

  val offset = if (hasPreceding) 1 else 0

  def makeElem(x: Iterable[String], i: Int): Iterable[String] =
    val s = (if (offset + i > 0) Iterable(sep) else Iterable.empty)
    s ++ Iterable(newline) ++ x

  val indented = indent(elems.zipWithIndex.flatMap(makeElem.tupled))

  val body = if (indented.isEmpty) indented else (indented :+ newline)

  head +: body :+ tail


// copied from: https://docs.scala-lang.org/overviews/core/custom-collection-operations.html
extension [Repr](coll: Repr)(using seq: IsSeq[Repr])
  def intersperse[B >: seq.A, That](sep: B)(using bf: BuildFrom[Repr, B, That]): That =
    val seqOps = seq(coll)
    bf.fromSpecific(coll)(new AbstractView[B]:
      def iterator = new AbstractIterator[B]:
        val it = seqOps.iterator
        var intersperseNext = false
        def hasNext = intersperseNext || it.hasNext
        def next() =
          val elem = if intersperseNext then sep else it.next()
          intersperseNext = !intersperseNext && it.hasNext
          elem
    )
