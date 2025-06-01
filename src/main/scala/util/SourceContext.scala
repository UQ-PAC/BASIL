package util

import scala.io.Source

object SourcePrinting {
  def formatSource(source: Source, lineNum: Int, colNum: Option[Int], width: Option[Int] = None, contextSize: Int = 2, showMargin: Boolean = true) = {
    require(lineNum >= 1, "should be strictly positive")
    require(colNum.forall(_ >= 1), "should be strictly positive")
    require(width.forall(_ >= 1), "should be strictly positive")
    require(contextSize >= 0, "should be non-negative")

    def marginCaret(thisLine: Int) = colNum match {
      case None if thisLine == lineNum => ">"
      case _ => " "
    }
    def underlineAnnotation(thisLine: Int) = colNum match {
      case Some(colNum) if thisLine == lineNum =>
        val wd = Math.max(0, width.getOrElse(1) - 1)
        Some(" " * (colNum-1) + "^" + "~" * wd)
      case _ => None
    }

    val lower = Math.max(1, lineNum - contextSize)
    val upper = lineNum + contextSize

    val lines = source.getLines()
      .zipWithIndex
      .drop(lower - 1)
      .take(upper - lower + 1)
      .map {
        case (a,b) => (a,b+1)
      }
      .toList

    val numWidth = (lineNum + contextSize).toString.length
    val fmt = s"%${numWidth}d"

    def leftMargin(thisLine: Int) =
      if (showMargin) then
        marginCaret(thisLine) + " " + fmt.format(thisLine) + " | "
      else
        ""

    val out = lines.flatMap {
      case (line, n) =>
        val prefix = leftMargin(n)
        Seq(prefix + line) ++ underlineAnnotation(n).map(" " * prefix.size + _)
    }

    println(out.mkString("\n"))
  }

  def main(args: Array[String]) = {
    val src = Source.fromFile("/home/rina/progs/basil/src/main/scala/util/SourceContext.scala")
    formatSource(src, 1, Some(9), Some(4), 2, false)
  }

}

