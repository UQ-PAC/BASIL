package test_util

import test_util.BASILTest.{mean, stdDev}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Try, Using}

case class Histogram(bins: List[Int], numBins: Int, minBin: Double, maxBin: Double) {

  def toSvg(title: String, imgWidth: Int, imgHeight: Int): String = {
    def template(width: Int = 300, height: Int = 130, content: String) =
      s""" <svg width="$width" height="$height" xmlns="http://www.w3.org/2000/svg">
      $content
    </svg> """

    def mkRect(width: Int, height: Int, x: Int, y: Int, crx: Int = 0, cry: Int = 0, fill: String = "Black") = {
      s"""<rect width="$width" height="$height" x="$x" y="$y" rx="$crx" ry="$cry" fill="$fill" />"""
    }

    def text(content: String, x: Int, y: Int, cssClass: String = "small") = {
      s"""<text x="$x" y="$y" class="$cssClass">$content</text>"""
    }

    val leftMargin = 20
    val histWidth = imgWidth - leftMargin
    val bottomMargin = 20
    val topMargin = 20
    val histHeight = imgHeight - topMargin - bottomMargin
    val maxHeight = bins.max
    val binWidth: Double = histWidth.doubleValue / bins.size
    val heightScaling: Double = histHeight.doubleValue / maxHeight
    val binPos = (0 to bins.size).map(i => (leftMargin + i * binWidth, binWidth * (i + 1)))
      .zip(bins.map(bh => heightScaling * bh))

    val rects = binPos.map((binXX, height) =>
      mkRect(binWidth.ceil.intValue, height.intValue, binXX(0).floor.intValue, histHeight.intValue - height.intValue + topMargin))

    val labels = {
      (text(title, imgWidth / 8, topMargin - 5),
        text("0", 0, histHeight + topMargin),
        text(maxHeight.toString, 0, topMargin),
        text(minBin.toInt.toString, 0, imgHeight),
        text(maxBin.toInt.toString, (binWidth * bins.size).intValue - leftMargin, imgHeight))
    }

    val bg = mkRect(imgWidth, imgHeight, 0, 0, fill = "White")

    val content = (Seq(bg) ++ rects ++ labels.toList).mkString("\n")
    template(imgWidth, imgHeight, content)
  }

}

object Histogram {
  def apply(numBins: Int, xs: Seq[Double], bounds: Option[(Double, Double)] = None): Histogram = {
    val meanX = mean(xs)
    val stdDevX = stdDev(xs)
    val minB = Seq(0, meanX - 2.25 * stdDevX, xs.min).max
    val maxB = Seq(meanX + 2.25 * stdDevX, xs.max).min
    val (minBin, maxBin) = bounds.getOrElse(minB, maxB)
    val binSize = ((maxBin - minBin) / numBins) * 1.000001
    val bins = (0 to numBins).map(x => (minBin + x * binSize, minBin + (x + 1) * binSize))
      .map((left, right) => xs.count(x => x >= left && x < right))
      .toList
    Histogram(bins, numBins, minBin, maxBin)
  }

  def load(path: String): Unit = {
    Using(Source.fromFile(path)) { source =>
      val sourceList = source.getLines().toList
      val headers = sourceList.head.split(",")

      val res = headers.map(h => h -> ArrayBuffer[String]()).toMap[String, ArrayBuffer[String]]

      sourceList.tail.foreach { line =>
        val cols = line.split(",")
        headers.zip(cols).foreach((h, v) => res(h).append(v))
      }
      val timeValues = res("verifyTime").map(_.toDouble)
      val histo = Histogram(50, timeValues.toSeq, Some(800.0, 1000.0))
      println(histo.toSvg("test histogram", 500, 300))
    }
  }
}