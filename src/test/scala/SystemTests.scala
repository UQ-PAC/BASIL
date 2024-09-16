import util.{Logger, PerformanceTimer, StaticAnalysisConfig}

import Numeric.Implicits.*
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.io.Source
import scala.util.{Try, Using}
import scala.sys.process.*
import test_util.BASILTest
import test_util.BASILTest.*
import test_util.TestConfig

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */

trait SystemTests extends BASILTest {
  case class TestResult(passed: Boolean, verified: Boolean, shouldVerify: Boolean, hasExpected: Boolean, timedOut: Boolean, matchesExpected: Boolean, translateTime: Long, verifyTime: Long) {
    val toCsv = s"$passed,$verified,$shouldVerify,$hasExpected,$timedOut,$matchesExpected,$translateTime,$verifyTime"
  }

  object TestResult {
    val csvHeader = "passed,verified,shouldVerify,hasExpected,timedOut,matchesExpected,translateTime,verifyTime"
  }

  val testResults: mutable.ArrayBuffer[(String, TestResult)] = mutable.ArrayBuffer()

  private val testPath = "./src/test/"

  def runTests(folder: String, conf: TestConfig): Unit = {
    val path = testPath + folder
    val programs = getSubdirectories(path)

    // get all variations of each program
    val testSuffix = if conf.useBAPFrontend then ":BAP" else ":GTIRB"
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach { t =>
        test(folder + "/" + p + "/" + t + testSuffix) {
          runTest(path, p, t, conf)
        }
      }
    }
  }

  /**
   * Writes test result data into .csv and .md files named according to given filename.
   */
  def summary(filename: String): Unit = {
    val csv: String = "testCase," + TestResult.csvHeader + System.lineSeparator() + testResults.map(r => s"${r(0)},${r(1).toCsv}").mkString(System.lineSeparator())
    writeToFile(csv, testPath + "full-" + filename + ".csv")

    val verifTimes = testResults.map(_(1).verifyTime.toDouble)

    val numVerified = testResults.count(_(1).verified)
    val numCounterexample = testResults.count(x => !x(1).verified && !x(1).timedOut)
    val numSuccess = testResults.count(_(1).passed)
    val numFail = testResults.count(!_(1).passed)
    val numTimeout = testResults.count(_(1).timedOut)
    val verifying = testResults.filter(x => !x(1).timedOut && x(1).verified).map(_(1).verifyTime)
    val counterExamples = testResults.filter(x => !x(1).timedOut && !x(1).verified).map(_(1).verifyTime)
    val medianVerifyTime = median(verifTimes)
    val meanVerifyTime = mean(verifTimes)
    val stdDevVerifyTime = stdDev(verifTimes)

    info(s"Test summary: $numSuccess succeeded, $numFail failed: $numVerified verified, $numCounterexample did not verify (including $numTimeout timeouts).")
    if (verifying.nonEmpty)
      info(s"Average time to verify: ${verifying.sum / verifying.size}")
    if (counterExamples.nonEmpty)
      info(s"Average time to counterexample: ${counterExamples.sum / counterExamples.size}")

    val summaryMap = ListMap(
      "passedCount" -> numSuccess,
      "failedCount" -> numFail,
      "verifiedCount" -> numVerified,
      "counterexampleCount" -> counterExamples.size,
      "timeoutCount" -> numTimeout,
      "verifyTotalTime" -> verifying.sum,
      "counterexampleTotalTime" -> counterExamples.sum,
      "meanVerifyTime" -> meanVerifyTime.toInt,
      "medianVerifyTime" -> medianVerifyTime.toInt,
      "stdDevVerifyTime" -> stdDevVerifyTime.toInt,
    )
    val summaryHeader = summaryMap.keys.mkString(",") + System.lineSeparator
    val summaryRow = summaryMap.values.mkString(",") + System.lineSeparator
    writeToFile(summaryHeader + summaryRow, testPath + "summary-" + filename + ".csv")

    // generates a verification time histogram
    val minB = Seq(0, meanVerifyTime - 2.25 * stdDevVerifyTime, verifTimes.min).max
    val maxB = Seq(meanVerifyTime + 2.25 * stdDevVerifyTime, verifTimes.max).min
    val nbins = 50

    val histo = histogram(nbins, Some(minB, maxB))(verifTimes.toSeq)
    val svgHistogram = histoToSvg(filename, 400,300, histo, minB, maxB)
    writeToFile(svgHistogram, testPath + "verifyTime-" + filename + ".svg")

    // generates a markdown table in separate parts.
    // the complete markdown file can be constructed by horizontal (line-wise)
    // concatenation of leftMarkdown and one or more partMarkdown.
    val mdMap = summaryMap + ("VerifyTimeHistogram" -> ("![](" + "HISTO" + filename + "HISTO" + ")"))
    val leftMarkdown =
      s"""
      || Metric |
      ||--------|
      |""".stripMargin
      + mdMap.map((k, _) => s"| $k |${System.lineSeparator}").mkString

    val partMarkdown =
      s"""
      | $filename |
      |-------|
      |""".stripMargin
      + mdMap.map((_, v) => s" $v |${System.lineSeparator}").mkString

    val summaryMarkdown = leftMarkdown.linesIterator
      .zip(partMarkdown.linesIterator)
      .map(_++_)
      .mkString("", System.lineSeparator, System.lineSeparator)

    writeToFile(partMarkdown, testPath + "summary-" + filename + ".md.part")
    writeToFile(leftMarkdown, testPath + "headers.md.part") // XXX likely not thread-safe
    writeToFile(summaryMarkdown, testPath + "summary-" + filename + ".md")
  }

  def runTest(path: String, name: String, variation: String, conf: TestConfig): Unit = {
    val directoryPath = path + "/" + name + "/"
    val variationPath = directoryPath + variation + "/" + name
    val inputPath = if conf.useBAPFrontend then variationPath + ".adt" else variationPath + ".gts"
    val BPLPath = if conf.useBAPFrontend then variationPath + "_bap.bpl" else variationPath + "_gtirb.bpl"
    val specPath = directoryPath + name + ".spec"
    val RELFPath = variationPath + ".relf"
    val resultPath = if conf.useBAPFrontend then variationPath + "_bap_result.txt" else variationPath + "_gtirb_result.txt"
    val testSuffix = if conf.useBAPFrontend then ":BAP" else ":GTIRB"
    val expectedOutPath = if conf.useBAPFrontend then variationPath + ".expected" else variationPath + "_gtirb.expected"

    Logger.info(BPLPath)
    val timer = PerformanceTimer(s"test $name/$variation$testSuffix")
    runBASIL(inputPath, RELFPath, Some(specPath), BPLPath, conf.staticAnalysisConfig)
    val translateTime = timer.checkPoint("translate-boogie")
    Logger.info(BPLPath + " done")

    val boogieResult = runBoogie(directoryPath, BPLPath, conf.boogieFlags)
    val verifyTime = timer.checkPoint("verify")
    val (boogieFailureMsg, verified, timedOut) = checkVerify(boogieResult, resultPath, conf.expectVerify)

    val (hasExpected, matchesExpected) = if (conf.checkExpected) {
      checkExpected(expectedOutPath, BPLPath)
    } else {
      (false, false)
    }

    val passed = boogieFailureMsg.isEmpty
    if (conf.logResults) {
      val result = TestResult(passed, verified, conf.expectVerify, hasExpected, timedOut, matchesExpected, translateTime, verifyTime)
      testResults.append((s"$name/$variation$testSuffix", result))
    }
    if (!passed) fail(boogieFailureMsg.get)
  }

}

class SystemTestsBAP extends SystemTests {
  runTests("correct", TestConfig(useBAPFrontend = true, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(useBAPFrontend = true, expectVerify = false, logResults = true))
  test("summary-BAP") {
    summary("testresult-BAP")
  }
}

class SystemTestsGTIRB extends SystemTests {
  runTests("correct", TestConfig(useBAPFrontend = false, expectVerify = true, checkExpected = true, logResults = true))
  runTests("incorrect", TestConfig(useBAPFrontend = false, expectVerify = false, checkExpected = true, logResults = true))
  test("summary-GTIRB") {
    summary("testresult-GTIRB")
  }
}

class AnalysisSystemTestsBAP extends SystemTests {
  runTests("correct", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = true))
  runTests("incorrect", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = false))
}

class AnalysisSystemTestsGTIRB extends SystemTests {
  runTests("correct", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = false, expectVerify = true))
  runTests("incorrect", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = false, expectVerify = false))
}

class ProcedureSummaryTests extends SystemTests {
  // TODO currently procedure_summary3 verifies despite incorrect procedure summary analysis
  // this is due to BASIL's currently limited handling of non-returning calls
  runTests("procedure_summaries", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig(summariseProcedures = true)),
    useBAPFrontend = true, expectVerify = true))
  runTests("procedure_summaries", TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig(summariseProcedures = true)),
    useBAPFrontend = false, expectVerify = true))
}

def mean(xs: Iterable[Double]): Double = xs.sum / xs.size

def variance(xs: Iterable[Double]): Double = {
  val avg = mean(xs)

  xs.map(a => math.pow(a - avg, 2)).sum / xs.size
}

def median(xs: Iterable[Double]): Double = xs.toArray.sorted.apply(xs.size / 2)

def stdDev(xs: Iterable[Double]): Double = math.sqrt(variance(xs))

def histogram(numBins: Int, bounds: Option[(Double, Double)] = None)(xs: Seq[Double]): List[Int] = {
  val (mn, mx) = bounds.getOrElse(xs.min, xs.max)
  val binSize = ((mx - mn) / numBins) * 1.000001
  val counts = (0 to numBins).map(x => (mn + x * binSize, mn + (x + 1) * binSize))
    .map((left, right) => xs.count(x => x >= left && x < right))
    .toList
  counts
}

def histoToSvg(title: String, imgWidth: Int, imgHeight: Int, bins: List[Int], minBin: Double, maxBin: Double) : String = {
  def template(width: Int = 300, height: Int = 130, content: String) =
    s""" <svg width="$width" height="$height" xmlns="http://www.w3.org/2000/svg">
    $content
  </svg> """
  def mkRect(width: Int, height: Int, x: Int, y: Int, crx: Int=0, cry: Int=0, fill: String="Black") = {
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
      text(maxBin.toInt.toString, (binWidth*bins.size).intValue - leftMargin, imgHeight))
  }

  val bg = mkRect(imgWidth, imgHeight, 0, 0, fill="White")

  val content = (Seq(bg) ++ rects ++ labels.toList).mkString("\n")
  template(imgWidth, imgHeight, content)
}


def loadHisto(): Unit = {
  Using(Source.fromFile("src/test/full-testresult-GTIRB.csv")) { source =>
    val sourceList = source.getLines().toList
    val headers = sourceList.head.split(",")

    val res = headers.map(h => h -> mutable.ArrayBuffer[String]()).toMap[String, mutable.ArrayBuffer[String]]

    sourceList.tail.foreach { line =>
      val cols = line.split(",")
      headers.zip(cols).foreach((h, v) => res(h).append(v))
    }
    val timeValues = res("verifyTime").map(_.toDouble)
    val histo = histogram(50, Some(800.0, 1000.0))(timeValues.toSeq)
    println(histoToSvg("test histogram", 500, 300, histo, 800.0, 1000.0))
  }
}