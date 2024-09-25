import org.scalatest.funsuite.AnyFunSuite
import util.{LogLevel, Logger, PerformanceTimer, StaticAnalysisConfig}

import Numeric.Implicits.*
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.*
import test_util.BASILTest
import test_util.BASILTest.*
import test_util.Histogram
import test_util.TestConfig

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */

trait SystemTests extends AnyFunSuite, BASILTest {
  case class TestResult(name: String, passed: Boolean, verified: Boolean, shouldVerify: Boolean, hasExpected: Boolean, timedOut: Boolean, matchesExpected: Boolean, translateTime: Long, verifyTime: Long) {
    val toCsv = s"$name,$passed,$verified,$shouldVerify,$hasExpected,$timedOut,$matchesExpected,$translateTime,$verifyTime"
  }

  object TestResult {
    val csvHeader = "testCase,passed,verified,shouldVerify,hasExpected,timedOut,matchesExpected,translateTime,verifyTime"
  }

  val testResults: ArrayBuffer[TestResult] = ArrayBuffer()

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
        val variationPath = programPath + "/" + t + "/" + p
        val inputPath = if conf.useBAPFrontend then variationPath + ".adt" else variationPath + ".gts"
        if (File(inputPath).exists) {
          test(folder + "/" + p + "/" + t + testSuffix) {
            runTest(path, p, t, conf)
          }
        }
      }
    }
  }

  /**
   * Writes test result data into .csv and .md files named according to given filename.
   */
  def summary(filename: String): Unit = {
    val csv: String = TestResult.csvHeader + System.lineSeparator() + testResults.map(r => s"${r.toCsv}").mkString(System.lineSeparator())
    writeToFile(csv, testPath + "full-" + filename + ".csv")

    val verifTimes = testResults.map(_.verifyTime.toDouble)

    val numVerified = testResults.count(_.verified)
    val numCounterexample = testResults.count(x => !x.verified && !x.timedOut)
    val numSuccess = testResults.count(_.passed)
    val numFail = testResults.count(!_.passed)
    val numTimeout = testResults.count(_.timedOut)
    val verifying = testResults.filter(x => !x.timedOut && x.verified).map(_.verifyTime)
    val counterExamples = testResults.filter(x => !x.timedOut && !x.verified).map(_.verifyTime)
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
    val histo = Histogram(50, verifTimes.toSeq)
    val svgHistogram = histo.toSvg(filename, 400, 300)
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

    Logger.info(s"$name/$variation$testSuffix")
    val timer = PerformanceTimer(s"test $name/$variation$testSuffix")
    runBASIL(inputPath, RELFPath, Some(specPath), BPLPath, conf.staticAnalysisConfig)
    val translateTime = timer.checkPoint("translate-boogie")
    Logger.info(s"$name/$variation$testSuffix DONE")

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
      val result = TestResult(s"$name/$variation$testSuffix", passed, verified, conf.expectVerify, hasExpected, timedOut, matchesExpected, translateTime, verifyTime)
      testResults.append(result)
    }
    if (!passed) fail(boogieFailureMsg.get)
  }

  def checkExpected(expectedOutPath: String, BPLPath: String): (Boolean, Boolean) = {
    val hasExpected = File(expectedOutPath).exists
    var matchesExpected = true
    if (hasExpected) {
      if (!BASILTest.compareFiles(expectedOutPath, BPLPath)) {
        matchesExpected = false
        info("Warning: Boogie file differs from expected")
      }
    } else {
      info("Note: this test has not previously succeeded")
    }
    (hasExpected, matchesExpected)
  }

}

class SystemTestsBAP extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", TestConfig(useBAPFrontend=true, expectVerify=true))
  runTests(incorrectPrograms, incorrectPath, "incorrect", TestConfig(useBAPFrontend=true, expectVerify=false))
  test("summary-BAP") {
    summary("testresult-BAP")
  }
}

class SystemTestsGTIRB extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", TestConfig(useBAPFrontend=false, expectVerify=true, BASILFlags = Seq("--analyse")))
  runTests(incorrectPrograms, incorrectPath, "incorrect", TestConfig(useBAPFrontend=false, expectVerify=false, BASILFlags = Seq("--analyse")))
  test("summary-GTIRB") {
    summary("testresult-GTIRB")
  }
}

class ExtraSpecTests extends SystemTests {
  // some of these tests have time out issues so they need more time, but some still time out even with this for unclear reasons
  val boogieFlags = Seq("/timeLimit:30", "/useArrayAxioms")
  runTests("extraspec_correct", TestConfig(boogieFlags = boogieFlags, useBAPFrontend = true, expectVerify = true, checkExpected = true, logResults = true))
  runTests("extraspec_correct", TestConfig(boogieFlags = boogieFlags, useBAPFrontend = false, expectVerify = true, checkExpected = true, logResults = true))
  runTests("extraspec_incorrect", TestConfig(boogieFlags = boogieFlags, useBAPFrontend = true, expectVerify = false, checkExpected = true, logResults = true))
  runTests("extraspec_incorrect", TestConfig(boogieFlags = boogieFlags, useBAPFrontend = false, expectVerify = false, checkExpected = true, logResults = true))
  test("summary-extraspec") {
    summary("testresult-extraspec")
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

def mean(xs: Iterable[Double]): Double = xs.sum.toDouble / xs.size

def variance(xs: Iterable[Double]): Double = {
  val avg = mean(xs)

  xs.map(a => math.pow(a - avg, 2)).sum / xs.size
}

def median(xs: Iterable[Double]) = xs.toArray.sorted.apply(xs.size / 2)

def stdDev(xs: Iterable[Double]): Double = math.sqrt(variance(xs))

def histogram(numBins: Int, bounds: Option[(Double, Double)] = None)(xs: Seq[Double]) : List[Int] = {
  val (mn, mx) = bounds.getOrElse(xs.min, xs.max)
  val binSize = ((mx - mn) / numBins) * (1.000001)
  val counts = (0 to numBins).map(x => (mn + x * binSize, mn + (x + 1) * binSize))
    .map((left, right) => xs.count(x => x >= left && x < right))
    .toList
  counts
}

def histoToSvg(title: String, imgWidth: Int, imgHeight: Int, bins: List[Int], minBin: Double, maxBin: Double) : String = {
  def template(width: Int = 300, height: Int = 130, content: String) =
    s""" <svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">
    ${content}
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
  val binWidth : Double = (histWidth).doubleValue / bins.size
  val heightScaling : Double =  (histHeight.doubleValue)/(maxHeight)
  val binPos = (0 to bins.size).map(i => (leftMargin + i * binWidth, binWidth * (i + 1)))
    .zip(bins.map(bh => heightScaling * bh))

  val rects = binPos.map((binXX, height) =>
    mkRect(binWidth.ceil.intValue, height.intValue, binXX._1.floor.intValue, histHeight.intValue - height.intValue + topMargin))

  val labels = {
    (text(title, imgWidth / 8, topMargin - 5),
      text("0", 0, histHeight + topMargin),
      text(maxHeight.toInt.toString, 0, topMargin),
      text(minBin.toInt.toString, 0, imgHeight),
      text(maxBin.toInt.toString, (binWidth*(bins.size)).intValue - leftMargin, imgHeight))
  }

  val bg = mkRect(imgWidth, imgHeight, 0, 0, fill="White")

  val content = (Seq(bg) ++ rects ++ labels.toList).mkString("\n")
  template(imgWidth, imgHeight, content)
}


def loadHisto() = {
  val source = scala.io.Source.fromFile("src/test/full-testresult-GTIRB.csv").getLines().toList
  val headers = source.head.split(",")

  val res = headers.map(h => h -> ArrayBuffer[String]()).toMap[String, ArrayBuffer[String]]

  source.tail.map(line => {
    val cols = line.split(",")
    headers.zip(cols).foreach((h,v) => res(h).append(v))
  })

  val timeValues = res("verifyTime").map(_.toDouble)
  val histo = histogram(50, Some(800.0, 1000.0))(timeValues.toSeq)
  println(histoToSvg("test histogram", 500, 300, histo, 800.0, 1000.0))
}
// tests that require currently unimplemented functionality to pass
class UnimplementedTests extends SystemTests {
  runTests("unimplemented", TestConfig(useBAPFrontend = false, expectVerify = true))
  runTests("unimplemented", TestConfig(useBAPFrontend = true, expectVerify = false))
}
