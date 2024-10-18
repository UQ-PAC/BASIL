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
    runBASIL(inputPath, RELFPath, Some(specPath), BPLPath, conf.staticAnalysisConfig, conf.simplify)
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


class NoSimplifySystemTests extends SystemTests {
  runTests("correct", TestConfig(simplify=false, useBAPFrontend = true, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify=false, useBAPFrontend = true, expectVerify = false, logResults = true))
  runTests("correct", TestConfig(simplify=false, useBAPFrontend = false, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify=false, useBAPFrontend = false, expectVerify = false, logResults = true))
  test("summary-nosimplify") {
    summary("nosimplify")
  }
}
class SimplifySystemTests extends SystemTests {
  runTests("correct", TestConfig(simplify=true, useBAPFrontend = true, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify=true, useBAPFrontend = true, expectVerify = false, logResults = true))
  runTests("correct", TestConfig(simplify=true, useBAPFrontend = false, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify=true, useBAPFrontend = false, expectVerify = false, logResults = true))
  test("summary-simplify") {
    summary("simplify")
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

// tests that require currently unimplemented functionality to pass
class UnimplementedTests extends SystemTests {
  runTests("unimplemented", TestConfig(useBAPFrontend = false, expectVerify = true))
  runTests("unimplemented", TestConfig(useBAPFrontend = true, expectVerify = false))
}
