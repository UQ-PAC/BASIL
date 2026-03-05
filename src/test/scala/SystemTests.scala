import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest.*
import test_util.{BASILTest, CaptureOutput, Histogram, TestConfig, TestCustomisation}
import util.DSAPhase.TD
import util.boogie_interaction.*
import util.{
  DSConfig,
  DebugDumpIRLogger,
  LogLevel,
  Logger,
  MemoryRegionsMode,
  PerformanceTimer,
  StaticAnalysisConfig,
  MemoryEncodingRepresentation
}

import java.io.File
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */

trait SystemTests extends AnyFunSuite, CaptureOutput, BASILTest, TestCustomisation {

  /**
   * A suffix appended to output file names, in order to avoid clashes between test suites.
   */
  def testSuiteSuffix: String = "_" + this.getClass.getSimpleName

  case class TestResult(
    name: String,
    passed: Boolean,
    verified: Boolean,
    shouldVerify: Boolean,
    hasExpected: Boolean,
    timedOut: Boolean,
    matchesExpected: Boolean,
    translateTime: Long,
    verifyTime: Long
  ) {
    val toCsv =
      s"$name,$passed,$verified,$shouldVerify,$hasExpected,$timedOut,$matchesExpected,$translateTime,$verifyTime"
  }

  Logger.setLevel(LogLevel.WARN)
  DebugDumpIRLogger.setLevel(LogLevel.OFF)

  object TestResult {
    val csvHeader =
      "testCase,passed,verified,shouldVerify,hasExpected,timedOut,matchesExpected,translateTime,verifyTime"
  }

  val testResults: ArrayBuffer[TestResult] = ArrayBuffer()

  private val testPath = s"${BASILTest.rootDirectory}/src/test/"

  override def customiseTestsByName(name: String): Mode = Mode.Normal

  override def withFixture(test: NoArgTest) = {
    import gtirb.GTIRBReadELF.RelfCompatibilityLevel.*

    val checkRelf = test.name match {
      // Throw by default for gtsrelf/oldrelf mismatches
      case _ => Exception
    }
    gtirb.GTIRBReadELF.relfCompatibilityLevel.withValue(checkRelf) {
      super.withFixture(test)
    }
  }

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

  /** Writes test result data into .csv and .md files named according to given filename.
    */
  def summary(filename: String): Unit = {
    val csv: String = TestResult.csvHeader + System
      .lineSeparator() + testResults.map(r => s"${r.toCsv}").mkString(System.lineSeparator())
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

    info(
      s"Test summary: $numSuccess succeeded, $numFail failed: $numVerified verified, $numCounterexample did not verify (including $numTimeout timeouts)."
    )
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
      "stdDevVerifyTime" -> stdDevVerifyTime.toInt
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
      .map(_ ++ _)
      .mkString("", System.lineSeparator, System.lineSeparator)

    writeToFile(partMarkdown, testPath + "summary-" + filename + ".md.part")
    writeToFile(leftMarkdown, testPath + "headers.md.part") // XXX likely not thread-safe
    writeToFile(summaryMarkdown, testPath + "summary-" + filename + ".md")
  }

  def runTest(path: String, name: String, variation: String, conf: TestConfig): Unit = {
    val directoryPath = path + "/" + name + "/"
    val variationPath = directoryPath + variation + "/" + name
    val suiteSuffix = testSuiteSuffix

    // input files:
    val inputPath = variationPath + (if conf.useBAPFrontend then ".adt" else ".gts")
    val specPath = directoryPath + name + ".spec"
    val RELFPath = variationPath + ".relf"

    // output files:
    val lifterString = if conf.useBAPFrontend then s"_bap" else s"_gtirb"
    val BPLPath = variationPath + lifterString + suiteSuffix + ".bpl"
    val resultPath = variationPath + lifterString + suiteSuffix + "_result.txt"

    // reference file:
    val expectedOutPath = if conf.useBAPFrontend then variationPath + ".expected" else variationPath + "_gtirb.expected"

    val testSuffix = if conf.useBAPFrontend then ":BAP" else ":GTIRB"

    Logger.info(s"$name/$variation$testSuffix")
    val timer = PerformanceTimer(s"test $name/$variation$testSuffix")
    runBASIL(
      inputPath,
      RELFPath,
      Some(specPath),
      BPLPath,
      conf.staticAnalysisConfig,
      conf.simplify,
      conf.summariseProcedures,
      dsa = conf.dsa,
      memoryTransform = conf.memoryTransform,
      memoryEncoding = conf.memoryEncoding,
      useOfflineLifterForGtirbFrontend = conf.useOfflineLifterForGtirbFrontend
    )
    val translateTime = timer.checkPoint("translate-boogie")
    Logger.info(s"$name/$variation$testSuffix DONE")

    val boogieOutput = runBoogie(directoryPath, BPLPath, conf.boogieFlags)

    val verifyTime = timer.checkPoint("verify")
    val boogieResult = parseOutput(boogieOutput)

    BASILTest.writeToFile(boogieOutput, resultPath)
    val (boogieFailureMsg, verified, timedOut) = checkVerify(boogieResult, conf.expectVerify)

    if (boogieFailureMsg.isDefined) {
      info(boogieResult.toString)

      for (e <- boogieResult.errors) {
        info(s"Failing assertion ${e.fileName}:${e.line}")
        for (msg <- e.formattedAssertionSnippet) {
          info(msg.trim + "\n")
        }
      }
    }

    val (hasExpected, matchesExpected) = if (conf.checkExpected) {
      checkExpected(expectedOutPath, BPLPath)
    } else {
      (false, false)
    }

    val passed = boogieFailureMsg.isEmpty
    if (conf.logResults) {
      val result = TestResult(
        s"$name/$variation$testSuffix",
        passed,
        verified,
        conf.expectVerify,
        hasExpected,
        timedOut,
        matchesExpected,
        translateTime,
        verifyTime
      )
      testResults.synchronized {
        testResults.append(result)
      }
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

@test_util.tags.StandardSystemTest
class SystemTestsBAP extends SystemTests {
  override def testSuiteSuffix = ""
  runTests("correct", TestConfig(useBAPFrontend = true, expectVerify = true, checkExpected = true, logResults = true))
  runTests(
    "incorrect",
    TestConfig(useBAPFrontend = true, expectVerify = false, checkExpected = true, logResults = true)
  )
  test("summary-BAP") {
    summary("testresult-BAP")
  }
}

@test_util.tags.StandardSystemTest
class SystemTestsGTIRB extends SystemTests {
  override def testSuiteSuffix = ""
  runTests("correct", TestConfig(useBAPFrontend = false, expectVerify = true, checkExpected = true, logResults = true))
  runTests(
    "incorrect",
    TestConfig(useBAPFrontend = false, expectVerify = false, checkExpected = true, logResults = true)
  )
  test("summary-GTIRB") {
    summary("testresult-GTIRB")
  }
}

@test_util.tags.StandardSystemTest
class SystemTestsGTIRBOfflineLifter extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = true,
      checkExpected = true,
      logResults = true,
      useOfflineLifterForGtirbFrontend = true
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = false,
      checkExpected = true,
      logResults = true,
      useOfflineLifterForGtirbFrontend = true
    )
  )
  test("summary-GTIRB") {
    summary("testresult-GTIRBOfflineLifter")
  }
}

@test_util.tags.Slow
@test_util.tags.StandardSystemTest
class ExtraSpecTests extends SystemTests {
  override def testSuiteSuffix = ""

  override def customiseTestsByName(name: String): Mode = super.customiseTestsByName(name).orElse {
    name match {
      case _ => Mode.Retry("timeout issues")
    }
  }

  // some of these tests have time out issues so they need more time, but some still time out even with this for unclear reasons
  private val timeout = 60
  private val boogieFlags: Seq[String] = Seq("/proverOpt:O:smt.array.extensional=false")
  runTests(
    "extraspec_correct",
    TestConfig(
      timeout = timeout,
      baseBoogieFlags = boogieFlags,
      useBAPFrontend = true,
      expectVerify = true,
      checkExpected = true,
      logResults = true
    )
  )
  runTests(
    "extraspec_correct",
    TestConfig(
      timeout = timeout,
      baseBoogieFlags = boogieFlags,
      useBAPFrontend = false,
      expectVerify = true,
      checkExpected = true,
      logResults = true
    )
  )
  runTests(
    "extraspec_incorrect",
    TestConfig(
      timeout = timeout,
      baseBoogieFlags = boogieFlags,
      useBAPFrontend = true,
      expectVerify = false,
      checkExpected = true,
      logResults = true
    )
  )
  runTests(
    "extraspec_incorrect",
    TestConfig(
      timeout = timeout,
      baseBoogieFlags = boogieFlags,
      useBAPFrontend = false,
      expectVerify = false,
      checkExpected = true,
      logResults = true
    )
  )
  test("summary-extraspec") {
    summary("testresult-extraspec")
  }
}

@test_util.tags.DisabledTest
class NoSimplifySystemTests extends SystemTests {
  runTests("correct", TestConfig(simplify = false, useBAPFrontend = true, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify = false, useBAPFrontend = true, expectVerify = false, logResults = true))
  runTests("correct", TestConfig(simplify = false, useBAPFrontend = false, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify = false, useBAPFrontend = false, expectVerify = false, logResults = true))
  test("summary-nosimplify") {
    summary("nosimplify")
  }
}

@test_util.tags.AnalysisSystemTest2
@test_util.tags.AnalysisSystemTest
class SimplifySystemTests extends SystemTests {
  runTests("correct", TestConfig(simplify = true, useBAPFrontend = true, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify = true, useBAPFrontend = true, expectVerify = false, logResults = true))
  runTests("correct", TestConfig(simplify = true, useBAPFrontend = false, expectVerify = true, logResults = true))
  runTests("incorrect", TestConfig(simplify = true, useBAPFrontend = false, expectVerify = false, logResults = true))
  test("summary-simplify") {
    summary("simplify")
  }
}

@test_util.tags.AnalysisSystemTest4
@test_util.tags.AnalysisSystemTest
class SimplifyMemorySystemTests extends SystemTests {

  override def customiseTestsByName(name: String) = super.customiseTestsByName(name).orElse {
    name match {
      case "correct/malloc_with_local3/clang:BAP" =>
        Mode.TempFailure(
          "previous failure was: Expected verification success, but got failure. Failing assertion is: assert (load37_1 == R30_in)"
        )
      case _ => Mode.Normal
    }
  }

  // Logger.setLevel(LogLevel.DEBUG)
  val staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA))
  runTests(
    "correct",
    TestConfig(
      simplify = true,
      useBAPFrontend = true,
      expectVerify = true,
      logResults = true,
      staticAnalysisConfig = staticAnalysisConfig
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      simplify = true,
      useBAPFrontend = true,
      expectVerify = false,
      logResults = true,
      staticAnalysisConfig = staticAnalysisConfig
    )
  )
  runTests(
    "correct",
    TestConfig(
      simplify = true,
      useBAPFrontend = false,
      expectVerify = true,
      logResults = true,
      staticAnalysisConfig = staticAnalysisConfig
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      simplify = true,
      useBAPFrontend = false,
      expectVerify = false,
      logResults = true,
      staticAnalysisConfig = staticAnalysisConfig
    )
  )
  test("summary-simplify-mem") {
    summary("simplify-mem")
  }
}

@test_util.tags.AnalysisSystemTest
class AnalysisSystemTestsBAP extends SystemTests {
  runTests(
    "correct",
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = true)
  )
  runTests(
    "incorrect",
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = false)
  )
}

@test_util.tags.AnalysisSystemTest
class AnalysisSystemTestsGTIRB extends SystemTests {
  runTests(
    "correct",
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = false, expectVerify = true)
  )
  runTests(
    "incorrect",
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = false, expectVerify = false)
  )
}

@test_util.tags.AnalysisSystemTest
class DSAMemoryRegionSystemTestsBAP extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA)),
      useBAPFrontend = true,
      expectVerify = true
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA)),
      useBAPFrontend = true,
      expectVerify = false
    )
  )
}

@test_util.tags.AnalysisSystemTest
class DSAMemoryRegionSystemTestsGTIRB extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA)),
      useBAPFrontend = false,
      expectVerify = true
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA)),
      useBAPFrontend = false,
      expectVerify = false
    )
  )
}

@test_util.tags.DisabledTest
class MRAMemoryRegionSystemTestsBAP extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.MRA)),
      useBAPFrontend = true,
      expectVerify = true
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.MRA)),
      useBAPFrontend = true,
      expectVerify = false
    )
  )
}

@test_util.tags.DisabledTest
class MRAMemoryRegionSystemTestsGTIRB extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.MRA)),
      useBAPFrontend = false,
      expectVerify = true
    )
  )
  runTests(
    "incorrect",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.MRA)),
      useBAPFrontend = false,
      expectVerify = false
    )
  )
}

@test_util.tags.StandardSystemTest
class MemoryRegionTestsDSA extends SystemTests {

  override def customiseTestsByName(name: String) = super.customiseTestsByName(name).orElse {
    name match {
      case "memory_regions/stack_pointer/clang:BAP" | "memory_regions/stack_pointer/clang_pic:BAP" =>
        Mode.Disabled("stack_pointer currently times out because Boogie is bad at handling abstract map accesses")
      case _ => Mode.Normal
    }
  }

  runTests(
    "memory_regions",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.DSA)),
      useBAPFrontend = true,
      expectVerify = true
    )
  )
}

@test_util.tags.DisabledTest
class MemoryRegionTestsMRA extends SystemTests {

  override def customiseTestsByName(name: String) = super.customiseTestsByName(name).orElse {
    name match {
      case "memory_regions/stack_pointer/clang:BAP" | "memory_regions/stack_pointer/clang_pic:BAP" =>
        Mode.Disabled("stack_pointer currently times out because Boogie is bad at handling abstract map accesses")
      case _ => Mode.Normal
    }
  }

  runTests(
    "memory_regions",
    TestConfig(
      staticAnalysisConfig = Some(StaticAnalysisConfig(memoryRegions = MemoryRegionsMode.MRA)),
      useBAPFrontend = true,
      expectVerify = true
    )
  )
}

@test_util.tags.DisabledTest
class MemoryRegionTestsNoRegion extends SystemTests {
  runTests(
    "memory_regions",
    TestConfig(staticAnalysisConfig = Some(StaticAnalysisConfig()), useBAPFrontend = true, expectVerify = true)
  )
}

@test_util.tags.UnitTest
class ProcedureSummaryTests extends SystemTests {
  runTests(
    "procedure_summaries",
    TestConfig(summariseProcedures = true, simplify = true, useBAPFrontend = true, expectVerify = true)
  )
  runTests(
    "procedure_summaries",
    TestConfig(summariseProcedures = true, simplify = true, useBAPFrontend = false, expectVerify = true)
  )
}

// tests that require currently unimplemented functionality to pass
@test_util.tags.DisabledTest
class UnimplementedTests extends SystemTests {
  runTests("unimplemented", TestConfig(useBAPFrontend = false, expectVerify = true))
  runTests("unimplemented", TestConfig(useBAPFrontend = true, expectVerify = false))
}

@test_util.tags.AnalysisSystemTest4
@test_util.tags.AnalysisSystemTest
class IntervalDSASystemTests extends SystemTests {
  runTests("correct", TestConfig(useBAPFrontend = false, expectVerify = true, simplify = true, dsa = Some(DSConfig())))
  runTests(
    "incorrect",
    TestConfig(useBAPFrontend = false, expectVerify = false, simplify = true, dsa = Some(DSConfig()))
  )
}

@test_util.tags.AnalysisSystemTest
class IntervalDSASystemTestsSplitGlobals extends SystemTests {
  runTests(
    "correct",
    TestConfig(useBAPFrontend = false, expectVerify = true, simplify = true, dsa = Some(DSConfig(TD, true, true)))
  )

  runTests(
    "dsa/correct",
    TestConfig(useBAPFrontend = false, expectVerify = true, simplify = true, dsa = Some(DSConfig(TD, true, true)))
  )
  runTests(
    "incorrect",
    TestConfig(useBAPFrontend = false, expectVerify = false, simplify = true, dsa = Some(DSConfig(TD, true, true)))
  )
}

@test_util.tags.AnalysisSystemTest
class IntervalDSASystemTestsEqClasses extends SystemTests {
  runTests(
    "correct",
    TestConfig(useBAPFrontend = false, expectVerify = true, simplify = true, dsa = Some(DSConfig(TD, eqClasses = true)))
  )
  runTests(
    "incorrect",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = false,
      simplify = true,
      dsa = Some(DSConfig(TD, eqClasses = true))
    )
  )
}

@test_util.tags.DisabledTest
class MemoryTransformSystemTests extends SystemTests {
  runTests(
    "correct",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = false,
      simplify = true,
      dsa = Some(DSConfig()),
      memoryTransform = true
    )
  )

  runTests(
    "incorrect",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = false,
      simplify = true,
      dsa = Some(DSConfig()),
      memoryTransform = true
    )
  )
}

@test_util.tags.StandardSystemTest
class MemoryEncodingSystemTests extends SystemTests {
  private val timeout = 240

  runTests(
    "memory_encoding/correct",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = true,
      memoryEncoding = Some(MemoryEncodingRepresentation.Flat),
      simplify = true,
      timeout = timeout
    )
  )

  runTests(
    "memory_encoding/incorrect",
    TestConfig(
      useBAPFrontend = false,
      expectVerify = false,
      memoryEncoding = Some(MemoryEncodingRepresentation.Flat),
      simplify = true,
      timeout = timeout
    )
  )
}
