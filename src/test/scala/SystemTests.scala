import org.scalatest.funsuite.AnyFunSuite
import util.{Logger, PerformanceTimer}


import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.*

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */

trait SystemTests extends AnyFunSuite {
  val testPath = "./src/test/"
  val correctPath = "./src/test/correct"
  val correctPrograms: Array[String] = getSubdirectories(correctPath)
  val incorrectPath = "./src/test/incorrect"
  val incorrectPrograms: Array[String] = getSubdirectories(incorrectPath)

  case class TestResult(passed: Boolean, verified: Boolean, shouldVerify: Boolean, hasExpected: Boolean, timedOut: Boolean, matchesExpected: Boolean, translateTime: Long, verifyTime: Long) {
    val toCsv = s"$passed,$verified,$shouldVerify,$hasExpected,$timedOut,$matchesExpected,$translateTime,$verifyTime"
  }

  object TestResult {
    val csvHeader = "passed,verified,shouldVerify,hasExpected,timedOut,matchesExpected,translateTime,verifyTime"
  }

  val testResults: mutable.ArrayBuffer[(String, TestResult)] = mutable.ArrayBuffer()

  def runTests(programs: Array[String], path: String, name: String, shouldVerify: Boolean, useADT: Boolean): Unit = {
    // get all variations of each program
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(t =>
        test(name + "/" + p + "/" + t) {
          runTest(path, p, t, shouldVerify, useADT)
        }
      )
    }
  }

  def summary(): Unit = {
    val csv: String = "testCase," + TestResult.csvHeader + System.lineSeparator() + testResults.map(r => s"${r._1},${r._2.toCsv}").mkString(System.lineSeparator())
    log(csv, testPath + "testResults.csv")

    val numVerified = testResults.count(_._2.verified)
    val numCounterexample = testResults.count(x => !x._2.verified && !x._2.timedOut)
    val numSuccess = testResults.count(_._2.passed)
    val numFail = testResults.count(!_._2.passed)
    val numTimeout = testResults.count(_._2.timedOut)
    val verifying = testResults.filter(x => !x._2.timedOut && x._2.verified).map(_._2.verifyTime)
    val counterExamples = testResults.filter(x => !x._2.timedOut && !x._2.verified).map(_._2.verifyTime)

    info(s"Test summary: $numSuccess succeeded, $numFail failed: $numVerified verified, $numCounterexample did not verify (including $numTimeout timeouts).")
    if (verifying.nonEmpty)
      info(s"Average time to verify: ${verifying.sum / verifying.size}")
    if (counterExamples.nonEmpty)
      info(s"Average time to counterexample: ${counterExamples.sum / counterExamples.size}")

    val summaryHeader = "passedCount,failedCount,verifiedCount,counterexampleCount,timeoutCount,verifyTotalTime,counterexampleTotalTime"
    val summaryRow = s"$numSuccess,$numFail,$numVerified,${counterExamples.size},$numTimeout,${verifying.sum},${counterExamples.sum}"
    log(summaryHeader + System.lineSeparator() + summaryRow, testPath + "summary.csv")
  }

  def runTest(path: String, name: String, variation: String, shouldVerify: Boolean, useADT: Boolean): Unit = {
    val directoryPath = path + "/" + name + "/"
    val variationPath = directoryPath + variation + "/" + name
    val specPath = directoryPath + name + ".spec"
    val outPath = if useADT then variationPath + "_bap.bpl" else variationPath + "_gtirb.bpl"
    val inputPath = if useADT then variationPath + ".adt" else variationPath + ".gts"
    val RELFPath = variationPath + ".relf"
    Logger.info(outPath)
    val timer = PerformanceTimer(s"test $name/$variation")

    val args = mutable.ArrayBuffer("--input", inputPath, "--relf", RELFPath, "--output", outPath)
    if (File(specPath).exists) args ++= Seq("--spec", specPath)

    Main.main(args.toArray)
    val translateTime = timer.checkPoint("translate-boogie")
    Logger.info(outPath + " done")
    val extraSpec = List.from(File(directoryPath).listFiles()).map(_.toString).filter(_.endsWith(".bpl")).filterNot(_.endsWith(outPath))
    val boogieResult = (Seq("boogie", "/timeLimit:10", "/printVerifiedProceduresCount:0", "/useArrayAxioms", outPath) ++ extraSpec).!!
    val verifyTime = timer.checkPoint("verify")
    val resultPath = if useADT then variationPath + "bap_result.txt" else variationPath + "gtirb_result.txt"
    log(boogieResult, resultPath)
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    val proveFailed = boogieResult.contains("could not be proved")
    val timedOut = boogieResult.strip() contains "timed out"

    def xor(x: Boolean, y: Boolean): Boolean = (x || y) && !(x && y)

    val failureMsg = if timedOut then "SMT Solver timed out" else
      (verified, shouldVerify, xor(verified, proveFailed)) match {
        case (true, true, true) => "Test passed"
        case (false, false, true) => "Test passed"
        case (_, _, false) => "Prover error: unknown result: " + boogieResult
        case (true, false, true) => "Expected verification failure, but got success."
        case (false, true, true) => "Expected verification success, but got failure."
      }

    val expectedOutPath = if useADT then variationPath + ".expected" else variationPath + "_gtirb.expected"
    val hasExpected = File(expectedOutPath).exists
    var matchesExpected = true
    if (hasExpected) {
      if (!compareFiles(expectedOutPath, outPath)) {
        matchesExpected = false
        info("Warning: Boogie file differs from expected")
      }
    } else {
      info("Note: this test has not previously succeeded")
    }
    val passed = !timedOut && (verified == shouldVerify) && xor(verified, proveFailed)
    val result = TestResult(passed, verified, shouldVerify, hasExpected, timedOut, matchesExpected, translateTime, verifyTime)
    testResults.append((s"$name/$variation", result))
    if (!passed) fail(failureMsg)
  }

  def compareFiles(path1: String, path2: String): Boolean = {
    val source1 = Source.fromFile(path1)
    val source2 = Source.fromFile(path2)
    val lines1 = source1.getLines
    val lines2 = source2.getLines
    while (lines1.hasNext && lines2.hasNext) {
      val line1 = lines1.next()
      val line2 = lines2.next()
      if (line1 != line2) {
        source1.close
        source2.close
        return false
      }
    }
    if (lines1.hasNext || lines2.hasNext) {
      source1.close
      source2.close
      return false
    }

    source1.close
    source2.close
    true
  }

  /** @param directoryName
    * of the parent directory
    * @return
    * the names all subdirectories of the given parent directory
    */
  def getSubdirectories(directoryName: String): Array[String] = {
    File(directoryName).listFiles.filter(_.isDirectory).map(_.getName)
  }

  def log(text: String, path: String): Unit = {
    val writer = BufferedWriter(FileWriter(path, false))
    writer.write(text)
    writer.flush()
    writer.close()
  }

}

class SystemTestsBAP extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", true, true)
  runTests(incorrectPrograms, incorrectPath, "incorrect", false, true)
  test("summary") {
    summary()
  }
}

class SystemTestsGTIRB extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", true, false)
  runTests(incorrectPrograms, incorrectPath, "incorrect", false, false)
  test("summary") {
    summary()
  }
}
