import org.scalatest.funsuite.AnyFunSuite
import util.{Logger, PerformanceTimer}

import Numeric.Implicits._
import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.*

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */


 case class TestConfig(val boogieFlags:Seq[String] = Seq("/timeLimit:10", "/useArrayAxioms"), val basilFlags:Seq[String] = Seq(), val useBAPFrontend: Boolean=true, val expectVerify: Boolean=true)


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

  def runTests(programs: Array[String], path: String, name: String, conf: TestConfig): Unit = {
    // get all variations of each program
    val testSuffix = if conf.useBAPFrontend then ":BAP" else ":GTIRB"
    for (p <- programs) {
      val programPath = path + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(t =>
        test(name + "/" + p + "/" + t + testSuffix) {
          runTest(path, p, t, conf)
        }
      )
    }
  }

  /**
   * Writes test result data into .csv and .md files named according to given filename.
   */
  def summary(filename: String): Unit = {
    val csv: String = "testCase," + TestResult.csvHeader + System.lineSeparator() + testResults.map(r => s"${r._1},${r._2.toCsv}").mkString(System.lineSeparator())
    log(csv, testPath + "full-" + filename + ".csv")

    val verifTimes = testResults.map(_._2.verifyTime.toDouble)

    val numVerified = testResults.count(_._2.verified)
    val numCounterexample = testResults.count(x => !x._2.verified && !x._2.timedOut)
    val numSuccess = testResults.count(_._2.passed)
    val numFail = testResults.count(!_._2.passed)
    val numTimeout = testResults.count(_._2.timedOut)
    val verifying = testResults.filter(x => !x._2.timedOut && x._2.verified).map(_._2.verifyTime)
    val counterExamples = testResults.filter(x => !x._2.timedOut && !x._2.verified).map(_._2.verifyTime)
    val medianVerifyTime = median(verifTimes)
    val meanVerifyTime = mean(verifTimes)
    val stdDevVerifyTime = stdDev(verifTimes)


    info(s"Test summary: $numSuccess succeeded, $numFail failed: $numVerified verified, $numCounterexample did not verify (including $numTimeout timeouts).")
    if (verifying.nonEmpty)
      info(s"Average time to verify: ${verifying.sum / verifying.size}")
    if (counterExamples.nonEmpty)
      info(s"Average time to counterexample: ${counterExamples.sum / counterExamples.size}")

    val summaryMap = collection.immutable.ListMap(
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
    log(summaryHeader + summaryRow, testPath + "summary-" + filename + ".csv")

    // generates a verification time histogram
    val minB = Seq(0, meanVerifyTime - 2.25 * stdDevVerifyTime, verifTimes.min).max
    val maxB = Seq(meanVerifyTime + 2.25 * stdDevVerifyTime, verifTimes.max).min
    val nbins = 50

    val histo = mkHistogram(nbins, Some(minB, maxB))(verifTimes.toSeq)
    val svgHistogram = histoToSvg(filename, 400,300, histo)
    log(svgHistogram, testPath + "verifyTime-" + filename + ".svg")


    // generates a markdown table in separate parts.
    // the complete markdown file can be constructed by horizontal (line-wise)
    // concatenation of leftMarkdown and one or more partMarkdown.
    val mdMap = summaryMap + ("VerifyTimeHistogram" -> ("![](" + "HISTO" + filename + "HISTO" + ")"))
    val leftMarkdown = 
      s"""
      || Metric |
      ||--------|
      |""".stripMargin
      + mdMap.map((k,_) => s"| $k |${System.lineSeparator}").mkString

    val partMarkdown =
      s"""
      | $filename |
      |-------|
      |""".stripMargin
      + mdMap.map((k,v) => s" $v |${System.lineSeparator}").mkString

    val summaryMarkdown = leftMarkdown.linesIterator
      .zip(partMarkdown.linesIterator)
      .map(_++_)
      .mkString("", System.lineSeparator, System.lineSeparator)

    log(partMarkdown, testPath + "summary-" + filename + ".md.part")
    log(leftMarkdown, testPath + "headers.md.part") // XXX likely not thread-safe
    log(summaryMarkdown, testPath + "summary-" + filename + ".md")
  }

  def runTest(path: String, name: String, variation: String, conf: TestConfig): Unit = {
    val shouldVerify = conf.expectVerify
    val useBAPFrontend = conf.useBAPFrontend

    val directoryPath = path + "/" + name + "/"
    val variationPath = directoryPath + variation + "/" + name
    val specPath = directoryPath + name + ".spec"
    val outPath = if useBAPFrontend then variationPath + "_bap.bpl" else variationPath + "_gtirb.bpl"
    val inputPath = if useBAPFrontend then variationPath + ".adt" else variationPath + ".gts"
    val RELFPath = variationPath + ".relf"
    Logger.info(outPath)
    val timer = PerformanceTimer(s"test $name/$variation")

    val args = mutable.ArrayBuffer("--input", inputPath, "--relf", RELFPath, "--output", outPath) ++ conf.basilFlags
    if (File(specPath).exists) args ++= Seq("--spec", specPath)

    Main.main(args.toArray)
    val translateTime = timer.checkPoint("translate-boogie")
    Logger.info(outPath + " done")
    val extraSpec = List.from(File(directoryPath).listFiles()).map(_.toString).filter(_.endsWith(".bpl")).filterNot(_.endsWith(outPath))
    val boogieCmd = (Seq("boogie", "/printVerifiedProceduresCount:0") ++ conf.boogieFlags ++ Seq(outPath) ++ extraSpec)
    Logger.info(s"Verifying... ${boogieCmd.mkString(" ")}")
    val boogieResult = boogieCmd.!!
    val verifyTime = timer.checkPoint("verify")
    val resultPath = if useBAPFrontend then variationPath + "_bap_result.txt" else variationPath + "_gtirb_result.txt"
    log(boogieResult, resultPath)
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    val proveFailed = boogieResult.contains("could not be proved")
    val timedOut = boogieResult.strip() contains "timed out"

    def xor(x: Boolean, y: Boolean): Boolean = (x || y) && !(x && y)

    val failureMsg = if timedOut then "SMT Solver timed out" else
      (verified, shouldVerify, xor(verified, proveFailed)) match {
        case (true, true, true) => "Test passed"
        case (false , false, true) => "Test passed"
        case (_, _, false) => "Prover error: unknown result: " + boogieResult
        case (true, false, true) => "Expected verification failure, but got success."
        case (false, true, true) => "Expected verification success, but got failure."
      }

    val expectedOutPath = if useBAPFrontend then variationPath + ".expected" else variationPath + "_gtirb.expected"
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
    Option(File(directoryName).listFiles(_.isDirectory)) match {
      case None => throw java.io.IOException(s"failed to read directory '$directoryName'")
      case Some(subdirs) => subdirs.map(_.getName)
    }
  }

  def log(text: String, path: String): Unit = {
    val writer = BufferedWriter(FileWriter(path, false))
    writer.write(text)
    writer.flush()
    writer.close()
  }

}



def histoToSvg(title: String, imgWidth: Int, imgHeight: Int, bins: List[((Double, Double), Int)]) : String = {
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
  val maxHeight = bins.map(_._2).max
  val binWidth : Double = (histWidth).doubleValue / bins.size
  val heightScaling : Double =  (histHeight.doubleValue)/(maxHeight)
  val binPos = (0 to bins.size).map(i => (leftMargin + i * binWidth, binWidth * (i + 1)))
    .zip(bins.map((_, bh) => heightScaling * bh))

  val rects = binPos.map((binXX, height) => 
      mkRect(binWidth.ceil.intValue, height.intValue, binXX._1.floor.intValue, histHeight.intValue - height.intValue + topMargin))

  val labels = {
    (text(title, imgWidth / 8, topMargin - 5),
    text("0", 0, histHeight + topMargin),
    text(maxHeight.toInt.toString, 0, topMargin),
    text(bins.head._1._1.toInt.toString, 0, imgHeight),
    text(bins.last._1._2.toInt.toString, (binWidth*(bins.size)).intValue - leftMargin, imgHeight))
  }

  val bg = mkRect(imgWidth, imgHeight, 0, 0, fill="White")

  val content = (Seq(bg) ++ rects ++ labels.toList).mkString("\n")
  template(imgWidth, imgHeight, content)
}


def loadHisto() = {
  val source = scala.io.Source.fromFile("src/test/full-testresult-GTIRB.csv").getLines().toList
  val headers = source.head.split(",")

  val res = headers.map(h => h -> mutable.ArrayBuffer[String]()).toMap[String, mutable.ArrayBuffer[String]]

  source.tail.map(line => {
    val cols = line.split(",")
    headers.zip(cols).foreach((h,v) => res(h).append(v))
  })

  val timeValues = res("verifyTime").map(_.toDouble)
  val histo = mkHistogram(50, Some(800.0, 1000.0))(timeValues.toSeq)


  println(histoToSvg("test histogram", 500, 300, histo))
}



class SystemTestsBAP extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", TestConfig(useBAPFrontend=true, expectVerify=true))
  runTests(incorrectPrograms, incorrectPath, "incorrect", TestConfig(useBAPFrontend=true, expectVerify=false))
  test("summarybap") {
    summary("testresult-BAP")
  }
}

class SystemTestsGTIRB extends SystemTests  {
  runTests(correctPrograms, correctPath, "correct", TestConfig(useBAPFrontend=false, expectVerify=true))
  runTests(incorrectPrograms, incorrectPath, "incorrect", TestConfig(useBAPFrontend=false, expectVerify=false))
  test("summarygtirb") {
    summary("testresult-GTIRB")
  }
}



// https://stackoverflow.com/questions/39617213/scala-what-is-the-generic-way-to-calculate-standard-deviation
def mean[T: Numeric](xs: Iterable[T]): Double = xs.sum.toDouble / xs.size

def variance[T: Numeric](xs: Iterable[T]): Double = {
  val avg = mean(xs)

  xs.map(_.toDouble).map(a => math.pow(a - avg, 2)).sum / xs.size
}

def median(xs: Iterable[Double]) = xs.toArray.sorted.apply(xs.size / 2)

def stdDev[T: Numeric](xs: Iterable[T]): Double = math.sqrt(variance(xs))

// https://stackoverflow.com/questions/24536215/scala-simple-histogram 
def mkHistogram(n_bins: Int, lowerUpperBound: Option[(Double, Double)] = None)(xs: Seq[Double]) : List[((Double, Double), Int)] = {
  val (mn, mx) = lowerUpperBound getOrElse(xs.min, xs.max)
  val epsilon = 0.0001
  val binSize = (mx - mn) / n_bins * (1 + epsilon)
  val bins = (0 to n_bins).map(mn + _ * binSize).sliding(2).map(xs => (xs(0), xs(1)))
  def binContains(bin:(Double,Double),x: Double) = (x >= bin._1) && (x < bin._2)
  bins.map(bin => (bin, xs.count(binContains(bin,_)))).toList
}
