package test_util

import org.scalatest.funsuite.AnyFunSuite
import ir.{Block, Procedure, Program}
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  DSAConfig,
  ILLoadingConfig,
  IRContext,
  Logger,
  RunUtils,
  StaticAnalysisConfig
}

import scala.sys.process.*
import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter}

case class TestConfig(
  boogieFlags: Seq[String] = Seq("/timeLimit:10", "/useArrayAxioms"),
  staticAnalysisConfig: Option[StaticAnalysisConfig] = None,
  useBAPFrontend: Boolean,
  expectVerify: Boolean,
  checkExpected: Boolean = false,
  logResults: Boolean = false,
  simplify: Boolean = false
)

trait BASILTest {
  def runBASIL(
    inputPath: String,
    RELFPath: String,
    specPath: Option[String],
    BPLPath: String,
    staticAnalysisConf: Option[StaticAnalysisConfig],
    simplify: Boolean = false,
    dsa: Option[DSAConfig] = None,
    postLoad: IRContext => Unit = s => ()
  ): BASILResult = {
    val specFile = if (specPath.isDefined && File(specPath.get).exists) {
      specPath
    } else {
      None
    }
    val config = BASILConfig(
      loading = ILLoadingConfig(inputFile = inputPath, relfFile = RELFPath, specFile = specFile, parameterForm = false),
      simplify = simplify,
      staticAnalysis = staticAnalysisConf,
      boogieTranslation =
        util.BoogieGeneratorConfig().copy(memoryFunctionType = util.BoogieMemoryAccessMode.SuccessiveStoreSelect),
      outputPrefix = BPLPath,
      dsaConfig = dsa
    )
    val result = RunUtils.loadAndTranslate(config, postLoad = postLoad)
    RunUtils.writeOutput(result)
    result
  }

  def runBoogie(directoryPath: String, bplPath: String, boogieFlags: Seq[String]): String = {
    val extraSpec = List
      .from(File(directoryPath).listFiles())
      .map(_.toString)
      .filter(_.endsWith(".bpl"))
      .filterNot(_.endsWith(bplPath))
    val boogieCmd = Seq("boogie", "/printVerifiedProceduresCount:0") ++ boogieFlags ++ Seq(bplPath) ++ extraSpec
    Logger.debug(s"Verifying... ${boogieCmd.mkString(" ")}")
    val boogieResult = boogieCmd.!!
    boogieResult
  }

  /** @return
    *   param 0: None if passes, Some(failure message) if doesn't pass param 1: whether the Boogie output verified param
    *   2: whether Boogie timed out
    */
  def checkVerify(
    boogieResult: String,
    resultPath: String,
    shouldVerify: Boolean
  ): (Option[String], Boolean, Boolean) = {
    BASILTest.writeToFile(boogieResult, resultPath)
    val verified = boogieResult.strip().equals("Boogie program verifier finished with 0 errors")
    val proveFailed = boogieResult.contains("could not be proved")
    val timedOut = boogieResult.strip().contains("timed out")

    val failureMsg = if (timedOut) {
      Some("SMT Solver timed out")
    } else {
      (verified, shouldVerify, BASILTest.xor(verified, proveFailed)) match {
        case (true, true, true) => None
        case (false, false, true) => None
        case (_, _, false) => Some("Prover error: unknown result: " + boogieResult)
        case (true, false, true) => Some("Expected verification failure, but got success.")
        case (false, true, true) => Some("Expected verification success, but got failure.")
      }
    }
    (failureMsg, verified, timedOut)
  }
}

object BASILTest {
  def writeToFile(text: String, path: String): Unit = {
    val writer = BufferedWriter(FileWriter(path, false))
    writer.write(text)
    writer.flush()
    writer.close()
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
    *   of the parent directory
    * @return
    *   the names all subdirectories of the given parent directory
    */
  def getSubdirectories(directoryName: String): Array[String] = {
    Option(File(directoryName).listFiles(_.isDirectory)) match {
      case None => throw java.io.IOException(s"failed to read directory '$directoryName'")
      case Some(subdirs) => subdirs.map(_.getName)
    }
  }

  def xor(x: Boolean, y: Boolean): Boolean = (x || y) && !(x && y)

  def mean(xs: Iterable[Double]): Double = xs.sum / xs.size

  def variance(xs: Iterable[Double]): Double = {
    val avg = mean(xs)

    xs.map(a => math.pow(a - avg, 2)).sum / xs.size
  }

  def median(xs: Iterable[Double]): Double = xs.toArray.sorted.apply(xs.size / 2)

  def stdDev(xs: Iterable[Double]): Double = math.sqrt(variance(xs))
}
