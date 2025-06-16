package test_util

import org.scalatest.concurrent.ScaledTimeSpans
import org.scalatest.time.{Span, Seconds}

import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  DSConfig,
  ILLoadingConfig,
  IRContext,
  Logger,
  RunUtils,
  StaticAnalysisConfig
}
import util.boogie_interaction.*

import scala.sys.process.*
import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter}

case class TestConfig(
  timeout: Int = 10,
  baseBoogieFlags: Seq[String] = Seq("/useArrayAxioms"),
  staticAnalysisConfig: Option[StaticAnalysisConfig] = None,
  useBAPFrontend: Boolean,
  expectVerify: Boolean,
  checkExpected: Boolean = false,
  logResults: Boolean = false,
  simplify: Boolean = false,
  summariseProcedures: Boolean = false,
  dsa: Option[DSConfig] = None,
  memoryTransform: Boolean = false
) {
  private val scaledtimespans = new ScaledTimeSpans {}
  def timeoutFlag =
    val seconds = scaledtimespans.scaled(Span(timeout, Seconds)).millisPart / 1000
    s"/timeLimit:${seconds}"
  def boogieFlags = timeoutFlag +: baseBoogieFlags
}

trait BASILTest {
  def runBASIL(
    inputPath: String,
    RELFPath: String,
    specPath: Option[String],
    BPLPath: String,
    staticAnalysisConf: Option[StaticAnalysisConfig],
    simplify: Boolean = false,
    summariseProcedures: Boolean = false,
    dsa: Option[DSConfig] = None,
    memoryTransform: Boolean = false,
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
      summariseProcedures = summariseProcedures,
      staticAnalysis = staticAnalysisConf,
      boogieTranslation =
        util.BoogieGeneratorConfig().copy(memoryFunctionType = util.BoogieMemoryAccessMode.SuccessiveStoreSelect),
      outputPrefix = BPLPath,
      dsaConfig = dsa,
      memoryTransform = memoryTransform
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
   *
    *   param 0: None if passes, Some(failure message) if doesn't pass
    *   param 1: whether the Boogie output verified
    *   param 2: whether Boogie timed out
    */
  def checkVerify(boogieStdout: String, expectVerify: Boolean): (Option[String], Boolean, Boolean) = {
    val boogieResult = parseOutput(boogieStdout)
    checkVerify(boogieResult, expectVerify)
  }

  def checkVerify(boogieResult: BoogieResult, expectVerify: Boolean): (Option[String], Boolean, Boolean) = {
    val failureMsg = boogieResult.kind match {
      case BoogieResultKind.Verified(_, _) if expectVerify => None
      case BoogieResultKind.AssertionFailed if !expectVerify => None
      case BoogieResultKind.Timeout => Some("SMT Solver timed out")
      case BoogieResultKind.Verified(_, _) if !expectVerify => Some("Expected verification failure, but got success.")
      case BoogieResultKind.AssertionFailed if expectVerify => Some("Expected verification success, but got failure.")
      case k: BoogieResultKind.Unknown => Some(k.toString)
    }
    (failureMsg, boogieResult.kind == BoogieResultKind.Verified, boogieResult.kind == BoogieResultKind.Timeout)
  }
}

object BASILTest {
  lazy val rootDirectory: String = {
    val millRoot = System.getenv("MILL_WORKSPACE_ROOT")
    if (millRoot == null) {
      System.getProperty("user.dir")
    } else {
      millRoot
    }
  }

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
