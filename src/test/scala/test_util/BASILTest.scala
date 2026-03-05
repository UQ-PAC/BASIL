package test_util

import boogie.SpecGlobal
import ir.{IRContext, Program, cilvisitor, transforms}
import org.scalatest.concurrent.ScaledTimeSpans
import org.scalatest.time.{Seconds, Span}
import specification.Specification
import util.boogie_interaction.*
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  DSConfig,
  ILLoadingConfig,
  Logger,
  RunUtils,
  StaticAnalysisConfig,
  MemoryEncodingRepresentation
}

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.sys.process.*

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
  memoryTransform: Boolean = false,
  memoryEncoding: Option[MemoryEncodingRepresentation] = None,
  useOfflineLifterForGtirbFrontend: Boolean = false
) {
  private val scaledtimespans = new ScaledTimeSpans {}
  def timeoutFlag: String =
    val seconds = scaledtimespans.scaled(Span(timeout, Seconds)).millisPart / 1000
    s"/timeLimit:$seconds"
  def boogieFlags: Seq[String] = timeoutFlag +: baseBoogieFlags
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
    memoryEncoding: Option[MemoryEncodingRepresentation] = None,
    postLoad: IRContext => Unit = s => (),
    useOfflineLifterForGtirbFrontend: Boolean = false
  ): BASILResult = {
    val specFile = if (specPath.isDefined && File(specPath.get).exists) {
      specPath
    } else {
      None
    }
    val config = BASILConfig(
      loading = ILLoadingConfig(
        inputFile = inputPath,
        relfFile = Some(RELFPath),
        specFile = specFile,
        parameterForm = false,
        gtirbLiftOffline = useOfflineLifterForGtirbFrontend
      ),
      simplify = simplify,
      summariseProcedures = summariseProcedures,
      staticAnalysis = staticAnalysisConf,
      boogieTranslation = util
        .BoogieGeneratorConfig(memoryEncoding = memoryEncoding)
        .copy(memoryFunctionType = util.BoogieMemoryAccessMode.SuccessiveStoreSelect),
      outputPrefix = BPLPath,
      dsaConfig = dsa,
      memoryTransform = memoryTransform,
      memoryEncoding = memoryEncoding
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
      case BoogieResultKind.Verified(_, _) =>
        Option.when(!expectVerify)("Expected verification failure, but got success.")
      case BoogieResultKind.Timeout => Some("SMT Solver timed out")
      case BoogieResultKind.AssertionFailed =>
        Option.when(expectVerify)("Expected verification success, but got failure.")
      case k: BoogieResultKind.Unknown => Some(k.toString)
    }
    (failureMsg, boogieResult.kind == BoogieResultKind.Verified, boogieResult.kind == BoogieResultKind.Timeout)
  }
}

object BASILTest {
  lazy val rootDirectory: String =
    Option(System.getenv("MILL_WORKSPACE_ROOT"))
      .getOrElse(System.getProperty("user.dir"))

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

  // only for use for DSL test cases, not for lifted binaries
  def programToContext(
    program: Program,
    globals: Set[SpecGlobal] = Set.empty,
    globalOffsets: Map[BigInt, BigInt] = Map.empty
  ): IRContext = {
    val replaceReturns = transforms.ReplaceReturns()
    cilvisitor.visit_prog(replaceReturns, program)
    replaceReturns.addR30Begins()
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val spec = Specification(Set(), globals, Map(), List(), List(), List(), Set())
    IRContext(List(), Set(), globals, Set(), globalOffsets, spec, program)
  }

}
