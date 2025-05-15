import org.scalatest.funsuite.AnyFunSuite
import util.{ILLoadingConfig, IRLoading, LogLevel, Logger, PerformanceTimer, RunUtils}
import translating.BAPToIR
import analysis.LoopDetector
import analysis.LoopTransform
import ir.{Block, Program, dotBlockGraph}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.*
import test_util.{BASILTest, CaptureOutput}

@test_util.tags.UnitTest
class IrreducibleLoop extends AnyFunSuite with CaptureOutput {
  private val testPath = s"${BASILTest.rootDirectory}/src/test/irreducible_loops"
  Logger.setLevel(LogLevel.ERROR)

  def load(conf: ILLoadingConfig): Program = {
    val bapProgram = IRLoading.loadBAP(conf.inputFile)
    val (_, _, _, _, _, mainAddress) = IRLoading.loadReadELF(conf.relfFile, conf)
    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    val IRProgram = IRTranslator.translate
    IRProgram
  }

  test("testverify reduced irreducible_loop") {
    val path = testPath + "/irreducible_loop/irreducible"
    val outPath = s"$path.bpl"
    val args = Array("-o", s"$outPath", "-i", s"$path.adt", "-r", s"$path.relf", "-s", s"$path.spec", "--analyse")

    Main.main(args)

    val boogieResult = Seq("boogie", "/smoke", "/timeLimit:10", "/useArrayAxioms", outPath).!!

    assert(boogieResult.contains("Boogie program verifier finished with 2 verified, 0 errors"))
    assert(!boogieResult.contains("found unreachable code"))
  }

  def runTest(path: String, name: String): Unit = {
    val variationPath = path + "/" + name
    val ADTPath = variationPath + ".adt"
    val RELFPath = variationPath + ".relf"
    Logger.debug(variationPath)

    val program: Program = load(ILLoadingConfig(ADTPath, RELFPath))

    val foundLoops = LoopDetector.identify_loops(program)

    BASILTest.writeToFile(
      dotBlockGraph(program, program.collect { case b: Block => b -> b.toString }.toMap),
      s"${variationPath}_blockgraph-before-reduce.dot"
    )

    foundLoops.identifiedLoops.foreach(l => Logger.debug(s"found loops${System.lineSeparator()}$l"))

    val newLoops = foundLoops.reducibleTransformIR()
    newLoops.identifiedLoops.foreach(l => Logger.debug(s"newloops${System.lineSeparator()}$l"))

    BASILTest.writeToFile(
      dotBlockGraph(program, program.collect { case b: Block => b -> b.toString }.toMap),
      s"${variationPath}_blockgraph-after-reduce.dot"
    )
    val foundLoops2 = LoopDetector.identify_loops(program)
    assert(foundLoops2.identifiedLoops.count(_.reducible) == foundLoops.identifiedLoops.size)
    assert(foundLoops2.identifiedLoops.count(_.reducible) > foundLoops.identifiedLoops.count(_.reducible))
    assert(foundLoops2.irreducibleLoops.isEmpty)

    foundLoops2.identifiedLoops.foreach(l => Logger.debug(s"updated found loops${System.lineSeparator()}$l"))
  }

  test("irreducible 1") {
    runTest(testPath + "/irreducible_loop", "irreducible")
  }

  test("irreducible 2") {
    runTest(testPath + "/irreducible_loop_2", "irreducible2")
  }

  test("testverify fail irreducible_loop") {
    val path = testPath + "/irreducible_loop/irreducible"
    val outPath = s"$path.bpl"
    val args = Array("-o", s"$outPath", "-i", s"$path.adt", "-r", s"$path.relf", "-s", s"$path.spec")

    Main.main(args)

    val boogieResult = Seq("boogie", "/useArrayAxioms", "/timeLimit:10", outPath).!!
    Logger.debug("Boogie result: " + boogieResult)
    assert(boogieResult.contains("Irreducible flow graphs are unsupported."))
  }

}
