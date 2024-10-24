
import org.scalatest.funsuite.AnyFunSuite
import util.{Logger, PerformanceTimer, ILLoadingConfig, RunUtils, IRLoading}
import translating.BAPToIR
import analysis.LoopDetector
import analysis.LoopTransform

import ir.{Program, Block, dotBlockGraph}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.*
import test_util.BASILTest.writeToFile


/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */
class IrreducibleLoop extends AnyFunSuite {
  val testPath = "./src/test/irreducible_loops"

  def load(conf: ILLoadingConfig) : Program = {
    val bapProgram = IRLoading.loadBAP(conf.inputFile)
    val (_, _, _, _, _, mainAddress) = IRLoading.loadReadELF(conf.relfFile, conf)
    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    val IRProgram = IRTranslator.translate
    IRProgram
  }

  def runTest(path: String, name: String): Unit = {
    val variationPath = path + "/" + name
    val ADTPath = variationPath + ".adt"
    val RELFPath = variationPath + ".relf"
    Logger.debug(variationPath)

    val program: Program = load(ILLoadingConfig(ADTPath, RELFPath))

    val detector = LoopDetector(program)
    val foundLoops = detector.identify_loops()

    writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${variationPath}_blockgraph-before-reduce.dot")

    foundLoops.foreach(l => Logger.debug(s"found loops${System.lineSeparator()}$l"))

    val loops_to_transform = detector.irreducible_loops()
    assert(loops_to_transform.forall(foundLoops.contains))

    val transformer = LoopTransform(foundLoops)
    val newLoops = transformer.llvm_transform()
    newLoops.foreach(l => Logger.debug(s"newloops${System.lineSeparator()}$l"))

    val newDetect = LoopDetector(program)

    writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${variationPath}_blockgraph-after-reduce.dot")
    val foundLoops2 = newDetect.identify_loops()
    assert(foundLoops2.count(_.reducible) == foundLoops.size)
    assert(foundLoops2.count(_.reducible) > foundLoops.count(_.reducible))
    assert(newDetect.irreducible_loops().isEmpty)

    foundLoops2.foreach(l => Logger.debug(s"updated found loops${System.lineSeparator()}$l"))
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

    val boogieResult = Seq("boogie", "/useArrayAxioms", outPath).!!

    Logger.debug("Boogie result: " + boogieResult)

    assert(boogieResult.contains("Irreducible flow graphs are unsupported."))
  }

  test("testverify reduced irreducible_loop") {
    val path = testPath + "/irreducible_loop/irreducible"
    val outPath = s"$path.bpl"
    val args = Array("-o", s"$outPath", "-i", s"$path.adt", "-r", s"$path.relf", "-s", s"$path.spec", "--analyse")

    Main.main(args)

    val boogieResult = Seq("boogie", "/smoke", "/useArrayAxioms", outPath).!!

    Logger.debug("Boogie result: " + boogieResult)

    assert(boogieResult.contains("Boogie program verifier finished with 2 verified, 0 errors"))
    assert(!boogieResult.contains("found unreachable code"))
  }


}
