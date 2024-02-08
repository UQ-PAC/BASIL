
import org.scalatest.funsuite.AnyFunSuite
import util.{Logger, PerformanceTimer, ILLoadingConfig, RunUtils}
import translating.BAPToIR
import analysis.LoopDetector
import analysis.LoopTransform

import ir.{Program, Block, dotBlockGraph}

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable
import scala.io.Source
import scala.sys.process.*

private def writeToFile(text: String, path: String): Unit = {
  val writer = BufferedWriter(FileWriter(path, false))
  writer.write(text)
  writer.flush()
  writer.close()
}

/** Add more tests by simply adding them to the programs directory. Refer to the existing tests for the expected
  * directory structure and file-name patterns.
  */
class IrreducibleLoop extends AnyFunSuite {
  val testPath = "./src/test/analysis/"

  def load(conf: ILLoadingConfig) : Program = {
    val bapProgram = RunUtils.loadBAP(conf.adtFile)

    val (externalFunctions, globals, globalOffsets, mainAddress) = RunUtils.loadReadELF(conf.relfFile, conf)

    val IRTranslator = BAPToIR(bapProgram, mainAddress)
    var IRProgram = IRTranslator.translate
    IRProgram
  }


  def runTest(path: String, name: String): Unit= {
    val variationPath = path + "/" + name
    val outPath = variationPath + ".bpl"
    val ADTPath = variationPath + ".adt"
    val RELFPath = variationPath + ".relf"
    val timer = PerformanceTimer(s"test $variationPath")
    Logger.info(variationPath)

    val args = mutable.ArrayBuffer("--adt", ADTPath, "--relf", RELFPath, "--output", outPath, "--analyse")

    val program : Program = load(ILLoadingConfig(ADTPath, RELFPath))

    val detector = LoopDetector(program);
    val foundLoops = detector.identify_loops()
    val oldn = foundLoops.count(_.reducible)


    writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${variationPath}_blockgraph-before-reduce.dot")

    foundLoops.foreach(l => 
        Logger.info(s"found loops\n${l}")
        )


    val transformer = LoopTransform(foundLoops);
    val newLoops = transformer.llvm_transform();
    newLoops.foreach(l => 
        Logger.info(s"newloops\n${l}")
        )

    val newDetect = LoopDetector(program)

    writeToFile(dotBlockGraph(program, program.filter(_.isInstanceOf[Block]).map(b => b -> b.toString).toMap), s"${variationPath}_blockgraph-after-reduce.dot")
    val foundLoops2 = newDetect.identify_loops()
    assert(foundLoops2.count(_.reducible) == foundLoops.size)
    assert(foundLoops2.count(_.reducible) > foundLoops.count(_.reducible))



    foundLoops2.foreach(l => 
        Logger.info(s"updated found loops\n${l}")
        )


  }

  test("irreducible 1") {
    runTest(testPath + "/" +  "irreducible_loop", "irreducible")
  }

  test("irreducible 2") {
    runTest(testPath + "/" +  "irreducible_loop_2", "irreducible2")
  }


}
