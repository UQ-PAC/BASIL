import analysis.LoopDetector
import analysis.NewLoopDetector
import ir.{Block, IRLoading, Program, dotBlockGraph}
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import translating.{BAPToIR, ReadELFData}
import util.{ILLoadingConfig, LogLevel, Logger}
import translating.PrettyPrinter.pprint

import scala.sys.process.*

@test_util.tags.UnitTest
class IrreducibleLoop extends AnyFunSuite with CaptureOutput {
  private val testPath = s"${BASILTest.rootDirectory}/src/test/irreducible_loops"
  Logger.setLevel(LogLevel.ERROR)

  def load(conf: ILLoadingConfig): Program = {
    val bapProgram = IRLoading.loadBAP(conf.inputFile)
    val ReadELFData(_, _, _, _, _, mainAddress) = IRLoading.loadReadELF(conf.relfFile.get, conf)
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

  def shuffleNextPrevBlocks(p: ir.Procedure) = p.blocks.foreach { b => b.internalShuffleJumps() }

  def runTest(path: String, name: String): Unit = {
    val variationPath = path + "/" + name
    val ADTPath = variationPath + ".adt"
    val RELFPath = variationPath + ".relf"
    Logger.debug(variationPath)

    val program: Program = load(ILLoadingConfig(ADTPath, Some(RELFPath)))
    ir.transforms.clearParams(program)

    val foundLoops = LoopDetector.identify_loops(program)

    BASILTest.writeToFile(
      dotBlockGraph(program, program.collect { case b: Block => b -> b.toString }.toMap),
      s"${variationPath}_blockgraph-before-reduce.dot"
    )

    foundLoops.identifiedLoops.foreach(l => Logger.debug(s"found loops${System.lineSeparator()}$l"))

    // loop headers for irreducible loops can be arbitrarily-chosen. shuffling the
    // order of nextBlocks/prevBlocks affects this choice.
    val clone = ir.dsl.IRToDSL.convertProgram(program).resolve
    clone.procedures.foreach(shuffleNextPrevBlocks(_))
    val cloneFoundLoops = LoopDetector.identify_loops(clone)
    assert {
      // in the reordered clone, every header should be either a header of the original analysis,
      // OR it should be a re-entry point.
      cloneFoundLoops.headers.map(_.label).forall { header =>
        foundLoops.headers.map(_.label).contains(header)
        || foundLoops.loops.values.exists { otherLoop => otherLoop.reentries.map(_.to.label).contains(header) }
      }
    }

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

  test("paper fig2") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("a", "e")),
        block("a", goto("b")),
        block("b", goto("c")),
        block("c", goto("d", "b")),
        block("d", goto("E", "a")),
        block("e", goto("f")),
        block("f", goto("g")),
        block("g", goto("f", "h")),
        block("h", goto("i")),
        block("i", goto("h", "e", "E")),
        block("E", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + loop.nodes.map(_.label))
    }

    println(NewLoopDetector(p.mainProcedure).identify_loops().compute_forest())

  }

  test("paper fig3") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("a", "d")),
        block("a", goto("b")),
        block("b", goto("a", "c", "E")),
        block("c", goto("b", "d", "E")),
        block("d", goto("c")),
        block("E", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    println(result.iloopHeaders.groupMap(_._2)(_._1))
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + loop.nodes.map(_.label))
      println(loop)
    }

    println(NewLoopDetector(p.mainProcedure).identify_loops().compute_forest())

  }


  test("plist_free") {
    val p = ir.parsing.ParseBasilIL.loadILFile("/home/rina/progs/basil/plist-free.il").program
    p.procedures.foreach { p =>
      p.blocks.foreach(_.statements.clear())
      while (ir.transforms.coalesceBlocks(p)) {}
    }

    util.writeToFile(p.pprint, "/home/rina/progs/basil/plist-before.il")

    println(LoopDetector.identify_loops(p))

    analysis.AnalysisPipelineMRA.reducibleLoops(p)

    util.writeToFile(p.pprint, "/home/rina/progs/basil/plist-after.il")

    p.procedures.foreach { p =>
      val blocksWithoutPrev = p.blocks.filter(b => Some(b) != p.entryBlock && b.prevBlocks.isEmpty).toList
      assertResult(Nil, "after loop transform, all blocks should still be reachable")(blocksWithoutPrev)
    }
  }

}
