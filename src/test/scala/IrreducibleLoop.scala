import analysis.{LoopDetector, IrreducibleLoops}
import ir.{Block, IRLoading, Procedure, Program, dotBlockGraph, dotFlowGraph}
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import translating.PrettyPrinter.pprint
import translating.{BAPToIR, ReadELFData}
import util.{ILLoadingConfig, LogLevel, Logger}

import scala.collection.immutable.ListMap
import scala.sys.process.*

import IrreducibleLoops.BlockLoopInfo

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
  case class TestLoopInfo(iloop_headers: ListMap[String, String], headers: Map[String, Set[String]])

  def assertLoopDetector(p: Procedure)(iloop_headers: ListMap[String, String])(headers: Map[String, Set[String]]) = {
    val loops = IrreducibleLoops.identify_loops(p).get

    loops.values.foreach(println(_))

    assertResult(TestLoopInfo(iloop_headers, headers)) {
      TestLoopInfo(
        loops.collect { case (k, BlockLoopInfo(_, Some(h), _, _, _)) => k.label -> h.label },
        loops.collect { case (k, info) if info.headers.nonEmpty => k.label -> info.headers.map(_.label) }
      )
    }
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

    assertLoopDetector(p.mainProcedure) {
      ListMap("f" -> "e", "b" -> "a", "g" -> "f", "c" -> "b", "d" -> "a", "h" -> "e", "i" -> "h")
    } {
      Map("e" -> Set("e"), "a" -> Set("a"), "f" -> Set("f"), "b" -> Set("b"), "h" -> Set("h"))
    }

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

    assertLoopDetector(p.mainProcedure) {
      ListMap("b" -> "a", "c" -> "b", "d" -> "c")
    } { ListMap("a" -> Set("a", "d"), "b" -> Set("b", "d"), "c" -> Set("c", "d")) }

    analysis.AnalysisPipelineMRA.reducibleLoops(p)

    val newLoopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    newLoopResult.values.foreach(println(_))

    assert(newLoopResult.values.forall(!_.isIrreducible()))

    util.writeToFile(dotBlockGraph(p.mainProcedure.blocks.toList, Set()), "/home/rina/progs/basil/out3.dot")
  }

  test("multiple entries - irreducible") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("a", "loopexit")),
        block("a", goto("loop")),
        block("b", goto("loop")),
        block("loop", goto("loopexit")),
        block("loopexit", goto("loop", "end")),
        block("end", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    println(result.iloopHeaders.groupMap(_._2)(_._1))
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + loop.nodes.map(_.label))
      println(loop.reentries)
      println(loop)
    }

    assertLoopDetector(p.mainProcedure) {
      ListMap("loopexit" -> "loop")
    } { Map("loop" -> Set("loop", "loopexit")) }

  }

  test("one long loop") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("preloop")),
        block("preloop", goto("loop")),
        block("loop", goto("loop2")),
        block("loop2", goto("loop3")),
        block("loop3", goto("loop", "end")),
        block("end", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    println(result.iloopHeaders.groupMap(_._2)(_._1))
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + (loop.nodes ++ loop.edges.flatMap(x => List(x.to, x.from))).map(_.label))
      println(loop)
    }

    assertLoopDetector(p.mainProcedure) {
      ListMap("loop2" -> "loop", "loop3" -> "loop")
    } { Map("loop" -> Set("loop")) }
  }

  test("nested loop") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("loop")),
        block("loop", goto("loop2")),
        block("loop2", goto("loop3")),
        block("loop3", goto("loop2", "loop4")),
        block("loop4", goto("loop", "end")),
        block("end", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    println(result.iloopHeaders.groupMap(_._2)(_._1))
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + (loop.nodes ++ loop.edges.flatMap(x => List(x.to, x.from))).map(_.label))
      println(loop)
    }

    assertLoopDetector(p.mainProcedure) {
      ListMap("loop2" -> "loop", "loop3" -> "loop2", "loop4" -> "loop")
    } { Map("loop" -> Set("loop"), "loop2" -> Set("loop2")) }
  }

  test("nested self-loop") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("loop")),
        block("loop", goto("loop2")),
        block("loop2", goto("loop3", "loop2")),
        block("loop3", goto("loop", "end")),
        block("end", ret)
      )
    )

    val result = LoopDetector.identify_loops(p)
    println(result.iloopHeaders.groupMap(_._2)(_._1))
    result.loops.values.foreach { loop =>
      println("" + loop.header + ": " + (loop.nodes ++ loop.edges.flatMap(x => List(x.to, x.from))).map(_.label))
      println(loop)
    }

    assertLoopDetector(p.mainProcedure) {
      ListMap("loop2" -> "loop", "loop3" -> "loop")
    } { ListMap("loop" -> Set("loop"), "loop2" -> Set("loop2")) }
  }

  test("sub-cycles applying transform") {
    import ir.dsl.*
    def makeProg = prog(
      proc("main")(
        block("S", goto("h1", "h2")),
        block("h1", goto("h2")),
        block("h2", goto("h1", "h3")),
        block("h3", goto("h2", "exit")),
        block("exit", ret)
      )
    )
    val p = makeProg

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    loopResult.values.foreach(println(_))
    val loops = loopResult.values.flatMap(_.toLoop())

    loops.foreach(println(_))
    // util.renderDotGraph(dotFlowGraph(p.mainProcedure.blocks.toList, Set()))

    val result = IrreducibleLoops.transform_loop(loopResult.values.filter(_.isIrreducible()).head).get
    // analysis.AnalysisPipelineMRA.reducibleLoops(p)

    val blocks = p.mainProcedure.labelToBlock

    assertResult(Set("S", "h1", "h3").map(x => result.entryIndices(blocks(x)))) {
      result.precedingIndices(blocks("h2")).toSet
    }
    assertResult(Set("S", "h2").map(x => result.entryIndices(blocks(x)))) {
      result.precedingIndices(blocks("h1")).toSet
    }

    assertResult(ListMap(), "there should be no irreducible loops after transform") {
      IrreducibleLoops.identify_loops(p.mainProcedure).get.filter(_._2.isIrreducible())
    }
  }

  test("crossover") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("h1", "h2")),
        block("h1", goto("x")),
        block("x", goto("h2", "h1")),
        block("h2", goto("y")),
        block("y", goto("x", "exit")),
        block("exit", ret)
      )
    )

    println(p)

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    loopResult.values.foreach(println(_))


    analysis.AnalysisPipelineMRA.reducibleLoops(p)
    util.writeToFile(dotFlowGraph(p.mainProcedure.blocks.toList, Set()), "/home/rina/progs/basil/out2.dot")

    // val result =
    //   IrreducibleLoops.transform_loop(loopResult.values.filter(_.isIrreducible()).flatMap(_.toLoop()).head).get
    //
    // util.writeToFile(dotFlowGraph(p.mainProcedure.blocks.toList, Set()), "/home/rina/progs/basil/out2.dot")
    //
    // println("\nAFTER\n")

    val newLoopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    newLoopResult.values.foreach(println(_))

    assert(newLoopResult.values.forall(!_.isIrreducible()))
  }

  test("paper fig4a") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("1")),
        block("1", goto("2")),
        block("2", goto("b0")),
        block("b0", goto("b")),
        block("b", goto("exit")),
        block("exit", ret)
      )
    )

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    assertResult(Nil) {
      loopResult.values.filter(_.isCycle()).toList
    }
  }

  test("paper fig4b") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("1")),
        block("1", goto("b")),
        block("b", goto("x")),
        block("x", goto("b0")),
        block("b0", goto("exit", "b")),
        block("exit", ret)
      )
    )

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    assertResult(List(Set("b", "b0", "x"))) {
      loopResult.values.filter(_.isCycle()).toList.map(_.nodes.map(_.label))
    }
  }

  test("paper fig4c") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("1")),
        block("1", goto("h")),
        block("h", goto("x", "b0")),
        block("x", goto("b")),
        block("b0", goto("b")),
        block("b", goto("z")),
        block("z", goto("exit")),
        block("exit", ret)
      )
    )

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    val cycles = loopResult.values.filter(_.isCycle()).toList
    assertResult(Nil) {
      cycles.map(_.nodes.map(_.label))
    }
  }

  test("paper fig4d") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("1")),
        block("1", goto("h")),
        block("h", goto("x")),
        block("x", goto("b0", "y")),
        block("y", goto("b")),
        block("b0", goto("b")),
        block("b", goto("z")),
        block("z", goto("exit", "h")),
        block("exit", ret)
      )
    )

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    val cycles = loopResult.values.filter(_.isCycle()).toList
    assertResult(List(Set("b", "b0", "x", "y", "h", "z"))) {
      cycles.map(_.nodes.map(_.label))
    }
  }

  test("paper fig6a") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("h3")),
        block("h3", goto("x")),
        block("x", goto("h2", "y")),
        block("y", goto("b0")),
        block("b0", goto("b")),
        block("h2", goto("h1")),
        block("h1", goto("b")),
        block("b", goto("z")),
        block("z", goto("h1", "a")),
        block("a", goto("h2", "back")),
        block("back", goto("h3", "exit")),
        block("exit", ret)
      )
    )
    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    loopResult.values.foreach(println(_))
    assertResult(2) {
      loopResult.values.filter(_.isIrreducible()).size
    }

    analysis.AnalysisPipelineMRA.reducibleLoops(p)

    val newLoopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    newLoopResult.values.foreach(println(_))

    assert(newLoopResult.values.forall(!_.isIrreducible()))
  }

  test("paper fig6b") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("h4")),
        block("h4", goto("h3")),
        block("h3", goto("h2")),
        block("h2", goto("h1")),
        block("h1", goto("x")),
        block("x", goto("y", "h4")),
        block("y", goto("z", "h3")),
        block("z", goto("back", "h2")),
        block("back", goto("h1", "exit")),
        block("exit", ret)
      )
    )
    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    loopResult.values.foreach(println(_))
    assertResult(Nil) {
      loopResult.values.filter(_.isIrreducible()).toList
    }

    analysis.AnalysisPipelineMRA.reducibleLoops(p)

    val newLoopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    newLoopResult.values.foreach(println(_))

    assert(newLoopResult.values.forall(!_.isIrreducible()))
  }

  test("paper fig4e") {
    import ir.dsl.*
    val p = prog(
      proc("main")(
        block("S", goto("h1")),
        block("h1", goto("y", "z")),
        block("y", goto("h")),
        block("h", goto("b")),
        block("z", goto("b0")),
        block("b0", goto("b")),
        block("b", goto("a")),
        block("a", goto("h", "h1", "exit")),
        block("exit", ret)
      )
    )

    val blocks = p.mainProcedure.labelToBlock

    val loopResult = IrreducibleLoops.identify_loops(p.mainProcedure).get
    val cycles = loopResult.filter(_._2.isCycle())
    cycles.foreach(println)

    val h1 = cycles(blocks("h1"))
    assertResult(Set("h1")) {
      h1.headers.map(_.label)
    }

    val inner = (cycles - blocks("h1")).head._2
    assertResult(Some(blocks("h1"))) {
      inner.iloop_header
    }

    assertResult(Set("h", "b")) {
      inner.headers.map(_.label)
    }
  }

  test("plist_free") {
    cancel()
    val p = ir.parsing.ParseBasilIL.loadILFile("/home/rina/progs/basil/plist-free.il").program
    // p.procedures.foreach { p =>
    //   p.blocks.foreach(_.statements.clear())
    //   while (ir.transforms.coalesceBlocks(p)) {}
    // }

    util.writeToFile(p.pprint, "/home/rina/progs/basil/plist-before.il")

    val loops = IrreducibleLoops.identify_loops(p.mainProcedure).get
    loops.values.filter(_.isCycle()).foreach { x =>
      println()
      println(x)
      println(x.toLoop().get)
    }

    val hl = loops.filter(_._2.headers.nonEmpty).last._2.nodes
    println("XXX" + hl)
    util.writeToFile(dotFlowGraph(p.mainProcedure.blocks.toList, hl), "/home/rina/progs/basil/out.dot")
    // util.renderDotGraph(dotFlowGraph(p.mainProcedure.blocks.toList, Set()))

    util.writeToFile(dotFlowGraph(p.mainProcedure.blocks.toList, Set()), "/home/rina/progs/basil/in.dot")
    analysis.AnalysisPipelineMRA.reducibleLoops(p)
    util.writeToFile(dotFlowGraph(p.mainProcedure.blocks.toList, Set()), "/home/rina/progs/basil/out.dot")

    util.writeToFile(p.pprint, "/home/rina/progs/basil/plist-after.il")

    println("AFTER TRANSFORM")
    p.procedures.filter(_.entryBlock.isDefined).foreach { p =>
      val blocksWithoutPrev = p.blocks.filter(b => Some(b) != p.entryBlock && b.prevBlocks.isEmpty).toList
      assertResult(Nil, "after loop transform, all blocks should still be reachable")(blocksWithoutPrev)

      val loops = IrreducibleLoops.identify_loops(p).get
      loops.values.flatMap(_.toLoop()).foreach(println(_))
      assert(loops.values.forall(!_.isIrreducible()))
    }
  }

}
