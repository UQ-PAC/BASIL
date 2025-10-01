import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import ir.Program
import ir.parsing.ParseBasilIL
import ir.shrinking.*
import ir.transforms.coalesceBlocks
import translating.PrettyPrinter.pprint

@test_util.tags.UnitTest
class ShrinkerTest extends AnyFunSuite with CaptureOutput {

  extension [T](x: Iterable[T]) {
    def it = x.iterator
  }

  test("boop") {
    println("x")
    println(roundRobin(
      List(
        List(1, 2, 3, 4).it,
        List(11, 12).it,
        List(21).it,
        List(31, 32, 33, 34).it,
      ).it
    ).toList)
  }

  test("shrink") {

    val s = ShrinkBasil(4, List(
      Shrinker.one[String](_.toLowerCase()).ifChanged(x => x),
      Shrinker.one[String](_.drop(1)).ifChanged(_.length)
    ))

    println(s.shrink(_ => true)("ASDJFIA"))

  }

  def stripAllStatements(p: Program) = {
    p.procedures.foreach { p =>
      p.blocks.foreach(_.statements.clear())
    }
    p
  }

  def countStatements(p: Program) =
    p.procedures.map { p =>
      p.blocks.map(_.statements.size).sum
    }.sum

  def countBlocks(p: Program) =
    p.procedures.map { p =>
      p.blocks.size
    }.sum

  def testIrredLoopBug(p: Program) = {
    p.mainProcedure.blocks.filterNot(Some(_) == p.mainProcedure.entryBlock).forall(_.prevBlocks.nonEmpty) && {

      val clone = ir.dsl.IRToDSL.convertProgram(p).resolve
      analysis.AnalysisPipelineMRA.reducibleLoops(clone)

      clone.mainProcedure.blocks.filterNot(Some(_) == p.mainProcedure.entryBlock).exists(_.prevBlocks.isEmpty)
    }
  }

  def clone(p: Program) = ir.dsl.IRToDSL.convertProgram(p).resolve

  test("irred loop bug") {
    val p = ParseBasilIL.loadILFile("/home/rina/progs/basil/plist-before.il").program

    val s = ShrinkBasil(10, List(
      Shrinker.one(clone.andThen(stripAllStatements)).ifChanged(countStatements),
      Shrinker.option(clone.andThen(p => Option.when(coalesceBlocks(p))(p)))
    ))

    assert(testIrredLoopBug(p))

    val reduced = s.shrink(testIrredLoopBug)(p)

    println(reduced)
  }

}
