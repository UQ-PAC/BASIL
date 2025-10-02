import analysis.LoopDetector
import ir.Program
import ir.Block
import ir.Procedure
import ir.Assume
import ir.GoTo
import ir.parsing.ParseBasilIL
import ir.shrinking.*
import ir.transforms.{RemoveUnreachableBlocks, simplifyCFG}
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import translating.PrettyPrinter.pprint

@test_util.tags.UnitTest
class ShrinkerTest extends AnyFunSuite with CaptureOutput {
  shrinkerTest =>

  extension [T](x: Iterable[T]) {
    def it = x.iterator
  }

  test("boop") {
    println("x")
    println(roundRobin(List(List(1, 2, 3, 4).it, List(11, 12).it, List(21).it, List(31, 32, 33, 34).it).it).toList)
  }

  test("shrink") {

    val s = ShrinkBasil(
      4,
      List(Shrinker.one[String](_.toLowerCase()).ifChanged(x => x), Shrinker.one[String](_.drop(1)).ifChanged(_.length))
    )

    println(s.shrink(_ => true)("ASDJFIA"))

  }

  def stripAllStatements(b: Block) = b.statements.clear()

  def findDeletableBranches(p: Procedure) = {
    val joinsWithNoAssume = p.blocks.filterNot(_.statements.exists {
      case _: Assume => true
      case _ => false
    }).filter(_.prevBlocks.size > 1)

    joinsWithNoAssume.flatMap { b =>
      b.prevBlocks.filter {
        _.nextBlocks.size > 1
      }.map(_ -> b)
    }
  }

  def transfer(newProgram: Program)(oldBlock: Block): Block =
    // XXX: what about if labels are duplicated?
    newProgram.nameToProcedure(oldBlock.parent.procName).labelToBlock(oldBlock.label)
    // TODO: this is kinda slow

  object DeleteSomeBranches extends Shrinker[Program] {
    def shrink(p: Program) = {
      p.procedures.flatMap { proc =>
        val deletable = findDeletableBranches(proc)

        deletable.map { deletable =>

          var (from, to) = deletable

          val cloned = shrinkerTest.clone(p)
          from = transfer(cloned)(from)
          to = transfer(cloned)(to)

          from.jump.asInstanceOf[GoTo].removeTarget(to)
          cloned
        }

      }

    }

  }

  def forallProcs(f: Procedure => Unit)(p: Program): Unit =
    p.procedures.foreach(f)

  def forallBlocks(f: Block => Unit)(p: Program): Unit =
    forallProcs(_.blocks.foreach(f))(p)

  def countStatements(p: Program) =
    p.procedures.map { p =>
      p.blocks.map(_.statements.size).sum
    }.sum

  def countBlocks(p: Program) =
    p.procedures.map { p =>
      p.blocks.size
    }.sum

  def fixupEntry(p: Procedure) = p.entryBlock match {
    case Some(entry) if entry.prevBlocks.nonEmpty =>
      val newEntry = Block(p.freshBlockId("new_entry"))

      p.addBlock(newEntry)
      newEntry.replaceJump(GoTo(entry))

      p.entryBlock = newEntry
    case _ =>  ()
  }

  def testIrredLoopBug(p: Program) = {
    val loops = LoopDetector.identify_loops(p)

    loops.irreducibleLoops.nonEmpty &&
    p.mainProcedure.blocks.filterNot(Some(_) == p.mainProcedure.entryBlock).forall(_.prevBlocks.nonEmpty) && {

      val clone = shrinkerTest.clone(p)
      // println()
      // LoopDetector.identify_loops(clone).loops.foreach { (h,loop) =>
      //   println(s"${h}: ${loop}, Nodes: ${loop.nodes}, Reentries: ${loop.reentries}")
      // }
      analysis.AnalysisPipelineMRA.reducibleLoops(clone)
      println(clone.pprint)

      val loops = LoopDetector.identify_loops(clone)

      // clone.mainProcedure.blocks.filterNot(Some(_) == clone.mainProcedure.entryBlock).exists(_.prevBlocks.isEmpty)
      loops.irreducibleLoops.nonEmpty
    }
  }

  def clone(p: Program) = ir.dsl.IRToDSL.convertProgram(p).resolve

  test("irred loop bug") {
    val p = ParseBasilIL.loadILFile("/home/rina/progs/basil/plist-before.il").program

    val s = ShrinkBasil(
      10,
      List(
        Shrinker.one(clone).tap(forallBlocks(stripAllStatements)).ifChanged(countStatements),
        Shrinker
          .one(clone)
          .tap(forallProcs(simplifyCFG))
          .tap(forallProcs(RemoveUnreachableBlocks.apply))
          .ifChanged(countBlocks),
        DeleteSomeBranches,
      )
    )

    assert(testIrredLoopBug(p))

    val reduced = s.shrink(testIrredLoopBug)(p)

    // println(reduced)
    println(reduced.map(p => (countBlocks(p), countStatements(p))))

    println()
    LoopDetector.identify_loops(reduced.head).loops.foreach { (h,loop) =>
      println(s"${h}: ${loop}, Nodes: ${loop.nodes}, Reentries: ${loop.reentries}")
    }

    println(reduced.head.pprint)
    println("SHRUNK AFTER")

    analysis.AnalysisPipelineMRA.reducibleLoops(reduced.head)
    LoopDetector.identify_loops(reduced.head).loops.foreach { (h,loop) =>
      println(s"${h}: ${loop}, Nodes: ${loop.nodes}, Reentries: ${loop.reentries}")
    }
    println(reduced.head.pprint)

  }

}
