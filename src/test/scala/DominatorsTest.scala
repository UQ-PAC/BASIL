import ir.dsl.*
import org.scalatest.*
import org.scalatest.funsuite.*
import test_util.CaptureOutput

@test_util.tags.UnitTest
class DominatorsTest extends AnyFunSuite with CaptureOutput {
  import analysis.Dominators.*

  def domsToStringMap(doms: Result) =
    doms.map((k, v) => k.label -> nodeToString(v)).toMap

  test("figure 4 in paper") {
    val p = prog(
      proc(
        "main",
        block("6", goto("5", "4")),
        block("5", goto("1")),
        block("4", goto("2", "3")),
        block("3", goto("2")),
        block("2", goto("1", "3")),
        block("1", goto("2"))
      )
    )

    val expected = Seq("1", "2", "3", "4", "5", "6").map(_ -> "6").toMap
    val doms = computeDominatorTree(p.mainProcedure)
    assertResult(expected) {
      doms.map((k, v) => k.label -> nodeToString(v)).toMap
    }
    val b = p.labelToBlock
    assertResult(true)(dominates(doms)(b("6"), b("1")))
    assertResult(false)(dominates(doms)(b("1"), b("6")))
    assertResult(true)(dominates(doms)(b("6"), b("6")))
    assertResult(true)(dominates(doms)(b("1"), b("1")))
    assertResult(false)(dominates(doms)(b("5"), b("1")))
  }

  test("figure 2 in paper") {
    val p = prog(
      proc(
        "main",
        block("5", goto("4", "3")),
        block("4", goto("1")),
        block("3", goto("2")),
        block("2", goto("1")),
        block("1", goto("2"))
      )
    )

    val expected = Seq("1", "2", "3", "4", "5").map(_ -> "5").toMap
    val doms = computeDominatorTree(p.mainProcedure)
    assertResult(expected) {
      domsToStringMap(doms)
    }
  }

  test("straight line") {
    val p = prog(
      proc(
        "main",
        block("5", goto("4")),
        block("4", goto("3")),
        block("3", goto("2")),
        block("2", goto("1")),
        block("1", goto("0")),
        block("0", goto("0"))
      )
    )

    val expected = Map("4" -> "5", "5" -> "5", "1" -> "2", "0" -> "1", "2" -> "3", "3" -> "4")
    val doms = computeDominatorTree(p.mainProcedure)
    assertResult(expected) {
      domsToStringMap(doms)
    }
  }

  test("irreducible2.c") {
    // from the diagram of
    // src/test/irreducible_loops/irreducible_loop_2/irreducible2.c
    val p = prog(
      proc(
        "main",
        block("start", goto("a", "g")),
        block("a", goto("b")),
        block("b", goto("c")),
        block("c", goto("d")),
        block("d", goto("f")),
        block("f", goto("e", "b")),
        block("g", goto("d")),
        block("e", goto("e"))
      )
    )

    val expected = Map(
      "e" -> "f",
      "f" -> "d",
      "a" -> "start",
      "b" -> "start",
      "g" -> "start",
      "c" -> "b",
      "start" -> "start",
      "d" -> "start"
    )
    val doms = computeDominatorTree(p.mainProcedure)
    assertResult(expected) {
      domsToStringMap(doms)
    }
  }
}
