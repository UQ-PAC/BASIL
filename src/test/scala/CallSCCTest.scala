import org.scalatest.funsuite.AnyFunSuite
import ir.dsl.*
import ir.{CallSCCWalker, updateWithCallSCC}
import test_util.programToContext

object CallSCCTestData {
  /*
    a -> b -> c -> g
         ^   /
         |  /
    f <- d <
   ^ |
   \/
   */
  def scc = {
    val program = prog(
      proc("a", block("a0", directCall("b"), ret)),
      proc("b", block("b0", directCall("c"), ret)),
      proc(
        "c",
        block("c0", goto("c1", "c2")),
        block("c1", directCall("d"), goto("c3")),
        block("c2", directCall("g"), goto("c3")),
        block("c3", ret)
      ),
      proc("g", block("g", ret)),
      proc(
        "d",
        block("d0", goto("d1", "d2")),
        block("d1", directCall("b"), goto("d3")),
        block("d2", directCall("f"), goto("d3")),
        block("d3", ret)
      ),
      proc(
        "f",
        block("f0", goto("f1", "f2")),
        block("f1", directCall("h"), goto("f3")),
        block("f2", directCall("f"), goto("f3")),
        block("f3", ret)
      ),
      proc("h", block("h", ret))
    )

    programToContext(program)
  }
}

@test_util.tags.UnitTest
class CallSCCTest extends AnyFunSuite with test_util.CaptureOutput {
  test("scc") {
    val p = CallSCCTestData.scc.program
    updateWithCallSCC(p)
    val a = p.nameToProcedure("a")
    val b = p.nameToProcedure("b")
    val c = p.nameToProcedure("c")
    val d = p.nameToProcedure("d")
    val f = p.nameToProcedure("f")
    val h = p.nameToProcedure("h")
    val g = p.nameToProcedure("g")

    val bcd = Set(b, c, d)

    assert(CallSCCWalker.succSCC(a) == Set(bcd))
    assert(CallSCCWalker.succSCC(b) == Set(Set(f), Set(g)))
    assert(CallSCCWalker.succSCC(b) == CallSCCWalker.succSCC(c))
    assert(CallSCCWalker.succSCC(b) == CallSCCWalker.succSCC(d))
    assert(CallSCCWalker.succSCC(f) == Set(Set(h)))
  }
}
