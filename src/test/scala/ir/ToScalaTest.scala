package ir



import org.scalatest.concurrent.TimeLimits.failAfter
import org.scalatest.concurrent.{Signaler, TimeLimitedTests, ThreadSignaler}
import org.scalatest.time.{Span, Seconds}
import org.scalatest.funsuite.AnyFunSuite
import util.Logger
import ir.dsl.*
import ir.dsl.{given}
import ir.*

import scala.runtime.stdLibPatches.Predef.assert

class ToScalaTest extends AnyFunSuite with TimeLimitedTests {

  val program: Program = prog(
    proc("main",
      block("first_call",
        LocalAssign(R0, bv64(1), None),
        LocalAssign(R1, bv64(1), None),
        directCall("callee1"),
        goto("second_call")
      ),
      block("second_call",
        directCall("callee2"),
        goto("returnBlock")
      ),
      block("returnBlock",
        ret
      ),
    ),
    proc("callee1",
      block("returnBlock",
        ret
      ),
    ),
    proc("callee2",
      block("returnBlock",
        ret
      ),
    ),
    proc("empty procedure")
  )

  val expected = """
prog(
  proc("main",
    block("first_call",
      LocalAssign(Register("R0", 64), BitVecLiteral(BigInt("1"), 64), None),
      LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("1"), 64), None),
      directCall("callee1"),
      goto("second_call")
    ),
    block("second_call",
      directCall("callee2"),
      goto("returnBlock")
    ),
    block("returnBlock",
      ret
    )
  ),
  proc("callee1",
    block("returnBlock",
      ret
    )
  ),
  proc("callee2",
    block("returnBlock",
      ret
    )
  ),
  proc("empty procedure")
)
  """

  override def timeLimit = Span(2, Seconds)
  override val defaultTestSignaler = ThreadSignaler

  def cleanOutput(s: String): String = s.trim

  // NOTE: if this test TIMES OUT, it is likely that the ToScala instances have
  // become recursive. make sure that all instances are provided and correctly scoped,
  // especially where multiple instances might be applicable.
  test("test basil ir to dsl") {
    // println(f)

    import ir.dsl.ToScalaWithSplitting.given

    if (cleanOutput(expected) != cleanOutput(program.toScala)) {
      println("current program.toScala output:")
      println(program.toScala)
    }
    assert(cleanOutput(expected) == cleanOutput(program.toScala))
  }
}
