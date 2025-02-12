package ir

import org.scalatest.funsuite.AnyFunSuite
import util.Logger
import ir.dsl.*
import ir.dsl.{given}
import ir.*

import scala.runtime.stdLibPatches.Predef.assert

class DSLExportTest extends AnyFunSuite {

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
    )
  )

  val expected = """
    prog(
      proc("main",
        block("first_call",
          LocalAssign(Register("R0", 64), BitVecLiteral(BigInt("1"), 64), None),
          LocalAssign(Register("R1", 64), BitVecLiteral(BigInt("1"), 64), None),
          directCall("callee1""),
          goto("second_call")
        ),
        block("second_call",
          directCall("callee2""),
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
      )
    )
  """

  def cleanOutput(s: String): String = s.replaceAll(" ", "").replaceAll("\n", "").trim

  test("test basil ir to dsl") {
    assert(cleanOutput(expected) == cleanOutput(program.toScala))
    println(program.toScala)
  }

}
