package ir

import org.scalatest.funsuite.AnyFunSuite
import util.Logger
import ir.dsl.*
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

  test("asd") {
    println(toScala(LocalAssign(R0, bv64(1), None)))
    println(toScala(Assert(TrueLiteral, Some("asd"))))
    assert(false)
  }

}
