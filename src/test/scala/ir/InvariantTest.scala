package ir

import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class InvariantTest extends AnyFunSuite with CaptureOutput {

  test("sat singleCallBlockEnd case") {
    val program: Program = prog(
      proc(
        "main",
        block("first_call", LocalAssign(R0, bv64(10)), LocalAssign(R1, bv64(10)), directCall("callee1"), ret),
        block("second_call", LocalAssign(R0, bv64(10)), directCall("callee2"), ret),
        block("returnBlock", ret)
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret))
    )

    assert(invariant.singleCallBlockEnd(program))
  }

  test("unsat singleCallBlockEnd 1 (two calls)") {
    val program: Program = prog(
      proc(
        "main",
        block(
          "first_call",
          LocalAssign(R0, bv64(10)),
          directCall("callee2"),
          LocalAssign(R1, bv64(10)),
          directCall("callee1"),
          ret
        ),
        block("second_call", LocalAssign(R0, bv64(10)), ret),
        block("returnBlock", ret)
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret))
    )

    assert(!invariant.singleCallBlockEnd(program))
  }

  test("unsat singleCallBlockEnd 2 (not at end)") {
    val program: Program = prog(
      proc(
        "main",
        block("first_call", LocalAssign(R0, bv64(10)), LocalAssign(R1, bv64(10)), ret),
        block("second_call", directCall("callee2"), LocalAssign(R0, bv64(10)), ret),
        block("returnBlock", ret)
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret))
    )

    assert(!invariant.singleCallBlockEnd(program))
  }

}
