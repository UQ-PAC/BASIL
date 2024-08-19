package ir


import ir.dsl._

import org.scalatest.funsuite.AnyFunSuite
class InvariantTest extends AnyFunSuite {

  test("sat singleCallBlockEnd case") {
    var program: Program = prog(
      proc("main",
        block("first_call",
          Assign(R0, bv64(10)),
          Assign(R1, bv64(10)),
          directCall("callee1"),
          ret 
        ),
        block("second_call",
          Assign(R0, bv64(10)),
          directCall("callee2"),
          ret
        ),
        block("returnBlock",
          ret
        )
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret)),
    )

    assert(invariant.singleCallBlockEnd(program))
  }

  test("unsat singleCallBlockEnd 1 (two calls)") {
    var program: Program = prog(
      proc("main",
        block("first_call",
          Assign(R0, bv64(10)),
          directCall("callee2"),
          Assign(R1, bv64(10)),
          directCall("callee1"),
          ret 
        ),
        block("second_call",
          Assign(R0, bv64(10)),
          ret
        ),
        block("returnBlock",
          ret
        )
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret)),
    )

    assert(!invariant.singleCallBlockEnd(program))
  }

  test("unsat singleCallBlockEnd 2 (not at end)") {
    var program: Program = prog(
      proc("main",
        block("first_call",
          Assign(R0, bv64(10)),
          Assign(R1, bv64(10)),
          ret 
        ),
        block("second_call",
          directCall("callee2"),
          Assign(R0, bv64(10)),
          ret
        ),
        block("returnBlock",
          ret
        )
      ),
      proc("callee1", block("bye1", ret)),
      proc("callee2", block("bye2", ret)),
    )

    assert(!invariant.singleCallBlockEnd(program))
  }

}
