import analysis.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import util.IRTransform

@test_util.tags.UnitTest
class LoopInvariantTests extends AnyFunSuite, CaptureOutput {
  def genInvariants(program: Program) = {
    val foundLoops = LoopDetector.identify_loops(program)
    val newLoops = foundLoops.reducibleTransformIR().identifiedLoops
    foundLoops.updateIrWithLoops()

    IRTransform.generateLoopInvariants(program)
  }

  test("boundedLoop") {
    /* x = 0;
     * while (x < 100)
     *   // we hope to find this invariant with an interval analysis
     *   invariant 0 <= x <= 100 // (or x in [0, 100])
     * {
     *   x++;
     * }
     * assert x == 100; */

    val program = prog(
      proc("main",
        block("entry", LocalAssign(R0, bv64(0)), goto("loop_head")),
        block("loop_head", goto("loop_body", "loop_exit")),
        block("loop_body", Assume(BinaryExpr(BVULT, R0, bv64(100))), LocalAssign(R0, BinaryExpr(BVADD, R0, bv64(1))), goto("loop_head")),
        block("loop_exit", Assume(BinaryExpr(BVUGE, R0, bv64(100))), Assert(BinaryExpr(BVEQ, R0, bv64(100))), ret),
      )
    )

    genInvariants(program)

    val main = program.nameToProcedure("main")

    main.blocks.foreach( b => {
      if (b.label == "loop_head") {
        assert(b.isLoopHeader())

        // TODO use an SMT solver to check that the generated invariant implies our expected result instead of using syntactic equality.

        assert(List(b.postconditions) == List(
          Predicate.and(
            Predicate.BVCmp(BVULE, BVTerm.Lit(bv64(0)), BVTerm.Var(R0)),
            Predicate.BVCmp(BVULE, BVTerm.Var(R0), BVTerm.Lit(bv64(100)))
          )
        ))
      } else {
        assert(!b.isLoopHeader())
      }
    })

    print(program)

    // TODO assert x == 100 with boogie
    assert(false)
  }
}
