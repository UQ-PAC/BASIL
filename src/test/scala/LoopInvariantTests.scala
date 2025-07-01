import analysis.*
import boogie.SpecGlobal
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.{CaptureOutput, programToContext}
import util.{
  BASILConfig,
  BASILResult,
  BoogieGeneratorConfig,
  ILLoadingConfig,
  IRContext,
  RunUtils,
  StaticAnalysisConfig
}

@test_util.tags.UnitTest
class LoopInvariantTests extends AnyFunSuite, CaptureOutput {
  def genProcedureInvariants(
    program: Program,
    procedure: Procedure
  ): (Map[Block, List[Predicate]], Map[Block, List[Predicate]]) = {
    val foundLoops = LoopDetector.identify_loops(program)
    val newLoops = foundLoops.reducibleTransformIR().identifiedLoops
    foundLoops.updateIrWithLoops()

    FullLoopInvariantGenerator(program).genInvariants(procedure)
  }

  def runTest(
    program: Program,
    globals: Set[SpecGlobal] = Set.empty,
    globalOffsets: Map[BigInt, BigInt] = Map.empty
  ): BASILResult = {
    val context = programToContext(program, globals, globalOffsets)
    // Don't simplify as we would have generated invariants before simplification. The simplifier will rename variables
    // among other things, breaking these invariants.
    RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = None),
        simplify = true,
        generateLoopInvariants = true,
        staticAnalysis = Some(StaticAnalysisConfig()),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = None // Some(DSAConfig(Set.empty))
      )
    )
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
      proc(
        "main",
        block("entry", LocalAssign(R0, bv64(0)), goto("loop_head")),
        block("loop_head", goto("loop_body", "loop_exit")),
        block(
          "loop_body",
          Assume(BinaryExpr(BVULT, R0, bv64(100))),
          LocalAssign(R0, BinaryExpr(BVADD, R0, bv64(1))),
          goto("loop_head")
        ),
        block("loop_exit", Assume(BinaryExpr(BVUGE, R0, bv64(100))), Assert(BinaryExpr(EQ, R0, bv64(100))), ret)
      )
    )

    val main = program.nameToProcedure("main")

    val (preconditions, postconditions) = genProcedureInvariants(program, main)

    main.blocks.foreach(b => {
      if (b.label == "loop_head") {
        assert(b.isLoopHeader())

        // TODO use an SMT solver to check that the generated invariant implies our expected result instead of using syntactic equality.

        assert(
          postconditions(b).contains(
            Predicate.and(
              Predicate.BVCmp(BVULE, BVTerm.Lit(bv64(0)), BVTerm.Var(R0)),
              Predicate.BVCmp(BVULE, BVTerm.Var(R0), BVTerm.Lit(bv64(100)))
            )
          )
        )
      } else {
        assert(!b.isLoopHeader())
      }
    })

    val basilOut = runTest(program)
    val results = basilOut.boogie.map(_.verifyBoogie())
    assertResult(true) {
      results.forall(_.kind.isVerified)
    }
  }

  // TODO fix gamma domains
//  test("gammaTest") {
//    /* while (x < y)
//     * {
//     *   x += y;
//     * }
//     */
//
//    val program = prog(
//      proc(
//        "main",
//        block("entry", goto("loop_head")),
//        block("loop_head", goto("loop_body", "loop_exit")),
//        block(
//          "loop_body",
//          Assume(BinaryExpr(BVULT, R0, R1)),
//          LocalAssign(R0, BinaryExpr(BVADD, R0, R1)),
//          goto("loop_head")
//        ),
//        block("loop_exit", Assume(BinaryExpr(BVUGE, R0, R1)), ret)
//      )
//    )
//
//    genInvariants(program)
//
//    val main = program.nameToProcedure("main")
//    val head = main.labelToBlock("loop_head")
//    assert(head.postconditions.contains(Predicate.True))
//  }
}
