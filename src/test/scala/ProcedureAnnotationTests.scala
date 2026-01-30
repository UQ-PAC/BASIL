import analysis.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest.programToContext
import test_util.CaptureOutput
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils}

@test_util.tags.UnitTest
class ProcedureAnnotationTests extends AnyFunSuite, CaptureOutput {

  def annotateProgram(program: Program) = {
    IrreducibleLoops.transform_all_and_update(program)

    val summaryGenerator = SummaryGenerator(program, true)
    program.procedures
      .filter(proc => program.mainProcedure != proc)
      .foreach { procedure =>
        {
          procedure.requires = summaryGenerator.generateRequires(procedure)
          procedure.ensures = summaryGenerator.generateEnsures(procedure)
        }
      }
    FullLoopInvariantGenerator(program).addInvariants()
  }

  test("loops-terminate") {
    /* int loop(int a, int b) {
     *     while (a < b) {
     *         a++;
     *         b--;
     *     }
     *     return a;
     * }
     */
    val a = LocalVar("a", BitVecType(64), 0)
    val b = LocalVar("b", BitVecType(64), 0)
    val program = prog(
      proc(
        "loop",
        in = Seq("a" -> BitVecType(64), "b" -> BitVecType(64)),
        out = Seq("a_out" -> BitVecType(64)),
        returnBlockLabel = Some("return")
      )(
        block("procentry", goto("entry")),
        block("entry", goto("loop_body", "loop_exit")),
        block(
          "loop_body",
          Assume(BinaryExpr(BVSLT, a, b), None, None, true),
          LocalAssign(a, BinaryExpr(BVADD, a, BitVecLiteral(BigInt("1"), 64))),
          LocalAssign(b, BinaryExpr(BVSUB, b, BitVecLiteral(BigInt("1"), 64))),
          goto("entry")
        ),
        block("loop_exit", Assume(BinaryExpr(BVSGE, a, b), None, None, true), goto("return")),
        block("return", ret("a_out" -> a))
      )
    )
    val procedure = program.nameToProcedure("loop")

    annotateProgram(program)

    // TODO run boogie once gamma constraints on the loop guard variables can be added in a loop invariant.
  }

  test("wp-dual-works") {
    // There was a bug where the wp dual domain was requiring false when it couldn't read a bitvector expression as a BVTerm.
    // This should hopefully catch that (though it's unlikely to happen again)
    val a = LocalVar("a", BitVecType(64), 0)
    val program = prog(
      proc("main", returnBlockLabel = Some("return_main"))(
        block("entry_main", directCall("assert"), goto("return_main")),
        block("return_main", ret())
      ),
      proc("assert", returnBlockLabel = Some("return_assert"))(
        block(
          "entry_assert",
          // Predicate translater couldn't read this expression
          LocalAssign(a, OldExpr(BitVecLiteral(BigInt(2), 64))),
          Assert(BinaryExpr(EQ, a, BitVecLiteral(BigInt("2"), 64))),
          goto("return_assert")
        ),
        block("return_assert", ret())
      )
    )

    annotateProgram(program)
    val context = programToContext(program)
    val basilOut = RunUtils.loadAndTranslate(
      BASILConfig(
        context = Some(context),
        loading = ILLoadingConfig(inputFile = "", relfFile = None),
        simplify = true,
        generateLoopInvariants = true,
        staticAnalysis = None,
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        dsaConfig = None // Some(DSAConfig(Set.empty))
      )
    )
    val results = basilOut.boogie.map(_.verifyBoogie())
    assertResult(true) {
      results.forall(_.kind.isVerified)
    }
  }
}
