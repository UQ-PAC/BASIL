import analysis.*
import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

@test_util.tags.UnitTest
class ProcedureAnnotationTests extends AnyFunSuite, CaptureOutput {

  def annotateProgram(program: Program) = {
    val foundLoops = LoopDetector.identify_loops(program)
    val newLoops = foundLoops.reducibleTransformIR().identifiedLoops
    foundLoops.updateIrWithLoops()

    val summaryGenerator = SummaryGenerator(program, true)
    program.procedures
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
}
