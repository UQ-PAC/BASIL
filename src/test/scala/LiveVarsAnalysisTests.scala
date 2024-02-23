import analysis.{InterLiveVarsAnalysis, TwoElementTop}
import ir.IRDSL.EventuallyJump
import ir.{BitVecLiteral, BitVecType, ConvertToSingleProcedureReturn, IRDSL, LocalAssign, Program, Register, Statement, Variable}
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil


class LiveVarsAnalysisTests extends AnyFunSuite, TestUtil {

  override val testPath = "./src/test/analysis/livevars/"
  override val resultsFile = "livevar_analysis_results"
  override val dumpPath: String = testPath + "dump/"
  override val resultParser: String => String = InterLiveVarsAnalysis.parseAnalysisResults

  // runs analysis phase on all correct and incorrect programs
//  for (p <- correctPrograms) {
//    val path = correctPath + p
//    val variations = getSubdirectories(path)
//    variations.foreach(t =>
//      test("correct/" + p + "/" + t) {
//        runTest(correctPath, p, t)
//      }
//    )
//  }
//
//  for (p <- incorrectPrograms) {
//    val path = incorrectPath +  p
//    val variations = getSubdirectories(path)
//    variations.foreach(t =>
//      test("incorrect/" + p + "/" + t) {
//        runTest(incorrectPath, p, t)
//      }
//    )
//  }

  def createSimpleProc(name: String, statements: Seq[Statement | EventuallyJump]): IRDSL.EventuallyProcedure = {
    import IRDSL._
    proc(name,
      block("l" + name,
        statements.:+(goto(name + "_return")): _*
      ),
      block(name + "_return",
        ret
      )
    )
  }

  def differentCalleesBothLive(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = new LocalAssign(R2, R1, Some("00004"))

    var program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
          call("callee1", Some("second_call"))
        ),
        block("second_call",
          call("callee2", Some("returnBlock"))
        ),
        block("returnBlock",
          ret
        )
      ),
      createSimpleProc("callee1", Seq(r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    val procs = program.procs
    assert(liveVarAnalysisResults(procs("main")) == Map(r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop, R1 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop, r30 -> TwoElementTop))
  }


  def differentCalleesOneAlive(): Unit = {
    import IRDSL._
    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = new LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = new LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = new LocalAssign(R2, R1, Some("00004"))
    val r1Reassign = new LocalAssign(R1, BitVecLiteral(2, 64), Some("00005"))

    var program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
          call("callee1", Some("second_call"))
        ),
        block("second_call",
          call("callee2", Some("returnBlock"))
        ),
        block("returnBlock",
          ret
        )
      ),
      createSimpleProc("callee1", Seq(r1Reassign, r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    val procs = program.procs
    assert(liveVarAnalysisResults(procs("main")) == Map(r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop, r30 -> TwoElementTop))
  }

  def twoCallers(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val constant1 = bv64(1)
    val r0ConstantAssign = new LocalAssign(R0, constant1, Some("00001"))
    val r0Reassign = new LocalAssign(R0, BitVecLiteral(2, 64), Some("00004"))
    val r1Assign = new LocalAssign(R0, R1, Some("00002"))
    val r2Assign = new LocalAssign(R0, R2, Some("00003"))

    var program = prog(
      proc("main",
        block("main_first_call",
          call("wrapper1", Some("main_second_call"))
        ),
        block("main_second_call",
          call("wrapper2", Some("main_return"))
        ),
        block("main_return", ret)
      ),
      createSimpleProc("callee", Seq(r0ConstantAssign)),
      createSimpleProc("callee2", Seq(r1Assign)),
      createSimpleProc("callee3", Seq(r2Assign)),
      proc("wrapper1",
        block("wrapper1_first_call",
          LocalAssign(R1, constant1),
          call("callee", Some("wrapper1_second_call"))
        ),
        block("wrapper1_second_call",
          call("callee2", Some("wrapper1_return"))),
        block("wrapper1_return", ret)
      ),
      proc("wrapper2",
        block("wrapper2_first_call",
          LocalAssign(R2, constant1),
          call("callee", Some("wrapper2_second_call"))
        ),
        block("wrapper2_second_call",
          call("callee3", Some("wrapper2_return"))),
        block("wrapper2_return", ret)
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks
    assert(liveVarAnalysisResults(blocks("wrapper1_first_call").jump) == Map(R1 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("wrapper2_first_call").jump) == Map(R2 -> TwoElementTop, r30 -> TwoElementTop))

  }

  def deadBeforeCall(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    var program = prog(
      proc("main",
        block("lmain",
          call("killer", Some("aftercall"))
        ),
        block("aftercall",
          LocalAssign(R0, R1),
          ret
        )
      ),
      createSimpleProc("killer", Seq(LocalAssign(R1, bv64(1))))
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(blocks("aftercall")) == Map(R1 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(r30 -> TwoElementTop))
  }

  def simpleBranch(): Unit = {
    import IRDSL._

    val r30 = Register("R30", BitVecType(64))
    val r1Assign = new LocalAssign(R0, R1, Some("00001"))
    val r2Assign = new LocalAssign(R0, R2, Some("00002"))

    var program : Program = prog(
      proc(
        "main",
        block(
          "lmain",
          goto("branch1", "branch2")
        ),
        block(
          "branch1",
          r1Assign,
          goto("main_return")
        ),
        block(
          "branch2",
          r2Assign,
          goto("main_return")
        ),
        block("main_return", ret)
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val blocks = program.blocks
    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    assert(liveVarAnalysisResults(blocks("branch1")) == Map(R1 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("branch2")) == Map(R2 -> TwoElementTop, r30 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop, r30 -> TwoElementTop))

  }

  def recursionInfinite(): Unit = { // can't handle this infinite recursion case
    import IRDSL._
    val r30 = Register("R30", BitVecType(64))
    var program : Program = prog(
      proc("main",
        block(
          "lmain",
          LocalAssign(R0, R1),
          call("main", Some("return"))
        ),
        block("return",
          LocalAssign(R0, R2),
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop, r30 -> TwoElementTop))
  }

  def recursionBaseCase(): Unit = {
    import IRDSL._
    val r30 = Register("R30", BitVecType(64))
    var program: Program = prog(
      proc("main",
        block("lmain",
          LocalAssign(R0, R1),
          goto("recursion", "non-recursion")
        ),
        block(
          "recursion",
          call("main", Some("assign"))
        ),
        block("assign",
          LocalAssign(R0, R2),
          goto("return")
        ),
        block(
          "non-recursion",
          goto("return")
        ),
        block("return",
          ret
        )
      )
    )

    val returnUnifier = ConvertToSingleProcedureReturn()
    program = returnUnifier.visitProgram(program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop, r30 -> TwoElementTop))
  }

  test("differentCalleesBothAlive") {
    differentCalleesBothLive()
  }

  test("differentCalleesOneAlive") {
    differentCalleesOneAlive()
  }

  test("twoCallers") {
    twoCallers()
  }

  test("deadBeforeCall") {
    deadBeforeCall()
  }

  test("simpleBranch") {
    simpleBranch()
  }

  ignore("recursionInfinite") {
    recursionInfinite()
  }

  test("recursionBaseCase") {
    recursionBaseCase()
  }

//  test("basic_array_write") {
//    runTest(examplePath, "basic_arrays_write", example = true)
//  }
//
//  test("function") {
//    runTest(examplePath, "function", example = true)
//  }
//
//  test("basic_function_call_caller") {
//    runTest(examplePath, "basic_function_call_caller", example = true)
//  }
//
//  test("function1") {
//    runTest(examplePath, "function1", example = true)
//  }
//
//  test("ifbranches") {
//    runTest(examplePath, "ifbranches", example = true)
//  }
}
