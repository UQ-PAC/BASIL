import analysis.{InterLiveVarsAnalysis, TwoElementTop}
import ir.dsl.*
import ir.{BitVecLiteral, BitVecType, dsl, LocalAssign, LocalVar, Program, Register, Statement, Variable, transforms, cilvisitor, Procedure}
import util.{Logger, LogLevel}
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest
import util.{BASILResult, StaticAnalysisConfig}

class LiveVarsAnalysisTests extends AnyFunSuite, BASILTest {
  private val correctPath = "./src/test/correct/"

  def runExample(name: String): BASILResult = {
    val inputFile = correctPath + s"/$name/gcc/$name.adt"
    val relfFile = correctPath + s"/$name/gcc/$name.relf"
    val staticAnalysisConfig = Some(StaticAnalysisConfig())
    val outputFile = correctPath + s"/$name/gcc/${name}_livevars.bpl"
    runBASIL(inputFile, relfFile, None, outputFile, staticAnalysisConfig)
  }

  def createSimpleProc(name: String, statements: Seq[Statement]): EventuallyProcedure = {
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
    val constant1 = bv64(1)
    val r0ConstantAssign = LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = LocalAssign(R2, R1, Some("00004"))

    val program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
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
      createSimpleProc("callee1", Seq(r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    // fix for DSA pairs of results?
    val procs = program.nameToProcedure
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop, R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop))
  }


  def differentCalleesOneAlive(): Unit = {
    val constant1 = bv64(1)
    val r0ConstantAssign = LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = LocalAssign(R2, R1, Some("00004"))
    val r1Reassign = LocalAssign(R1, BitVecLiteral(2, 64), Some("00005"))

    val program: Program = prog(
      proc("main",
        block("first_call",
          r0ConstantAssign,
          r1ConstantAssign,
          directCall("callee1"), goto("second_call")
        ),
        block("second_call",
          directCall("callee2"), goto("returnBlock")
        ),
        block("returnBlock",
          ret
        )
      ),
      createSimpleProc("callee1", Seq(r1Reassign, r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    val procs = program.nameToProcedure
    // assert(liveVarAnalysisResults(procs("main")) == Map())
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop))
  }

  def twoCallers(): Unit = {
    val constant1 = bv64(1)
    val r0ConstantAssign = LocalAssign(R0, constant1, Some("00001"))
    val r1Assign = LocalAssign(R0, R1, Some("00002"))
    val r2Assign = LocalAssign(R0, R2, Some("00003"))

    val program = prog(
      proc("main",
        block("main_first_call",
          directCall("wrapper1"),
          goto("main_second_call")
        ),
        block("main_second_call",
          directCall("wrapper2"),
          goto("main_return")
        ),
        block("main_return", ret)
      ),
      createSimpleProc("callee", Seq(r0ConstantAssign)),
      createSimpleProc("callee2", Seq(r1Assign)),
      createSimpleProc("callee3", Seq(r2Assign)),
      proc("wrapper1",
        block("wrapper1_first_call",
          LocalAssign(R1, constant1),
          directCall("callee"),
          goto("wrapper1_second_call")
        ),
        block("wrapper1_second_call",
          directCall("callee2"),
          goto("wrapper1_return")),
        block("wrapper1_return", ret)
      ),
      proc("wrapper2",
        block("wrapper2_first_call",
          LocalAssign(R2, constant1),
          directCall("callee"), goto("wrapper2_second_call")
        ),
        block("wrapper2_second_call",
          directCall("callee3"), goto("wrapper2_return")),
        block("wrapper2_return", ret)
      )
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.labelToBlock
    assert(liveVarAnalysisResults(blocks("wrapper1_first_call").jump) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("wrapper2_first_call").jump) == Map(R2 -> TwoElementTop))

  }

  def deadBeforeCall(): Unit = {
    val program = prog(
      proc("main",
        block("lmain",
          directCall("killer"), goto("aftercall")
        ),
        block("aftercall",
          LocalAssign(R0, R1),
          ret
        )
      ),
      createSimpleProc("killer", Seq(LocalAssign(R1, bv64(1))))
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.labelToBlock

    assert(liveVarAnalysisResults(blocks("aftercall")) == Map(R1 -> TwoElementTop))
    // assert(liveVarAnalysisResults(blocks("lmain")) == Map())
  }

  def simpleBranch(): Unit = {
    val r1Assign = LocalAssign(R0, R1, Some("00001"))
    val r2Assign = LocalAssign(R0, R2, Some("00002"))

    val program : Program = prog(
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

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val blocks = program.labelToBlock
    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    assert(liveVarAnalysisResults(blocks("branch1")) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("branch2")) == Map(R2 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))

  }

  def recursionInfinite(): Unit = { // can't handle this infinite recursion case
    val program : Program = prog(
      proc("main",
        block(
          "lmain",
          LocalAssign(R0, R1),
          directCall("main"), goto("return")
        ),
        block("return",
          LocalAssign(R0, R2),
          ret
        )
      )
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))
  }

  def recursionBaseCase(): Unit = {
    val program: Program = prog(
      proc("main",
        block("lmain",
          LocalAssign(R0, R1),
          goto("recursion", "non-recursion")
        ),
        block(
          "recursion",
          directCall("main"), goto("assign")
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

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))
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

  test("basic_arrays_write") {
    val result: BASILResult = runExample("basic_arrays_write")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    // main has a parameter, R0 should be alive
    assert(analysisResults(blocks("lmain")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("function") {
    val result: BASILResult = runExample("function")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    // checks function call blocks
    assert(analysisResults(blocks("lmain")) == Map(R29 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("lget_two")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(blocks("l00000946")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
  }

  test("basic_function_call_caller") {
    val result: BASILResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    // main has parameter, callee (zero) has return and no parameter
    assert(analysisResults(blocks("lmain")) == Map(R0 -> TwoElementTop, R29 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("lzero")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(blocks("l00000323")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
    assert(analysisResults(blocks("zero_basil_return")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("function1") {
    val result: BASILResult = runExample("function1")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    // main has no parameters, get_two has three and a return
    assert(analysisResults(blocks("lmain")) == Map(R29 -> TwoElementTop, R31 -> TwoElementTop, R30 -> TwoElementTop))
    assert(analysisResults(blocks("l000003ec")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // get_two aftercall
    assert(analysisResults(blocks("l00000430")) == Map(R31 -> TwoElementTop)) // printf aftercall
    assert(analysisResults(blocks("lget_two")) == Map(R0 -> TwoElementTop, R1 -> TwoElementTop, R2 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("get_two_basil_return")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("ifbranches") {
    val result: BASILResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    // block after branch
    assert(analysisResults(blocks("l00000342")) == Map(R31 -> TwoElementTop))
    // branch blocks
    assert(analysisResults(blocks("lmain_goto_l00000330")) == Map(Register("ZF", 1) -> TwoElementTop,
      R31 -> TwoElementTop))
    assert(analysisResults(blocks("lmain_goto_l00000369")) == Map(Register("ZF", 1) -> TwoElementTop,
      R31 -> TwoElementTop))
  }
}
