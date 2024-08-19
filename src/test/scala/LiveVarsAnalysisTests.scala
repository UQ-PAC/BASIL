import analysis.{InterLiveVarsAnalysis, TwoElementTop}
import ir.dsl.*
import ir.{BitVecLiteral, BitVecType, dsl, Assign, LocalVar, Program, Register, Statement, Variable, transforms, cilvisitor, Procedure}
import util.{Logger, LogLevel}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import util.BASILResult


class LiveVarsAnalysisTests extends AnyFunSuite, TestUtil {
  Logger.setLevel(LogLevel.ERROR)

  def createSimpleProc(name: String, statements: Seq[Statement | EventuallyJump]): EventuallyProcedure = {
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
    val r0ConstantAssign = Assign(R0, constant1, Some("00001"))
    val r1ConstantAssign = Assign(R1, constant1, Some("00002"))
    val r2r0Assign = Assign(R2, R0, Some("00003"))
    val r2r1Assign = Assign(R2, R1, Some("00004"))

    var program: Program = prog(
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
    val procs = program.procs
    println(liveVarAnalysisResults.filter((k,n) => k match {
      case p => true
      case _ => false
    }))
    // assert(liveVarAnalysisResults(procs("main")) == Map(R30 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop, R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop))
  }


  def differentCalleesOneAlive(): Unit = {
    val constant1 = bv64(1)
    val r0ConstantAssign = Assign(R0, constant1, Some("00001"))
    val r1ConstantAssign = Assign(R1, constant1, Some("00002"))
    val r2r0Assign = Assign(R2, R0, Some("00003"))
    val r2r1Assign = Assign(R2, R1, Some("00004"))
    val r1Reassign = Assign(R1, BitVecLiteral(2, 64), Some("00005"))

    var program: Program = prog(
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

    val procs = program.procs
    // assert(liveVarAnalysisResults(procs("main")) == Map())
    assert(liveVarAnalysisResults(procs("callee1")) == Map(R0 -> TwoElementTop))
    assert(liveVarAnalysisResults(procs("callee2")) == Map(R1 -> TwoElementTop))
  }

  def twoCallers(): Unit = {

    val constant1 = bv64(1)
    val r0ConstantAssign = Assign(R0, constant1, Some("00001"))
    val r0Reassign = Assign(R0, BitVecLiteral(2, 64), Some("00004"))
    val r1Assign = Assign(R0, R1, Some("00002"))
    val r2Assign = Assign(R0, R2, Some("00003"))

    var program = prog(
      proc("main",
        block("main_first_call",
          directCall("wrapper1"), goto("main_second_call")
        ),
        block("main_second_call",
          directCall("wrapper2"), goto("main_return")
        ),
        block("main_return", ret)
      ),
      createSimpleProc("callee", Seq(r0ConstantAssign)),
      createSimpleProc("callee2", Seq(r1Assign)),
      createSimpleProc("callee3", Seq(r2Assign)),
      proc("wrapper1",
        block("wrapper1_first_call",
          Assign(R1, constant1),
          directCall("callee"), goto("wrapper1_second_call")
        ),
        block("wrapper1_second_call",
          directCall("callee2"), goto("wrapper1_return")),
        block("wrapper1_return", ret)
      ),
      proc("wrapper2",
        block("wrapper2_first_call",
          Assign(R2, constant1),
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
    val blocks = program.blocks
    assert(liveVarAnalysisResults(blocks("wrapper1_first_call").jump) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("wrapper2_first_call").jump) == Map(R2 -> TwoElementTop))

  }

  def deadBeforeCall(): Unit = {
    var program = prog(
      proc("main",
        block("lmain",
          directCall("killer"), goto("aftercall")
        ),
        block("aftercall",
          Assign(R0, R1),
          ret
        )
      ),
      createSimpleProc("killer", Seq(Assign(R1, bv64(1))))
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(blocks("aftercall")) == Map(R1 -> TwoElementTop))
    // assert(liveVarAnalysisResults(blocks("lmain")) == Map())
  }

  def simpleBranch(): Unit = {
    val r1Assign = Assign(R0, R1, Some("00001"))
    val r2Assign = Assign(R0, R2, Some("00002"))

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

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val blocks = program.blocks
    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()

    assert(liveVarAnalysisResults(blocks("branch1")) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("branch2")) == Map(R2 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))

  }

  def recursionInfinite(): Unit = { // can't handle this infinite recursion case
    import dsl._
    var program : Program = prog(
      proc("main",
        block(
          "lmain",
          Assign(R0, R1),
          directCall("main"), goto("return")
        ),
        block("return",
          Assign(R0, R2),
          ret
        )
      )
    )

    cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    val liveVarAnalysisResults = InterLiveVarsAnalysis(program).analyze()
    val blocks = program.blocks

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))
  }

  def recursionBaseCase(): Unit = {
    import dsl._
    var program: Program = prog(
      proc("main",
        block("lmain",
          Assign(R0, R1),
          goto("recursion", "non-recursion")
        ),
        block(
          "recursion",
          directCall("main"), goto("assign")
        ),
        block("assign",
          Assign(R0, R2),
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
    val blocks = program.blocks

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
    val blocks = result.ir.program.blocks

    // main has a parameter, R0 should be alive
    assert(analysisResults(blocks("lmain")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("function") {
    val result: BASILResult = runExample("function")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.blocks

    // checks function call blocks
    assert(analysisResults(blocks("lmain")) == Map(R29 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("lget_two")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(blocks("l00000946")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
  }



  test("basic_function_call_caller") {
    val result: BASILResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.blocks

    // main has parameter, callee (zero) has return and no parameter
    assert(analysisResults(blocks("lmain")) == Map(R0 -> TwoElementTop, R29 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("lzero")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(blocks("l00000323")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
    assert(analysisResults(blocks("zero_basil_return")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("function1") {
    val result: BASILResult = runExample("function1")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.blocks

    // main has no parameters, get_two has three and a return
    assert(analysisResults(blocks("lmain").jump) == Map(R29 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("l000003ec").jump) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // get_two aftercall
    assert(analysisResults(blocks("l00000430").jump) == Map(R31 -> TwoElementTop)) // printf aftercall
    assert(analysisResults(blocks("main_basil_return").jump) == Map(R30 -> TwoElementTop))
    assert(analysisResults(blocks("lget_two").jump) == Map(R0 -> TwoElementTop, R1 -> TwoElementTop, R2 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("get_two_basil_return").jump) == Map(R0 -> TwoElementTop,  R30 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("ifbranches") {
    val result: BASILResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.blocks

    // block after branch
    assert(analysisResults(blocks("l00000342")) == Map(R31 -> TwoElementTop))
    // branch blocks
    assert(analysisResults(blocks("lmain_goto_l00000330")) == Map(Register("ZF", 1) -> TwoElementTop,
      R31 -> TwoElementTop))
    assert(analysisResults(blocks("lmain_goto_l00000369")) == Map(Register("ZF", 1) -> TwoElementTop,
      R31 -> TwoElementTop))
  }
}
