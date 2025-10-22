import analysis.{InterLiveVarsAnalysis, TwoElement, TwoElementTop}
import ir.dsl.*
import ir.{BitVecLiteral, LocalAssign, Program, Register, cilvisitor, transforms}
import ir.{BitVecLiteral, Block, CFGPosition, LocalAssign, Program, Register, Variable, cilvisitor, dsl, transforms}
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import util.{BASILResult, LogLevel, Logger, StaticAnalysisConfig}

@test_util.tags.UnitTest
class LiveVarsAnalysisTests extends AnyFunSuite, CaptureOutput, BASILTest {
  Logger.setLevel(LogLevel.ERROR)
  private val correctPath = s"${BASILTest.rootDirectory}/src/test/correct/"
  def runExample(name: String): BASILResult = {
    val inputFile = correctPath + s"/$name/gcc/$name.adt"
    val relfFile = correctPath + s"/$name/gcc/$name.relf"
    val staticAnalysisConfig = Some(StaticAnalysisConfig())
    val outputFile = correctPath + s"/$name/gcc/${name}_livevars.bpl"
    runBASIL(inputFile, relfFile, None, outputFile, staticAnalysisConfig)
  }

  def runAnalysis(program: Program): Map[CFGPosition, Map[Variable, TwoElement]] = {
    val replaceReturns = transforms.ReplaceReturns()
    cilvisitor.visit_prog(replaceReturns, program)
    replaceReturns.addR30Begins()
    transforms.addReturnBlocks(program)
    cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

    InterLiveVarsAnalysis(program).analyze()
  }

  def createSimpleProc(name: String, statements: Seq[NonCallStatement]): EventuallyProcedure = {
    proc(name, block("l" + name, statements.:+(goto(name + "_return")): _*), block(name + "_return", ret))
  }

  def differentCalleesBothLive(): Unit = {
    val constant1 = bv64(1)
    val r0ConstantAssign = LocalAssign(R0, constant1, Some("00001"))
    val r1ConstantAssign = LocalAssign(R1, constant1, Some("00002"))
    val r2r0Assign = LocalAssign(R2, R0, Some("00003"))
    val r2r1Assign = LocalAssign(R2, R1, Some("00004"))

    val program: Program = prog(
      proc(
        "main",
        block("first_call", r0ConstantAssign, r1ConstantAssign, directCall("callee1"), goto("second_call")),
        block("second_call", directCall("callee2"), goto("returnBlock")),
        block("returnBlock", ret)
      ),
      createSimpleProc("callee1", Seq(r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val liveVarAnalysisResults = runAnalysis(program)

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
      proc(
        "main",
        block("first_call", r0ConstantAssign, r1ConstantAssign, directCall("callee1"), goto("second_call")),
        block("second_call", directCall("callee2"), goto("returnBlock")),
        block("returnBlock", ret)
      ),
      createSimpleProc("callee1", Seq(r1Reassign, r2r0Assign)),
      createSimpleProc("callee2", Seq(r2r1Assign))
    )

    val liveVarAnalysisResults = runAnalysis(program)

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
      proc(
        "main",
        block("main_first_call", directCall("wrapper1"), goto("main_second_call")),
        block("main_second_call", directCall("wrapper2"), goto("main_return")),
        block("main_return", ret)
      ),
      createSimpleProc("callee", Seq(r0ConstantAssign)),
      createSimpleProc("callee2", Seq(r1Assign)),
      createSimpleProc("callee3", Seq(r2Assign)),
      proc(
        "wrapper1",
        block("wrapper1_first_call", LocalAssign(R1, constant1), directCall("callee"), goto("wrapper1_second_call")),
        block("wrapper1_second_call", directCall("callee2"), goto("wrapper1_return")),
        block("wrapper1_return", ret)
      ),
      proc(
        "wrapper2",
        block("wrapper2_first_call", LocalAssign(R2, constant1), directCall("callee"), goto("wrapper2_second_call")),
        block("wrapper2_second_call", directCall("callee3"), goto("wrapper2_return")),
        block("wrapper2_return", ret)
      )
    )

    val liveVarAnalysisResults = runAnalysis(program)
    val blocks = program.labelToBlock
    assert(liveVarAnalysisResults(blocks("wrapper1_first_call").jump) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("wrapper2_first_call").jump) == Map(R2 -> TwoElementTop))

  }

  def deadBeforeCall(): Unit = {
    val program = prog(
      proc(
        "main",
        block("lmain", directCall("killer"), goto("aftercall")),
        block("aftercall", LocalAssign(R0, R1), ret)
      ),
      createSimpleProc("killer", Seq(LocalAssign(R1, bv64(1))))
    )

    val liveVarAnalysisResults = runAnalysis(program)
    val blocks = program.labelToBlock

    assert(liveVarAnalysisResults(blocks("aftercall")) == Map(R1 -> TwoElementTop))
    // assert(liveVarAnalysisResults(blocks("lmain")) == Map())
  }

  def simpleBranch(): Unit = {
    val r1Assign = LocalAssign(R0, R1, Some("00001"))
    val r2Assign = LocalAssign(R0, R2, Some("00002"))

    val program: Program = prog(
      proc(
        "main",
        block("lmain", goto("branch1", "branch2")),
        block("branch1", r1Assign, goto("main_return")),
        block("branch2", r2Assign, goto("main_return")),
        block("main_return", ret)
      )
    )

    val liveVarAnalysisResults = runAnalysis(program)
    val blocks = program.labelToBlock

    assert(liveVarAnalysisResults(blocks("branch1")) == Map(R1 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("branch2")) == Map(R2 -> TwoElementTop))
    assert(liveVarAnalysisResults(blocks("lmain")) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))

  }

  def recursionInfinite(): Unit = { // can't handle this infinite recursion case
    val program: Program = prog(
      proc(
        "main",
        block("lmain", LocalAssign(R0, R1), directCall("main"), goto("return")),
        block("return", LocalAssign(R0, R2), ret)
      )
    )

    val liveVarAnalysisResults = runAnalysis(program)

    assert(liveVarAnalysisResults(program.mainProcedure) == Map(R1 -> TwoElementTop, R2 -> TwoElementTop))
  }

  def recursionBaseCase(): Unit = {
    val program: Program = prog(
      proc(
        "main",
        block("lmain", LocalAssign(R0, R1), goto("recursion", "non-recursion")),
        block("recursion", directCall("main"), goto("assign")),
        block("assign", LocalAssign(R0, R2), goto("return")),
        block("non-recursion", goto("return")),
        block("return", ret)
      )
    )

    val liveVarAnalysisResults = runAnalysis(program)

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
    assert(analysisResults(blocks("main_entry")) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop))
  }

  test("function") {
    val result: BASILResult = runExample("function")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    val lmain = blocks("main_entry")
    val laftercall = lmain.singleSuccessor.head
    // checks function call blocks
    assert(analysisResults(lmain) == Map(R29 -> TwoElementTop, R30 -> TwoElementTop, R31 -> TwoElementTop))
    assert(analysisResults(blocks("get_two_entry")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(laftercall) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
  }

  test("basic_function_call_caller") {
    val result: BASILResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    val lmain = blocks("main_entry")
    val laftercall = lmain.singleSuccessor.head
    // main has parameter, callee (zero) has return and no parameter
    assert(
      analysisResults(lmain) == Map(
        R0 -> TwoElementTop,
        R29 -> TwoElementTop,
        R30 -> TwoElementTop,
        R31 -> TwoElementTop
      )
    )
    assert(analysisResults(blocks("zero_entry")) == Map(R31 -> TwoElementTop))
    assert(analysisResults(laftercall) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)) // aftercall block
    assert(
      analysisResults(blocks("zero_entry").parent.returnBlock.get) == Map(R0 -> TwoElementTop, R31 -> TwoElementTop)
    )
  }

  test("function1") {
    val result: BASILResult = runExample("function1")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    val lmain = blocks("main_entry")
    val l_get_two_aftercall = lmain.singleSuccessor.head
    val l_printf_aftercall = l_get_two_aftercall.singleSuccessor.head
    // main has no parameters, get_two has three and a return
    // We have substantially overapproximated due to printf's set overapproximating
    val main = Map(
      Register("R16", 64) -> TwoElementTop,
      Register("R8", 64) -> TwoElementTop,
      Register("R14", 64) -> TwoElementTop,
      Register("R5", 64) -> TwoElementTop,
      Register("R15", 64) -> TwoElementTop,
      Register("R18", 64) -> TwoElementTop,
      Register("R17", 64) -> TwoElementTop,
      Register("R29", 64) -> TwoElementTop,
      Register("R31", 64) -> TwoElementTop,
      Register("R7", 64) -> TwoElementTop,
      Register("R9", 64) -> TwoElementTop,
      Register("R12", 64) -> TwoElementTop,
      Register("R4", 64) -> TwoElementTop,
      Register("R10", 64) -> TwoElementTop,
      Register("R3", 64) -> TwoElementTop,
      Register("R11", 64) -> TwoElementTop,
      Register("R13", 64) -> TwoElementTop,
      Register("R6", 64) -> TwoElementTop
    )
    assert(analysisResults(lmain) == main ++ Map(Register("R30", 64) -> TwoElementTop))
    assert(
      analysisResults(l_get_two_aftercall) == main ++ Map(
        Register("R0", 64) -> TwoElementTop,
        Register("R2", 64) -> TwoElementTop
      )
    ) // get_two aftercall
    assert(analysisResults(l_printf_aftercall) == Map(R31 -> TwoElementTop)) // printf aftercall
    assert(
      analysisResults(blocks("get_two_entry")) == main ++ Map(
        R0 -> TwoElementTop,
        R1 -> TwoElementTop,
        R2 -> TwoElementTop
      )
    )
    assert(
      analysisResults(blocks("get_two_entry").parent.returnBlock.get) == main ++ Map(
        R0 -> TwoElementTop,
        R2 -> TwoElementTop,
        R31 -> TwoElementTop
      )
    )
  }

  test("ifbranches") {
    val result: BASILResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.interLiveVarsResults
    val blocks = result.ir.program.labelToBlock

    val gotoBlocks = result.ir.program.procedures
      .flatMap(_.blocks)
      .filter(_.nextBlocks.size > 1)
      .flatMap(b => b.nextBlocks.map(nb => nb.label -> nb))
      .toMap
    assert(gotoBlocks.size == 2)

    val blockAfterBranch = gotoBlocks.values.map(_.singleSuccessor.head.singleSuccessor.head).toSet
    assert(blockAfterBranch.size == 1)

    // block after branch
    assert(analysisResults(blockAfterBranch.head) == Map(R31 -> TwoElementTop))
    // branch blocks
    for ((_, b) <- gotoBlocks) {
      assert(analysisResults(b) == Map(Register("ZF", 1) -> TwoElementTop, R31 -> TwoElementTop))
    }
  }
}
