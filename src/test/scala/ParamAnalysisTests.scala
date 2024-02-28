import analysis.ParamAnalysis
import ir.dsl.{R0, R1, R2}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import util.BASILResult

class ParamAnalysisTests extends AnyFunSuite, TestUtil{

  test("basic_arrays_write") {
    val result: BASILResult = runExample("basic_arrays_write")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0))
  }

  test("function") {
    val result: BASILResult = runExample("function")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set.empty)
  }

  test("basic_function_call_caller") {
    val result: BASILResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0))
    assert(analysisResults(procs("zero")) == Set.empty)
  }

  test("function1") {
    val result: BASILResult = runExample("function1")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set(R0, R1, R2))
  }

  test("ifbranches") {
    val result: BASILResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0, R1))
  }

  test("functions_with_params") {
    val result: BASILResult = runExample("functions_with_params")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("plus_one")) == Set(R0))
  }


  test("initialisation") {
    val result: BASILResult = runExample("initialisation")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults.isEmpty)
  }

  test("jumptable") {
    val result: BASILResult = runExample("jumptable")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0, R1))
  }

  ignore("unresolved_calls") {
    val result: BASILResult = runExample("jumptable")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    // not analysed due to unresolved call
    assert(analysisResults(procs("add_two")) == Set.empty)
    assert(analysisResults(procs("add_six")) == Set.empty)
    assert(analysisResults(procs("sub_seven")) == Set.empty)
  }
}
