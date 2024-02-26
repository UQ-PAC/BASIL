import analysis.ParamAnalysis
import ir.IRDSL.{R0, R1, R2}
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import util.BasilResult

class ParamAnalaysisTests extends AnyFunSuite, TestUtil{

  test("basic_arrays_write") {
    val result: BasilResult = runExample("basic_arrays_write")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0))
  }

  test("function") {
    val result: BasilResult = runExample("function")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set.empty)
  }

  test("basic_function_call_caller") {
    val result: BasilResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0))
    assert(analysisResults(procs("zero")) == Set.empty)
  }

  test("function1") {
    val result: BasilResult = runExample("function1")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set(R0, R1, R2))
  }

  test("ifbranches") {
    val result: BasilResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0, R1))
  }

  test("functions_with_params") {
    val result: BasilResult = runExample("functions_with_params")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("plus_one")) == Set(R0))
  }


  test("initialisation") {
    val result: BasilResult = runExample("initialisation")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set.empty)
  }

  test("jumptable") {
    val result: BasilResult = runExample("jumptable")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    assert(analysisResults(procs("main")) == Set(R0, R1))
  }

  ignore("unresolved_calls") {
    val result: BasilResult = runExample("jumptable")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.procs

    // not analysed due to unresolved call
    assert(analysisResults(procs("add_two")) == Set.empty)
    assert(analysisResults(procs("add_six")) == Set.empty)
    assert(analysisResults(procs("sub_seven")) == Set.empty)
  }
}
