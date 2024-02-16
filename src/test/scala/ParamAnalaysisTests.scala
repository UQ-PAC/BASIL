import analysis.ParamAnalysis
import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil

class ParamAnalaysisTests extends AnyFunSuite, TestUtil{


  override val testPath = "./src/test/analysis/params/"
  override val dumpPath: String = testPath + "dump/"
  override val resultsFile: String = "param_analysis_results"
  override val resultParser: String => String = ParamAnalysis.parseAnalysisResults


  test("basic_array_write") {
    runTest(examplePath, "basic_arrays_write", example = true)
  }

  test("function") {
    runTest(examplePath, "function", example = true)
  }

  test("basic_function_call_caller") {
    runTest(examplePath, "basic_function_call_caller", example = true)
  }

  test("function1") {
    runTest(examplePath, "function1", example = true)
  }

  test("ifbranches") {
    runTest(examplePath, "ifbranches", example = true)
  }
  test("functions_with_params") {
    runTest(examplePath, "functions_with_params", example = true)
  }

  test("indirect_call_outparam") {
    runTest(examplePath, "indirect_call_outparam", example = true)
  }

  test("initialisation") {
    runTest(examplePath, "initialisation", example = true)
  }

  test("jumptable") {
    runTest(examplePath, "jumptable", example = true)
  }


}
