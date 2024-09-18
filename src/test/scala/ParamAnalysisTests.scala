import analysis.ParamAnalysis
import ir.dsl.{R0, R1, R2}
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest
import util.{BASILResult, StaticAnalysisConfig}

class ParamAnalysisTests extends AnyFunSuite, BASILTest {
  private val correctPath = "./src/test/correct/"

  def runExample(name: String): BASILResult = {
    val inputFile = correctPath + s"/$name/gcc/$name.adt"
    val relfFile = correctPath + s"/$name/gcc/$name.relf"
    val staticAnalysisConfig = Some(StaticAnalysisConfig())
    val outputFile = correctPath + s"/$name/gcc/${name}_paramanalysis.bpl"
    runBASIL(inputFile, relfFile, None, outputFile, staticAnalysisConfig)
  }

  test("basic_arrays_write") {
    val result: BASILResult = runExample("basic_arrays_write")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set(R0))
  }

  test("function") {
    val result: BASILResult = runExample("function")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set.empty)
  }

  test("basic_function_call_caller") {
    val result: BASILResult = runExample("basic_function_call_caller")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set(R0))
    assert(analysisResults(procs("zero")) == Set.empty)
  }

  test("function1") {
    val result: BASILResult = runExample("function1")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("get_two")) == Set(R0, R1, R2))
  }

  test("ifbranches") {
    val result: BASILResult = runExample("ifbranches")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set(R0, R1))
  }

  test("functions_with_params") {
    val result: BASILResult = runExample("functions_with_params")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set.empty)
    assert(analysisResults(procs("plus_one")) == Set(R0))
  }
  
  test("initialisation") {
    val result: BASILResult = runExample("initialisation")
    val analysisResults = result.analysis.get.paramResults
    val procs = result.ir.program.nameToProcedure

    assert(analysisResults(procs("main")) == Set.empty)
  }
}
