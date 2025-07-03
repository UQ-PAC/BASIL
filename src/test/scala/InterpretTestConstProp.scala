import analysis.{AnalysisPipelineMRA, *}
import ir.{IRLoading, *}
import org.scalatest.*
import org.scalatest.funsuite.*
import test_util.{BASILTest, CaptureOutput, TestValueDomainWithInterpreter}
import util.{ILLoadingConfig, LogLevel, Logger, StaticAnalysisConfig}

@test_util.tags.StandardSystemTest
class InterpretTestConstProp
    extends AnyFunSuite
    with CaptureOutput
    with TestValueDomainWithInterpreter[FlatElement[BitVecLiteral]] {

  Logger.setLevel(LogLevel.ERROR)

  def valueInAbstractValue(absval: FlatElement[BitVecLiteral], concrete: Expr): Expr = {
    absval match {
      case Top => TrueLiteral
      case Bottom => TrueLiteral /* deliberately don't check */
      case FlatEl(value) => BinaryExpr(EQ, value, concrete)
    }
  }

  private val testPath = s"${BASILTest.rootDirectory}/src/test/correct"
  def testInterpretConstProp(testName: String, compiler: String): Unit = {
    val path = s"$testPath/$testName/$compiler/$testName"
    val loading = ILLoadingConfig(inputFile = s"$path.adt", relfFile = Some(s"$path.relf"), dumpIL = None)

    val ictx = IRLoading.load(loading)
    ir.transforms.doCleanupWithoutSimplify(ictx, AnalysisManager(ictx.program))
    ir.transforms.clearParams(ictx.program)
    val analyses = AnalysisPipelineMRA.runToFixpoint(StaticAnalysisConfig(None, None, None), ictx)

    val analysisres = analyses.intraProcConstProp.collect { case (block: Block, v) =>
      block -> v
    }

    val result = runTestInterpreter(ictx, analysisres)
    assert(result.getFailures.isEmpty)
    info(s"${result.checksPassed.size} checks passed")

  }

  test("function1/clang") {
    testInterpretConstProp("function1", "clang")
  }

  test("function1/gcc") {
    testInterpretConstProp("function1", "gcc")
  }

  test("secret_write/clang") {
    testInterpretConstProp("secret_write", "clang")
  }

  test("secret_write/gcc") {
    testInterpretConstProp("secret_write", "gcc")
  }
}
