import org.scalatest.funsuite.AnyFunSuite

import ir.*
import test_util.CaptureOutput
import util.{
  BASILConfig,
  IRContext,
  BoogieGeneratorConfig,
  ILLoadingConfig,
  StaticAnalysisConfig,
}

@test_util.tags.UnitTest
class RemovePCTest extends AnyFunSuite {

  def load(name: String, variation: String, keepPC: Boolean): IRContext = {
    util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = s"src/test/correct/$name/$variation/$name.gts",
          relfFile = s"src/test/correct/$name/$variation/$name.relf",
          specFile = None,
          dumpIL = None,
          keepPC = keepPC
        ),
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out"
      )
    ).ir
  }


  test("has no pc by default") {
    val p = load("cjump", "clang", false).program

    assertResult(false) {
      p.procedures.forall(allVarsPos(_).contains(Register("_PC", 64)))
    }
  }

}

