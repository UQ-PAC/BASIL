import ir.{IRContext, *}
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import util.{
  BASILConfig,
  BoogieGeneratorConfig,
  ILLoadingConfig,
  IRContext,
  PCTrackingOption,
  SimplifyMode,
  StaticAnalysisConfig
}

@test_util.tags.UnitTest
class PCTrackingTest extends AnyFunSuite with CaptureOutput {

  def load(name: String, variation: String, pcTracking: PCTrackingOption, simplify: Boolean = false) = {
    util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.gts",
          relfFile = Some(s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.relf"),
          specFile = None,
          dumpIL = None,
          pcTracking = pcTracking
        ),
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        simplify = if simplify then SimplifyMode.Simplify else SimplifyMode.Disabled
      )
    )
  }

  test("mode: keep pc") {
    var p = load("ifbranches", "clang", PCTrackingOption.Keep).ir.program
    assertResult(Nil) {
      val asserts = p.collect {
        case x: Assert if allVarsPos(x).map(_.name).contains("_PC") =>
          x
      }
      println(asserts)
      asserts
    }

  }

  test("mode: assert") {
    val loaded = load("ifbranches", "clang", PCTrackingOption.Assert)
    val p = loaded.ir.program
    assertResult(5) {
      val asserts = p.collect {
        case x: Assert if allVarsPos(x).map(_.name).contains("_PC") =>
          x
      }
      println(asserts)
      asserts.size
    }

    val results = loaded.boogie.map(_.verifyBoogie())

    // XXX: test broken until .gts files are updated with fixed PC offsets
    pendingUntilFixed {
      assertResult(List(true)) {
        results.map(_.kind.isVerified)
      }
    }
  }

  test("mode: assert. with simplify") {
    val loaded = load("ifbranches", "clang", PCTrackingOption.Assert, true)
    val p = loaded.ir.program

    val results = loaded.boogie.map(_.verifyBoogie())

    // XXX: test broken until .gts files are updated with fixed PC offsets
    pendingUntilFixed {
      assertResult(List(true)) {
        results.map(_.kind.isVerified)
      }
    }
  }

}
