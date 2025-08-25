import ir.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.{BASILTest, CaptureOutput}
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, PCTrackingOption, StaticAnalysisConfig}

@test_util.tags.UnitTest
class RemovePCTest extends AnyFunSuite with CaptureOutput {

  def load(name: String, variation: String, keepPC: Boolean) = {
    util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.gts",
          relfFile = Some(s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.relf"),
          specFile = None,
          dumpIL = None,
          pcTracking = if keepPC then PCTrackingOption.Assert else PCTrackingOption.None
        ),
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out"
      )
    )
  }

  test("has no pc by default") {
    var p = load("cjump", "gcc", false).ir.program
    assertResult(false) {
      // println(p.mainProcedure.blocks.toList)
      p.procedures.forall(allVarsPos(_).contains(Register("_PC", 64)))
    }

    p = load("cjump", "gcc", true).ir.program
    assertResult(true) {
      p.procedures.forall(allVarsPos(_).contains(Register("_PC", 64)))
    }
  }

  test("cjump has 2 pc assigns") {
    // this test case has a single if branch.
    // it has two direct PC assignments, the rest being fall-through edges
    val loaded = load("cjump", "gcc", true)
    val p = loaded.ir.program
    assertResult(2) {
      p.mainProcedure
        .collect {
          case LocalAssign(Register("_PC", 64), BitVecLiteral(n, _), _) => {
            n
          }
        }
        .toList
        .size
    }

    val results = loaded.boogie.map(_.verifyBoogie())
    assertResult(true) {
      results.forall(_.kind.isVerified)
    }
  }

}
