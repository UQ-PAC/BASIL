import org.scalatest.funsuite.AnyFunSuite

import ir.*
import test_util.{BASILTest, CaptureOutput}
import util.{BASILConfig, IRContext, BoogieGeneratorConfig, ILLoadingConfig, StaticAnalysisConfig, PCTrackingOption}

import java.nio.file.{Path, Files}
import java.io.{BufferedWriter, FileWriter}

@test_util.tags.UnitTest
class PCTrackingTest extends AnyFunSuite with CaptureOutput {

  def load(name: String, variation: String, pcTracking: PCTrackingOption, simplify: Boolean = false) = {
    util.RunUtils.loadAndTranslate(
      BASILConfig(
        loading = ILLoadingConfig(
          inputFile = s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.gts",
          relfFile = s"${BASILTest.rootDirectory}/src/test/correct/$name/$variation/$name.relf",
          specFile = None,
          dumpIL = None,
          pcTracking = pcTracking
        ),
        staticAnalysis = Some(StaticAnalysisConfig(None)),
        boogieTranslation = BoogieGeneratorConfig(),
        outputPrefix = "boogie_out",
        simplify = simplify
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
