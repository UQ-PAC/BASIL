import org.scalatest.funsuite.AnyFunSuite
import test_util.TestUtil
import util.{BASILConfig, BoogieGeneratorConfig, ILLoadingConfig, RunUtils, StaticAnalysisConfig}

class RegionBuilderTests extends AnyFunSuite, TestUtil {

    for (p <- correctPrograms) {
      val programPath = correctPath + "/" + p
      val variations = getSubdirectories(programPath)
      variations.foreach(t =>
        test("Correct" + "/" + p + "/" + t) {
          RunUtils.loadAndTranslate(
            BASILConfig(
              loading = ILLoadingConfig(
                inputFile = correctPath + s"/$p/$t/$p.adt",
                relfFile = correctPath + s"/$p/$t/$p.relf",
                specFile = None,
                dumpIL = None
              ),
              staticAnalysis = Some(StaticAnalysisConfig(None)),
              boogieTranslation = BoogieGeneratorConfig(),
              outputPrefix = "boogie_out",
            )
          )
        }
      )
    }
}
