import org.scalatest.funsuite.AnyFunSuite
import util.{IRLoading, ILLoadingConfig, Logger}
import ir.*

@test_util.tags.UnitTest
class GTIRBLoaderTest extends AnyFunSuite {


  def withLogCapture[T](body: => T): (T, String) = {
    val outStream = java.io.ByteArrayOutputStream()
    val errorLog = java.io.PrintStream(outStream)

    Logger.setOutput(errorLog)

    val x = body

    (x, outStream.toString)
  }

  def loadGTIRB(relfFile: String, gtsFile: String) = {
    val config =
      ILLoadingConfig(gtsFile, relfFile)
    val (symbols, externalFunctions, globals, funcEntries, globalOffsets, mainAddress) =
      IRLoading.loadReadELF(relfFile, config)

    IRLoading.loadGTIRB(gtsFile, mainAddress)
  }

  test("sdiv loads with ite expression") {
    val (prog, errs) = withLogCapture {
      loadGTIRB("src/test/correct/sdiv/gcc_O2/sdiv.relf", "src/test/correct/sdiv/gcc_O2/sdiv.gts")
    }
    assert(!(errs.contains("\"ite.0\"")))
    val assertFalses = prog.collect { case x@Assert(FalseLiteral, _, _) => x }.toList
    withClue(errs) {
      assert(assertFalses.isEmpty)
    }
  }
}
