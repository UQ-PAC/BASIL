import analysis.ParamAnalysis
import ir.dsl.*
import ir.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.BASILTest
import util.*
import translating.BasilIRToSMT2

@test_util.tags.UnitTest
class SATTest extends AnyFunSuite with test_util.CaptureOutput {
  test(" basic taut ") {
    // Logger.setLevel(LogLevel.DEBUG)
    val e = BinaryExpr(BoolEQ, BinaryExpr(BVNEQ, R0, bv64(0)), BinaryExpr(BVEQ, bv64(0), R0))
    val r = BasilIRToSMT2.proveExpr(e)
    assert(r == Some(true))
  }
}
