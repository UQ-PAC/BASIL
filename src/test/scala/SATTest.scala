import ir.*
import ir.dsl.*
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput
import translating.BasilIRToSMT2

@test_util.tags.UnitTest
class SATTest extends AnyFunSuite with CaptureOutput {
  test(" basic taut ") {
    // Logger.setLevel(LogLevel.DEBUG)
    val e = BinaryExpr(EQ, BinaryExpr(NEQ, R0, bv64(0)), BinaryExpr(EQ, bv64(0), R0))
    val r = BasilIRToSMT2.proveExpr(e)
    assert(r == Some(true))
  }
}
