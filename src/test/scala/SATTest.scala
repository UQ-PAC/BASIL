import analysis.ParamAnalysis
import ir.dsl.*
import ir.*
import munit.FunSuite
import test_util.BASILTest
import util.*
import translating.BasilIRToSMT2

import org.junit.experimental.categories.Category
import test_util.UnitTest

@Category(Array(classOf[UnitTest]))
class SATTest extends FunSuite {
  test(" basic taut ") {
    val e = BinaryExpr(BoolEQ, BinaryExpr(BVNEQ, R0, bv64(0)), BinaryExpr(BVEQ, bv64(0), R0))
    val r = BasilIRToSMT2.proveExpr(e)
    assertEquals(r, Some(true))
  }
}
