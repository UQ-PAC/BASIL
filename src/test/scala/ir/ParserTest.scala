package ir

import ir.parsing.ParseBasilIL
import org.scalatest.funsuite.AnyFunSuite
import test_util.CaptureOutput

import scala.collection.immutable.*

@test_util.tags.UnitTest
class ParserTest extends AnyFunSuite with CaptureOutput {

  override def withFixture(test: NoArgTest) = {
    DeepEquality.debug.withValue(true) {
      super.withFixture(test)
    }
  }

  val minimal = """
prog entry @main_1812;

proc @main_1812 () -> ()
  { .name = "main"; .address = 0x714 }
[
  block %main_basil_return_1 [
    return ();
  ]
];
  """;

  test("minimal") {
    ParseBasilIL.loadILString(minimal)
  }

  test("minimal + declare-fun") {
    ParseBasilIL.loadILString(minimal + """
      declare-fun $FPToFixed_bv64_bv1123_bool_bv32_bv1123_bv32 : (bv64, bv1123, bool, bv32, bv1123) -> bv32;
    """)
  }
}
