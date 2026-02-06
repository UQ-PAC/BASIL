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

  test("named memory") {
    ParseBasilIL.loadILString("""
memory $stack:(bv64->bv8);

proc @main_1876()
    -> ()
[
    block %main_entry [
      $stack:(bv64->bv8) := store le $stack:(bv64->bv8) 0xffffffffffffffe0:bv64 0xffffffffffffffe0:bv64 64;
      var load18:bv32 := load le $stack:(bv64->bv8) 0xffffffffffffffe0:bv64 32;
      return;
    ];
];
    """);
  }

  test("basic ssa") {
    val p = ParseBasilIL.loadILString("""
prog entry @main_1876;
proc @main_1876(ZF_in:bv1, VF_in:bv1, R31_in:bv64, R30_in:bv64, R29_in:bv64,
  R1_in:bv64, R0_in:bv64, NF_in:bv1, CF_in:bv1)
  -> ()
[
  block %inputs [
    (var R29_test:bv64 := R29_in:bv64);
    goto (%inputs2, %inputs3);
  ];

  block %inputs2
    (var R29:bv64 := (%inputs -> R29_in:bv64, %inputs2 -> R30_in:bv64))
  [
    goto (%inputs);
  ];

  block %inputs3
    (var x:bv64 := (%inputs -> R31_in:bv64))
  [
    goto (%inputs, %inputs2);
  ];
];
    """);

    assertResult("""prog entry @main_1876;

proc @main_1876
  (CF_in:bv1, NF_in:bv1, R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, VF_in:bv1, ZF_in:bv1)
    -> ()
  { .name = "main_1876" }
[
  block %inputs [
    ( var R29_test: bv64 := R29_in:bv64 );
    ( var R29: bv64 := R29_in:bv64,
      var x: bv64 := R31_in:bv64 ) { .label = "phi" };
    goto(%inputs2, %inputs3);
  ];
  block %inputs2 [
    ( var R29: bv64 := R30_in:bv64 ) { .label = "phi" };
    goto(%inputs);
  ];
  block %inputs3 [
    goto(%inputs, %inputs2);
  ]
];""".replace("\\r", "")) { p.program.toString.trim }

  }

  test("call params") {

    val p = ParseBasilIL.loadILString("""
proc @main_1876
  (CF_in:bv1, NF_in:bv1, R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, VF_in:bv1, ZF_in:bv1)
    -> ()
  { .name = "main_1876" }
[
  block %entry [
    (var ZF_7:bv1=ZF_out, var VF_7:bv1=VF_out, var R31_6:bv64=R31_out,
      var R30_6:bv64=R30_out, var R29_6:bv64=R29_out, var R1_6:bv64=R1_out,
      var R0_23:bv64=R0_out, var NF_7:bv1=NF_out, var CF_7:bv1=CF_out) :=
    call @puts_1584(ZF_in=ZF_4:bv1, VF_in=VF_4:bv1, R31_in=R31_3:bv64,
      R30_in=R30_5:bv64, R29_in=R29_3:bv64, R1_in=R1_2:bv64, R0_in=R0_22:bv64,
      NF_in=NF_4:bv1, CF_in=CF_4:bv1);
    return;
  ]
];

proc @puts_1584(ZF_in:bv1, VF_in:bv1, R31_in:bv64, R30_in:bv64, R29_in:bv64,
    R1_in:bv64, R0_in:bv64, NF_in:bv1, CF_in:bv1)
    -> (ZF_out:bv1, VF_out:bv1, R31_out:bv64, R30_out:bv64, R29_out:bv64,
    R1_out:bv64, R0_out:bv64, NF_out:bv1, CF_out:bv1)
[  ];
""")

    // call params lose their names and change order, but
    // formals and actuals remain in correspondence.
    assertResult("""prog entry @main_1876;

proc @main_1876
  (CF_in:bv1, NF_in:bv1, R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, VF_in:bv1, ZF_in:bv1)
    -> ()
  { .name = "main_1876" }
[
  block %entry [
    var (CF_7:bv1, NF_7:bv1, R0_23:bv64, R1_6:bv64, R29_6:bv64, R30_6:bv64, R31_6:bv64, VF_7:bv1, ZF_7:bv1)
        := call @puts_1584 (CF_4:bv1, NF_4:bv1, R0_22:bv64, R1_2:bv64, R29_3:bv64, R30_5:bv64, R31_3:bv64, VF_4:bv1, ZF_4:bv1);
    return ();
  ]
];


proc @puts_1584
  (CF_in:bv1, NF_in:bv1, R0_in:bv64, R1_in:bv64, R29_in:bv64, R30_in:bv64, R31_in:bv64, VF_in:bv1, ZF_in:bv1)
    -> (CF_out:bv1, NF_out:bv1, R0_out:bv64, R1_out:bv64, R29_out:bv64, R30_out:bv64, R31_out:bv64, VF_out:bv1, ZF_out:bv1)
  { .name = "puts_1584" };""".replace("\\r", "")) {
      p.program.toString.trim
    }

  }

}
