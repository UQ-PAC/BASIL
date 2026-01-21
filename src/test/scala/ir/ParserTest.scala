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
    ParseBasilIL.loadILString("""
var $R29:bv64;
var $ZF:bv1;
var $R1:bv64;
memory $stack:(bv64->bv8);
var $R31:bv64;
var $R0:bv64;
var $VF:bv1;
memory $mem:(bv64->bv8);
var $CF:bv1;
var $NF:bv1;
var $R30:bv64;
prog entry @main_1876;
proc @main_1876(ZF_in:bv1, VF_in:bv1, R31_in:bv64, R30_in:bv64, R29_in:bv64,
   R1_in:bv64, R0_in:bv64, NF_in:bv1, CF_in:bv1)
   -> (ZF_out:bv1, VF_out:bv1, R31_out:bv64, R30_out:bv64, R29_out:bv64,
   R1_out:bv64, R0_out:bv64, NF_out:bv1, CF_out:bv1)
[
   block %inputs [
      (var R30:bv64 := R30_in:bv64, var NF:bv1 := NF_in:bv1,
       var CF:bv1 := CF_in:bv1, var VF:bv1 := VF_in:bv1, var R0:bv64 := R0_in:bv64,
       var R31:bv64 := R31_in:bv64, var R1:bv64 := R1_in:bv64,
       var ZF:bv1 := ZF_in:bv1, var R29:bv64 := R29_in:bv64);
      goto (%main_entry);
      ];
   block %main_entry [
      var #4_1:bv64 := bvadd(R31:bv64, 0xffffffffffffffe0:bv64);
      var R31_1:bv64 := #4_1:bv64;
      var R29_1:bv64 := R31_1:bv64;
      var R0_1:bv64 := 0x20000:bv64;
      var R0_2:bv64 := bvadd(R0_1:bv64, 0x3c:bv64);
      var R0_3:bv64 := 0x20000:bv64;
      var R0_4:bv64 := bvadd(R0_3:bv64, 0x40:bv64);
      var load18_1:bv32 := load le $mem:(bv64->bv8) R0_4:bv64 32;
      var R0_5:bv64 := zero_extend(32, load18_1:bv32);
      var R0_6:bv64 := zero_extend(32,
      bvconcat(0x0:bv31, extract(1,0, R0_5:bv64)));
      var #5_1:bv32 := bvadd(extract(32,0, R0_6:bv64), 0xffffffff:bv32);
      var VF_1:bv1 := bvnot(booltobv1(eq(sign_extend(1,
         bvadd(#5_1:bv32, 0x1:bv32)), sign_extend(1, extract(32,0, R0_6:bv64)))));
      var CF_1:bv1 := bvnot(booltobv1(eq(zero_extend(1,
         bvadd(#5_1:bv32, 0x1:bv32)),
         bvadd(zero_extend(1, extract(32,0, R0_6:bv64)), 0x100000000:bv33))));
      var ZF_1:bv1 := booltobv1(eq(bvadd(#5_1:bv32, 0x1:bv32), 0x0:bv32));
      var NF_1:bv1 := extract(32,31, bvadd(#5_1:bv32, 0x1:bv32));
      goto (%main_27,%main_23);
      ];
   block %main_23 [
      guard neq(booltobv1(eq(ZF_1:bv1, 0x1:bv1)), 0x0:bv1);
      goto (%main_21);
      ];
   block %main_27 [
      guard eq(booltobv1(eq(ZF_1:bv1, 0x1:bv1)), 0x0:bv1);
      goto (%main_25);
      ];
   block %main_21 [ goto (%main_19); ];
   block %main_25 [ goto (%main_5); ];
   block %main_3 [
      var R0_16:bv64 := 0x20000:bv64;
      var R0_17:bv64 := bvadd(R0_16:bv64, 0x3c:bv64);
      var load19_1:bv32 := load le $mem:(bv64->bv8) R0_17:bv64 32;
      var R0_18:bv64 := zero_extend(32, load19_1:bv32);
      var R1_5:bv64 := zero_extend(32, bvadd(extract(32,0, R0_18:bv64), 0x1:bv32));
      var R0_19:bv64 := 0x20000:bv64;
      var R0_20:bv64 := bvadd(R0_19:bv64, 0x3c:bv64);
      goto (%main_19);
      ];
   block %main_5 [
      (var CF_5:bv1 := phi(%main_25 -> CF_1:bv1, %main_7 -> CF_4:bv1),
       var NF_5:bv1 := phi(%main_25 -> NF_1:bv1, %main_7 -> NF_4:bv1),
       var R1_3:bv64 := phi(%main_25 -> R1:bv64, %main_7 -> R1_2:bv64),
       var R29_4:bv64 := phi(%main_25 -> R29_1:bv64, %main_7 -> R29_3:bv64),
       var R31_4:bv64 := phi(%main_25 -> R31_1:bv64, %main_7 -> R31_3:bv64),
       var VF_5:bv1 := phi(%main_25 -> VF_1:bv1, %main_7 -> VF_4:bv1),
       var ZF_5:bv1 := phi(%main_25 -> ZF_1:bv1, %main_7 -> ZF_4:bv1));
      var R0_13:bv64 := 0x0:bv64;
      var R0_14:bv64 := bvadd(R0_13:bv64, 0x820:bv64);
      var R30_3:bv64 := 0x7a0:bv64;
      (var ZF_6:bv1=ZF_out, var VF_6:bv1=VF_out, var R31_5:bv64=R31_out,
         var R30_4:bv64=R30_out, var R29_5:bv64=R29_out, var R1_4:bv64=R1_out,
         var R0_15:bv64=R0_out, var NF_6:bv1=NF_out, var CF_6:bv1=CF_out) :=
      call @puts_1584(ZF_in=ZF_5:bv1, VF_in=VF_5:bv1, R31_in=R31_4:bv64,
         R30_in=R30_3:bv64, R29_in=R29_4:bv64, R1_in=R1_3:bv64, R0_in=R0_14:bv64,
         NF_in=NF_5:bv1, CF_in=CF_5:bv1);
      goto (%main_3);
      ];
   block %main_7 [ goto (%main_5); ];
   block %main_9 [
      guard neq(bvnot(booltobv1(eq(ZF_4:bv1, 0x1:bv1))), 0x0:bv1);
      goto (%main_7);
      ];
   block %main_17 [
      var R0_10:bv64 := 0x20000:bv64;
      var R0_11:bv64 := bvadd(R0_10:bv64, 0x3c:bv64);
      var load20_1:bv32 := load le $mem:(bv64->bv8) R0_11:bv64 32;
      var R0_12:bv64 := zero_extend(32, load20_1:bv32);
      var #6_1:bv32 := bvadd(extract(32,0, R0_12:bv64), 0xfffffffa:bv32);
      var VF_4:bv1 := bvnot(booltobv1(eq(sign_extend(1,
         bvadd(#6_1:bv32, 0x1:bv32)),
         bvadd(sign_extend(1, extract(32,0, R0_12:bv64)), 0x1fffffffb:bv33))));
      var CF_4:bv1 := bvnot(booltobv1(eq(zero_extend(1,
         bvadd(#6_1:bv32, 0x1:bv32)),
         bvadd(zero_extend(1, extract(32,0, R0_12:bv64)), 0xfffffffb:bv33))));
      var ZF_4:bv1 := booltobv1(eq(bvadd(#6_1:bv32, 0x1:bv32), 0x0:bv32));
      var NF_4:bv1 := extract(32,31, bvadd(#6_1:bv32, 0x1:bv32));
      goto (%main_15,%main_9);
      ];
   block %main_19 [
      (var CF_2:bv1 := phi(%main_3 -> CF_6:bv1, %main_21 -> CF_1:bv1),
       var NF_2:bv1 := phi(%main_3 -> NF_6:bv1, %main_21 -> NF_1:bv1),
       var R1_1:bv64 := phi(%main_3 -> R1_5:bv64, %main_21 -> R1:bv64),
       var R29_2:bv64 := phi(%main_3 -> R29_5:bv64, %main_21 -> R29_1:bv64),
       var R31_2:bv64 := phi(%main_3 -> R31_5:bv64, %main_21 -> R31_1:bv64),
       var VF_2:bv1 := phi(%main_3 -> VF_6:bv1, %main_21 -> VF_1:bv1),
       var ZF_2:bv1 := phi(%main_3 -> ZF_6:bv1, %main_21 -> ZF_1:bv1));
      var R0_7:bv64 := 0x0:bv64;
      var R0_8:bv64 := bvadd(R0_7:bv64, 0x820:bv64);
      var R30_1:bv64 := 0x7d0:bv64;
      (var ZF_3:bv1=ZF_out, var VF_3:bv1=VF_out, var R31_3:bv64=R31_out,
         var R30_2:bv64=R30_out, var R29_3:bv64=R29_out, var R1_2:bv64=R1_out,
         var R0_9:bv64=R0_out, var NF_3:bv1=NF_out, var CF_3:bv1=CF_out) :=
      call @puts_1584(ZF_in=ZF_2:bv1, VF_in=VF_2:bv1, R31_in=R31_2:bv64,
         R30_in=R30_1:bv64, R29_in=R29_2:bv64, R1_in=R1_1:bv64, R0_in=R0_8:bv64,
         NF_in=NF_2:bv1, CF_in=CF_2:bv1);
      goto (%main_17);
      ];
   block %main_15 [
      guard eq(bvnot(booltobv1(eq(ZF_4:bv1, 0x1:bv1))), 0x0:bv1);
      var R0_21:bv64 := 0x0:bv64;
      var R0_22:bv64 := bvadd(R0_21:bv64, 0x828:bv64);
      var R30_5:bv64 := 0x7f4:bv64;
      (var ZF_7:bv1=ZF_out, var VF_7:bv1=VF_out, var R31_6:bv64=R31_out,
         var R30_6:bv64=R30_out, var R29_6:bv64=R29_out, var R1_6:bv64=R1_out,
         var R0_23:bv64=R0_out, var NF_7:bv1=NF_out, var CF_7:bv1=CF_out) :=
      call @puts_1584(ZF_in=ZF_4:bv1, VF_in=VF_4:bv1, R31_in=R31_3:bv64,
         R30_in=R30_5:bv64, R29_in=R29_3:bv64, R1_in=R1_2:bv64, R0_in=R0_22:bv64,
         NF_in=NF_4:bv1, CF_in=CF_4:bv1);
      goto (%main_13);
      ];
   block %main_13 [ goto (%main_11); ];
   block %main_11 [
      var R0_24:bv64 := 0x0:bv64;
      var load21_1:bv64 := load le $stack:(bv64->bv8) R31_6:bv64 64;
      var R29_7:bv64 := load21_1:bv64;
      var load22_1:bv64 := load le $stack:(bv64->bv8) bvadd(R31_6:bv64, 0x8:bv64) 64;
      var R30_7:bv64 := load22_1:bv64;
      var R31_7:bv64 := bvadd(R31_6:bv64, 0x20:bv64);
      goto (%main_basil_return_1);
      ];
   block %main_basil_return_1 [ goto (%returns); ];
   block %returns [
      (var R30_out:bv64 := R30_7:bv64, var NF_out:bv1 := NF_7:bv1,
       var CF_out:bv1 := CF_7:bv1, var VF_out:bv1 := VF_7:bv1,
       var R0_out:bv64 := R0_24:bv64, var R31_out:bv64 := R31_7:bv64,
       var R1_out:bv64 := R1_6:bv64, var ZF_out:bv1 := ZF_7:bv1,
       var R29_out:bv64 := R29_7:bv64);
      return;
      ]
];
proc @puts_1584(ZF_in:bv1, VF_in:bv1, R31_in:bv64, R30_in:bv64, R29_in:bv64,
   R1_in:bv64, R0_in:bv64, NF_in:bv1, CF_in:bv1)
   -> (ZF_out:bv1, VF_out:bv1, R31_out:bv64, R30_out:bv64, R29_out:bv64,
   R1_out:bv64, R0_out:bv64, NF_out:bv1, CF_out:bv1)
[  ];
    """);
  }

}
