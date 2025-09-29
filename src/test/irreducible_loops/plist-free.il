memory shared $mem : (bv64 -> bv8);
memory $stack : (bv64 -> bv8);
var $CF: bv1;
var $NF: bv1;
var $R0: bv64;
var $R1: bv64;
var $R10: bv64;
var $R11: bv64;
var $R12: bv64;
var $R13: bv64;
var $R14: bv64;
var $R15: bv64;
var $R16: bv64;
var $R17: bv64;
var $R18: bv64;
var $R19: bv64;
var $R2: bv64;
var $R20: bv64;
var $R21: bv64;
var $R22: bv64;
var $R23: bv64;
var $R24: bv64;
var $R25: bv64;
var $R26: bv64;
var $R27: bv64;
var $R28: bv64;
var $R29: bv64;
var $R3: bv64;
var $R30: bv64;
var $R31: bv64;
var $R4: bv64;
var $R5: bv64;
var $R6: bv64;
var $R7: bv64;
var $R8: bv64;
var $R9: bv64;
var $V0: bv128;
var $V1: bv128;
var $V16: bv128;
var $V17: bv128;
var $V18: bv128;
var $V19: bv128;
var $V2: bv128;
var $V20: bv128;
var $V21: bv128;
var $V22: bv128;
var $V3: bv128;
var $V4: bv128;
var $V5: bv128;
var $V6: bv128;
var $V7: bv128;
var $VF: bv1;
var $ZF: bv1;

prog entry @plist_free_4253248;

proc @#free () -> ()
  { .name = "#free" };

proc @plist_free_4253248 () -> ()
  { .name = "plist_free"; .address = 0x40e640; .returnBlock = "plist_free_basil_return_1" }
[
  block %plist_free_entry {.address = 0x40e640; .originalLabel = "kHF6FaodQm66ozbOQtAxoQ=="} [
    goto(%plist_free_5, %plist_free_39);
  ];
  block %plist_free_3 {.address = 0x40e69c; .originalLabel = "nOCjep03SOmYLV8gT7i+4Q=="} [
    goto(%plist_free_basil_return_1);
  ];
  block %plist_free_5 [
    guard eq($R0:bv64, 0x0:bv64);
    $R0: bv64 := 0x0:bv64 { .label = "4253340_0" };
    goto(%plist_free_3);
  ];
  block %plist_free_7 {.address = 0x40e68c; .originalLabel = "XccktzCZS8OFPwQLbiTMBQ=="} [
    var Cse0__5_1248: bv64 := bvadd($R31:bv64, 0x10:bv64) { .label = "4253324_0" };
    var load3100: bv64 := load le $stack Cse0__5_1248:bv64 64 { .label = "4253324_1_0" };
    $R19: bv64 := load3100:bv64 { .label = "4253324_1_1" };
    var load3101: bv64 := load le $stack bvadd(Cse0__5_1248:bv64, 0x8:bv64) 64 { .label = "4253324_2_0" };
    $R20: bv64 := load3101:bv64 { .label = "4253324_2_1" };
    $R0: bv64 := 0x0:bv64 { .label = "4253328_0" };
    var load3102: bv64 := load le $stack $R31:bv64 64 { .label = "4253332_0_0" };
    $R29: bv64 := load3102:bv64 { .label = "4253332_0_1" };
    var load3103: bv64 := load le $stack bvadd($R31:bv64, 0x8:bv64) 64 { .label = "4253332_1_0" };
    $R30: bv64 := load3103:bv64 { .label = "4253332_1_1" };
    $R31: bv64 := bvadd($R31:bv64, 0x20:bv64) { .label = "4253332_2" };
    goto(%plist_free_basil_return_1);
  ];
  block %plist_free_9 [
    guard eq($R20:bv64, 0x0:bv64);
    goto(%plist_free_7);
  ];
  block %plist_free_11 [
    guard boolnot(eq($R20:bv64, 0x0:bv64));
    goto(%plist_free_23);
  ];
  block %plist_free_13 {.address = 0x40e670; .originalLabel = "EOvi9EQ8TeKKY+JazaLrww=="} [
    goto(%plist_free_9, %plist_free_11);
  ];
  block %plist_free_15 {.address = 0x40e668; .originalLabel = "vOVGQChwTQSuOaGfTF9FJw=="} [
    $R0: bv64 := $R19:bv64 { .label = "4253288_0" };
    $R30: bv64 := 0x402708:bv64 { .label = "4253292_0" };
    goto(%FUN_4024a0_entry_2093);
  ];
  block %plist_free_17 {.address = 0x40e660; .originalLabel = "Qa1LaJMHRYaM4sfCJBwH9A=="} [
    $R0: bv64 := $R1:bv64 { .label = "4253280_0" };
    $R30: bv64 := 0x402708:bv64 { .label = "4253284_0" };
    goto(%FUN_4024a0_entry_1869);
  ];
  block %plist_free_19 [
    guard boolnot(eq($R1:bv64, 0x0:bv64));
    goto(%plist_free_17);
  ];
  block %plist_free_21 [
    guard boolnot(boolnot(eq($R1:bv64, 0x0:bv64)));
    goto(%plist_free_31);
  ];
  block %plist_free_23 {.address = 0x40e674; .originalLabel = "VzsohlPDQNuRuTiH008dQg=="} [
    $R19: bv64 := $R20:bv64 { .label = "4253300_0" };
    $R0: bv64 := $R19:bv64 { .label = "4253304_0" };
    var Cse0__5_1249: bv64 := bvadd($R19:bv64, 0x8:bv64) { .label = "4253308_0" };
    var load3104: bv64 := load le $mem Cse0__5_1249:bv64 64 { .label = "4253308_1_0" };
    $R1: bv64 := load3104:bv64 { .label = "4253308_1_1" };
    var load3105: bv64 := load le $mem bvadd(Cse0__5_1249:bv64, 0x8:bv64) 64 { .label = "4253308_2_0" };
    $R20: bv64 := load3105:bv64 { .label = "4253308_2_1" };
    goto(%plist_free_19, %plist_free_21);
  ];
  block %plist_free_25 [
    guard boolnot(eq($R20:bv64, 0x0:bv64));
    goto(%plist_free_23);
  ];
  block %plist_free_27 [
    guard boolnot(boolnot(eq($R20:bv64, 0x0:bv64)));
    goto(%plist_free_7);
  ];
  block %plist_free_29 {.address = 0x40e688; .originalLabel = "M1SfjziaSfWcofXdmzs01g=="} [
    goto(%plist_free_25, %plist_free_27);
  ];
  block %plist_free_31 {.address = 0x40e684; .originalLabel = "GuuRDmJMRLuYKcHxeCpMPQ=="} [
    $R30: bv64 := 0x402704:bv64 { .label = "4253316_0" };
    goto(%FUN_4024a0_entry_1765);
  ];
  block %plist_free_33 [
    guard eq($R1:bv64, 0x0:bv64);
    goto(%plist_free_31);
  ];
  block %plist_free_35 [
    guard boolnot(eq($R1:bv64, 0x0:bv64));
    goto(%plist_free_17);
  ];
  block %plist_free_37 {.address = 0x40e644; .originalLabel = "VVIojEWDQ7imZF/Skp/KTQ=="} [
    goto(%plist_free_33, %plist_free_35);
  ];
  block %plist_free_39 [
    guard boolnot(eq($R0:bv64, 0x0:bv64));
    var Cse0__5_1245: bv64 := bvadd($R31:bv64, 0xffffffffffffffe0:bv64) { .label = "4253252_0" };
    store le $stack Cse0__5_1245:bv64 $R29:bv64 64 { .label = "4253252_1" };
    store le $stack bvadd(Cse0__5_1245:bv64, 0x8:bv64) $R30:bv64 64 { .label = "4253252_2" };
    $R31: bv64 := Cse0__5_1245:bv64 { .label = "4253252_3" };
    $R29: bv64 := $R31:bv64 { .label = "4253256_0" };
    var Cse0__5_1246: bv64 := bvadd($R31:bv64, 0x10:bv64) { .label = "4253260_0" };
    store le $stack Cse0__5_1246:bv64 $R19:bv64 64 { .label = "4253260_1" };
    store le $stack bvadd(Cse0__5_1246:bv64, 0x8:bv64) $R20:bv64 64 { .label = "4253260_2" };
    $R19: bv64 := $R0:bv64 { .label = "4253264_0" };
    $R0: bv64 := $R19:bv64 { .label = "4253268_0" };
    var Cse0__5_1247: bv64 := bvadd($R19:bv64, 0x8:bv64) { .label = "4253272_0" };
    var load3098: bv64 := load le $mem Cse0__5_1247:bv64 64 { .label = "4253272_1_0" };
    $R1: bv64 := load3098:bv64 { .label = "4253272_1_1" };
    var load3099: bv64 := load le $mem bvadd(Cse0__5_1247:bv64, 0x8:bv64) 64 { .label = "4253272_2_0" };
    $R20: bv64 := load3099:bv64 { .label = "4253272_2_1" };
    goto(%plist_free_37);
  ];
  block %_inlineret_3 {.originalLabel = "GuuRDmJMRLuYKcHxeCpMPQ=="} [
    goto(%plist_free_29);
  ];
  block %FUN_4024a0_entry_1765 {.address = 0x4024a0} [
    $R16: bv64 := 0x430000:bv64 { .label = "4203680_0" };
    var load1722: bv64 := load le $mem bvadd($R16:bv64, 0x200:bv64) 64 { .label = "4203684_0_0" };
    $R17: bv64 := load1722:bv64 { .label = "4203684_0_1" };
    $R16: bv64 := bvadd($R16:bv64, 0x200:bv64) { .label = "4203688_0" };
    call @#free () { .label = "4203692_2" };
    goto(%FUN_4024a0_basil_return_1_1766);
  ];
  block %FUN_4024a0_basil_return_1_1766 [
    goto(%_inlineret_3);
  ];
  block %_inlineret_4 {.originalLabel = "Qa1LaJMHRYaM4sfCJBwH9A=="} [
    goto(%plist_free_15);
  ];
  block %FUN_4024a0_entry_1869 {.address = 0x4024a0} [
    $R16: bv64 := 0x430000:bv64 { .label = "4203680_0" };
    var load1722: bv64 := load le $mem bvadd($R16:bv64, 0x200:bv64) 64 { .label = "4203684_0_0" };
    $R17: bv64 := load1722:bv64 { .label = "4203684_0_1" };
    $R16: bv64 := bvadd($R16:bv64, 0x200:bv64) { .label = "4203688_0" };
    call @#free () { .label = "4203692_2" };
    goto(%FUN_4024a0_basil_return_1_1870);
  ];
  block %FUN_4024a0_basil_return_1_1870 [
    goto(%_inlineret_4);
  ];
  block %_inlineret_5 {.originalLabel = "vOVGQChwTQSuOaGfTF9FJw=="} [
    goto(%plist_free_13);
  ];
  block %FUN_4024a0_entry_2093 {.address = 0x4024a0} [
    $R16: bv64 := 0x430000:bv64 { .label = "4203680_0" };
    var load1722: bv64 := load le $mem bvadd($R16:bv64, 0x200:bv64) 64 { .label = "4203684_0_0" };
    $R17: bv64 := load1722:bv64 { .label = "4203684_0_1" };
    $R16: bv64 := bvadd($R16:bv64, 0x200:bv64) { .label = "4203688_0" };
    call @#free () { .label = "4203692_2" };
    goto(%FUN_4024a0_basil_return_1_2094);
  ];
  block %FUN_4024a0_basil_return_1_2094 [
    goto(%_inlineret_5);
  ];
  block %plist_free_basil_return_1 [
    return ();
  ]
];

