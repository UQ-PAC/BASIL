var stack: [bv64]bv8;
function {:bvbuiltin "bvand"} bvand64(bv64, bv64) returns (bv64);
function {:bvbuiltin "bvadd"} bvadd64(bv64, bv64) returns (bv64);
function {:bvbuiltin "sign_extend 32"} sign_extend32_32(bv32) returns (bv64);
function {:bvbuiltin "bvor"} bvor64(bv64, bv64) returns (bv64);
function {:bvbuiltin "bvand"} bvand1(bv1, bv1) returns (bv1);
function {:bvbuiltin "bvcomp"} bvcomp1(bv1, bv1) returns (bv1);
function {:bvbuiltin "bvcomp"} bvcomp64(bv64, bv64) returns (bv1);
function {:bvbuiltin "bvor"} bvor1(bv1, bv1) returns (bv1);
function {:bvbuiltin "zero_extend 32"} zero_extend32_32(bv32) returns (bv64);
function {:bvbuiltin "bvsub"} bvsub64(bv64, bv64) returns (bv64);
function {:bvbuiltin "bvnot"} bvnot1(bv1) returns (bv1);
procedure main(main_argc: bv32, main_argv: bv64, FP: bv64, LR: bv64, SP: bv64) returns (main_argv_out: bv64, main_result: bv32, FP_out: bv64, LR_out: bv64, SP_out: bv64)
  modifies stack;
{
  var R0: bv64;
  var R1: bv64;
  var R29: bv64;
  var R30: bv64;
  var R31: bv64;
  var #31: bv64;
  var #32: bv64;
  var NF: bv1;
  var CF: bv1;
  var #34: bv1;
  var ZF: bv1;
  var VF: bv1;
  var #temp: bv32;
  R0 := zero_extend32_32(main_argc);
  R1 := main_argv;
  R29 := FP;
  R30 := LR;
  R31 := SP;
  lmain:
    R31 := bvsub64(R31, 16bv64);
    #31 := zero_extend32_32(stack[bvadd64(bvadd64(R31, 12bv64), 3bv64)] ++ stack[bvadd64(bvadd64(R31, 12bv64), 2bv64)] ++ stack[bvadd64(bvadd64(R31, 12bv64), 1bv64)] ++ stack[bvadd64(R31, 12bv64)]);
    R0 := 0bv64;
    R0 := bvor64(bvand64(R0, 18446744069414584320bv64), #31);
    #32 := sign_extend32_32(R0[32:0]);
    NF := #32[64:63];
    VF := bvand1(R0[32:31], bvnot1(#32[64:63]));
    ZF := bvcomp64(#32, 0bv64);
    CF := bvor1(bvor1(R0[32:31], bvand1(R0[32:31], bvnot1(#32[64:63]))), bvnot1(#32[64:63]));
    #34 := bvnot1(bvcomp1(bvnot1(ZF), 0bv1));
    if (#34 != 0bv1) {
      goto l0000012b;
    }
    goto l000001ff;
  l000001ff:
    R0 := 0bv64;
    R0 := bvor64(bvand64(R0, 18446744069414584320bv64), 1bv64);
    #temp := R0[32:0];
    stack[bvadd64(R31, 12bv64)] := #temp[8:0];
    stack[bvadd64(bvadd64(R31, 12bv64), 1bv64)] := #temp[16:8];
    stack[bvadd64(bvadd64(R31, 12bv64), 2bv64)] := #temp[24:16];
    stack[bvadd64(bvadd64(R31, 12bv64), 3bv64)] := #temp[32:24];
    goto l0000012b;
  l0000012b:
    R0 := 0bv64;
    R0 := bvand64(R0, 18446744069414584320bv64);
    R31 := bvadd64(R31, 16bv64);
    main_argv_out := R1;
    main_result := R0[32:0];
    FP_out := R29;
    LR_out := R30;
    SP_out := R31;
    return;
}
