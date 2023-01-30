var mem: [bv64]bv8;
var Gamma_mem: [bv64]bool;
const $z_addr: bv64;
axiom ($z_addr == 69652bv64);
const $x_addr: bv64;
axiom ($x_addr == 69656bv64);
const $_IO_stdin_used_addr: bv64;
axiom ($_IO_stdin_used_addr == 2084bv64);
const $secret_addr: bv64;
axiom ($secret_addr == 69660bv64);
function gamma_load32(gammaMap: [bv64]bool, index: bv64) returns (bool) {
  (gammaMap[bvadd64(index, 3bv64)] && (gammaMap[bvadd64(index, 2bv64)] && (gammaMap[bvadd64(index, 1bv64)] && gammaMap[index])))
}

function {:bvbuiltin "bvand"} bvand64(bv64, bv64) returns (bv64);
function gamma_store32(gammaMap: [bv64]bool, index: bv64, value: bool) returns ([bv64]bool) {
  gammaMap[index := value][bvadd64(index, 1bv64) := value][bvadd64(index, 2bv64) := value][bvadd64(index, 3bv64) := value]
}

function L(memory: [bv64]bv8, index: bv64) returns (bool) {
  (if (index == $secret_addr) then false else (if (index == $x_addr) then (bvsmod32(memory_load32_le(memory, $z_addr), 2bv32) == 0bv32) else (if (index == $z_addr) then true else false)))
}

function memory_store32_le(memory: [bv64]bv8, index: bv64, value: bv32) returns ([bv64]bv8) {
  memory[index := value[8:0]][bvadd64(index, 1bv64) := value[16:8]][bvadd64(index, 2bv64) := value[24:16]][bvadd64(index, 3bv64) := value[32:24]]
}

function {:bvbuiltin "sign_extend 32"} sign_extend32_32(bv32) returns (bv64);
function {:bvbuiltin "bvsge"} bvsge32(bv32, bv32) returns (bool);
function memory_load32_le(memory: [bv64]bv8, index: bv64) returns (bv32) {
  (memory[bvadd64(index, 3bv64)] ++ (memory[bvadd64(index, 2bv64)] ++ (memory[bvadd64(index, 1bv64)] ++ memory[index])))
}

function {:bvbuiltin "zero_extend 32"} zero_extend32_32(bv32) returns (bv64);
function {:bvbuiltin "bvor"} bvor64(bv64, bv64) returns (bv64);
function {:bvbuiltin "bvadd"} bvadd64(bv64, bv64) returns (bv64);
function {:bvbuiltin "bvsmod"} bvsmod32(bv32, bv32) returns (bv32);
procedure rely();
  modifies mem, Gamma_mem;
  ensures (forall i: bv64 :: (((mem[i] == old(mem[i])) ==> (Gamma_mem[i] == old(Gamma_mem[i])))));
  ensures (memory_load32_le(mem, $z_addr) == old(memory_load32_le(mem, $z_addr)));
  ensures (old(gamma_load32(Gamma_mem, $x_addr)) ==> gamma_load32(Gamma_mem, $x_addr));

procedure rely_transitive()
  modifies mem, Gamma_mem;
  ensures (memory_load32_le(mem, $z_addr) == old(memory_load32_le(mem, $z_addr)));
  ensures (old(gamma_load32(Gamma_mem, $x_addr)) ==> gamma_load32(Gamma_mem, $x_addr));
{
  call rely();
  call rely();
}

procedure rely_reflexive()
{
  assert (memory_load32_le(mem, $z_addr) == memory_load32_le(mem, $z_addr));
  assert (gamma_load32(Gamma_mem, $x_addr) ==> gamma_load32(Gamma_mem, $x_addr));
}

procedure guarantee_reflexive()
{
  assert bvsge32(memory_load32_le(mem, $z_addr), memory_load32_le(mem, $z_addr));
}

procedure main(main_argc: bv32, Gamma_main_argc: bool, main_argv: bv64, Gamma_main_argv: bool, FP: bv64, Gamma_FP: bool, LR: bv64, Gamma_LR: bool, SP: bv64, Gamma_SP: bool) returns (main_argv_out: bv64, Gamma_main_argv_out: bool, main_result: bv32, Gamma_main_result: bool, FP_out: bv64, Gamma_FP_out: bool, LR_out: bv64, Gamma_LR_out: bool, SP_out: bv64, Gamma_SP_out: bool)
  modifies mem, Gamma_mem;
  requires (gamma_load32(Gamma_mem, $x_addr) == true);
  requires (gamma_load32(Gamma_mem, $z_addr) == true);
  requires (gamma_load32(Gamma_mem, $secret_addr) == false);
  requires (memory_load32_le(mem, $z_addr) == 0bv32);
{
  var R0: bv64;
  var Gamma_R0: bool;
  var R1: bv64;
  var Gamma_R1: bool;
  var R29: bv64;
  var Gamma_R29: bool;
  var R30: bv64;
  var Gamma_R30: bool;
  var R31: bv64;
  var Gamma_R31: bool;
  var #35: bv64;
  var Gamma_#32: bool;
  var z_old: bv32;
  var #32: bv64;
  var Gamma_#33: bool;
  var #33: bv64;
  var #38: bv64;
  var Gamma_#35: bool;
  var Gamma_x_old: bool;
  var Gamma_#39: bool;
  var #39: bv64;
  var Gamma_#38: bool;
  R0, Gamma_R0 := zero_extend32_32(main_argc), Gamma_main_argc;
  R1, Gamma_R1 := main_argv, Gamma_main_argv;
  R29, Gamma_R29 := FP, Gamma_FP;
  R30, Gamma_R30 := LR, Gamma_LR;
  R31, Gamma_R31 := SP, Gamma_SP;
  lmain:
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 20bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> true);
    z_old := memory_load32_le(mem, $z_addr);
    Gamma_x_old := (gamma_load32(Gamma_mem, $x_addr) || L(mem, $x_addr));
    mem, Gamma_mem := memory_store32_le(mem, R0, 0bv32), gamma_store32(Gamma_mem, R0, true);
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 20bv64), Gamma_R0;
    call rely();
    #32, Gamma_#32 := zero_extend32_32(memory_load32_le(mem, R0)), (gamma_load32(Gamma_mem, R0) || L(mem, R0));
    R0, Gamma_R0 := 0bv64, true;
    R0, Gamma_R0 := bvor64(bvand64(R0, 18446744069414584320bv64), #32), (Gamma_#32 && Gamma_R0);
    #33, Gamma_#33 := bvadd64(sign_extend32_32(R0[32:0]), 1bv64), Gamma_R0;
    R1, Gamma_R1 := 0bv64, true;
    R1, Gamma_R1 := bvor64(bvand64(R1, 18446744069414584320bv64), #33), (Gamma_#33 && Gamma_R1);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 20bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> Gamma_R1);
    z_old := memory_load32_le(mem, $z_addr);
    Gamma_x_old := (gamma_load32(Gamma_mem, $x_addr) || L(mem, $x_addr));
    mem, Gamma_mem := memory_store32_le(mem, R0, R1[32:0]), gamma_store32(Gamma_mem, R0, Gamma_R1);
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 28bv64), Gamma_R0;
    call rely();
    #35, Gamma_#35 := zero_extend32_32(memory_load32_le(mem, R0)), (gamma_load32(Gamma_mem, R0) || L(mem, R0));
    R1, Gamma_R1 := 0bv64, true;
    R1, Gamma_R1 := bvor64(bvand64(R1, 18446744069414584320bv64), #35), (Gamma_#35 && Gamma_R1);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 24bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> Gamma_R1);
    z_old := memory_load32_le(mem, $z_addr);
    Gamma_x_old := (gamma_load32(Gamma_mem, $x_addr) || L(mem, $x_addr));
    mem, Gamma_mem := memory_store32_le(mem, R0, R1[32:0]), gamma_store32(Gamma_mem, R0, Gamma_R1);
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 24bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> true);
    z_old := memory_load32_le(mem, $z_addr);
    Gamma_x_old := (gamma_load32(Gamma_mem, $x_addr) || L(mem, $x_addr));
    mem, Gamma_mem := memory_store32_le(mem, R0, 0bv32), gamma_store32(Gamma_mem, R0, true);
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 20bv64), Gamma_R0;
    call rely();
    #38, Gamma_#38 := zero_extend32_32(memory_load32_le(mem, R0)), (gamma_load32(Gamma_mem, R0) || L(mem, R0));
    R0, Gamma_R0 := 0bv64, true;
    R0, Gamma_R0 := bvor64(bvand64(R0, 18446744069414584320bv64), #38), (Gamma_#38 && Gamma_R0);
    #39, Gamma_#39 := bvadd64(sign_extend32_32(R0[32:0]), 1bv64), Gamma_R0;
    R1, Gamma_R1 := 0bv64, true;
    R1, Gamma_R1 := bvor64(bvand64(R1, 18446744069414584320bv64), #39), (Gamma_#39 && Gamma_R1);
    R0, Gamma_R0 := 69632bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 20bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> Gamma_R1);
    z_old := memory_load32_le(mem, $z_addr);
    Gamma_x_old := (gamma_load32(Gamma_mem, $x_addr) || L(mem, $x_addr));
    mem, Gamma_mem := memory_store32_le(mem, R0, R1[32:0]), gamma_store32(Gamma_mem, R0, Gamma_R1);
    assert ((R0 == $z_addr) ==> (L(mem, $x_addr) ==> Gamma_x_old));
    assert bvsge32(memory_load32_le(mem, $z_addr), z_old);
    R0, Gamma_R0 := 0bv64, true;
    R0, Gamma_R0 := bvand64(R0, 18446744069414584320bv64), Gamma_R0;
    main_argv_out, Gamma_main_argv_out := R1, Gamma_R1;
    main_result, Gamma_main_result := R0[32:0], Gamma_R0;
    FP_out, Gamma_FP_out := R29, Gamma_R29;
    LR_out, Gamma_LR_out := R30, Gamma_R30;
    SP_out, Gamma_SP_out := R31, Gamma_R31;
    return;
}
