var Gamma_R0: bool;
var Gamma_R1: bool;
var Gamma_R29: bool;
var Gamma_R30: bool;
var Gamma_R31: bool;
var Gamma_mem: [bv64]bool;
var Gamma_stack: [bv64]bool;
var R0: bv64;
var R1: bv64;
var R29: bv64;
var R30: bv64;
var R31: bv64;
var mem: [bv64]bv8;
var stack: [bv64]bv8;
function L(memory: [bv64]bv8, index: bv64) returns (bool) {
  false
}

function {:bvbuiltin "bvadd"} bvadd64(bv64, bv64) returns (bv64);
function gamma_load64(gammaMap: [bv64]bool, index: bv64) returns (bool) {
  (gammaMap[bvadd64(index, 7bv64)] && (gammaMap[bvadd64(index, 6bv64)] && (gammaMap[bvadd64(index, 5bv64)] && (gammaMap[bvadd64(index, 4bv64)] && (gammaMap[bvadd64(index, 3bv64)] && (gammaMap[bvadd64(index, 2bv64)] && (gammaMap[bvadd64(index, 1bv64)] && gammaMap[index])))))))
}

function gamma_store64(gammaMap: [bv64]bool, index: bv64, value: bool) returns ([bv64]bool) {
  gammaMap[index := value][bvadd64(index, 1bv64) := value][bvadd64(index, 2bv64) := value][bvadd64(index, 3bv64) := value][bvadd64(index, 4bv64) := value][bvadd64(index, 5bv64) := value][bvadd64(index, 6bv64) := value][bvadd64(index, 7bv64) := value]
}

function memory_load64_le(memory: [bv64]bv8, index: bv64) returns (bv64) {
  (memory[bvadd64(index, 7bv64)] ++ (memory[bvadd64(index, 6bv64)] ++ (memory[bvadd64(index, 5bv64)] ++ (memory[bvadd64(index, 4bv64)] ++ (memory[bvadd64(index, 3bv64)] ++ (memory[bvadd64(index, 2bv64)] ++ (memory[bvadd64(index, 1bv64)] ++ memory[index])))))))
}

function memory_load8_le(memory: [bv64]bv8, index: bv64) returns (bv8) {
  memory[index]
}

function memory_store64_le(memory: [bv64]bv8, index: bv64, value: bv64) returns ([bv64]bv8) {
  memory[index := value[8:0]][bvadd64(index, 1bv64) := value[16:8]][bvadd64(index, 2bv64) := value[24:16]][bvadd64(index, 3bv64) := value[32:24]][bvadd64(index, 4bv64) := value[40:32]][bvadd64(index, 5bv64) := value[48:40]][bvadd64(index, 6bv64) := value[56:48]][bvadd64(index, 7bv64) := value[64:56]]
}

procedure rely();
  modifies mem, Gamma_mem;
  ensures (forall i: bv64 :: (((mem[i] == old(mem[i])) ==> (Gamma_mem[i] == old(Gamma_mem[i])))));
  ensures (mem == old(mem));
  ensures (Gamma_mem == old(Gamma_mem));

procedure rely_transitive()
  modifies mem, Gamma_mem;
  ensures (mem == old(mem));
  ensures (Gamma_mem == old(Gamma_mem));
{
  call rely();
  call rely();
}

procedure rely_reflexive();

procedure guarantee_reflexive();
  modifies mem, Gamma_mem;

procedure #free();

procedure get_secret()
  modifies Gamma_R0, Gamma_R29, Gamma_R30, Gamma_R31, Gamma_stack, R0, R29, R30, R31, stack;
  free ensures (Gamma_R29 == old(Gamma_R29));
  free ensures (Gamma_R31 == old(Gamma_R31));
  free ensures (R29 == old(R29));
  free ensures (R31 == old(R31));
{
  var #4: bv64;
  var Gamma_#4: bool;
  lget_secret:
    #4, Gamma_#4 := bvadd64(R31, 18446744073709551600bv64), Gamma_R31;
    stack, Gamma_stack := memory_store64_le(stack, #4, R29), gamma_store64(Gamma_stack, #4, Gamma_R29);
    stack, Gamma_stack := memory_store64_le(stack, bvadd64(#4, 8bv64), R30), gamma_store64(Gamma_stack, bvadd64(#4, 8bv64), Gamma_R30);
    R31, Gamma_R31 := #4, Gamma_#4;
    R29, Gamma_R29 := R31, Gamma_R31;
    R0, Gamma_R0 := 10bv64, true;
    R30, Gamma_R30 := 2020bv64, true;
    call malloc();
    goto l00000323;
  l00000323:
    R29, Gamma_R29 := memory_load64_le(mem, R31), (gamma_load64(Gamma_mem, R31) || L(mem, R31));
    R30, Gamma_R30 := memory_load64_le(mem, bvadd64(R31, 8bv64)), (gamma_load64(Gamma_mem, bvadd64(R31, 8bv64)) || L(mem, bvadd64(R31, 8bv64)));
    R31, Gamma_R31 := bvadd64(R31, 16bv64), Gamma_R31;
    return;
}

procedure main()
  modifies Gamma_R0, Gamma_R1, Gamma_R29, Gamma_R30, Gamma_R31, Gamma_mem, Gamma_stack, R0, R1, R29, R30, R31, mem, stack;
  free requires (memory_load8_le(mem, 2104bv64) == 1bv8);
  free requires (memory_load8_le(mem, 2105bv64) == 0bv8);
  free requires (memory_load8_le(mem, 2106bv64) == 2bv8);
  free requires (memory_load8_le(mem, 2107bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131000bv64) == 216bv8);
  free requires (memory_load8_le(mem, 131001bv64) == 253bv8);
  free requires (memory_load8_le(mem, 131002bv64) == 1bv8);
  free requires (memory_load8_le(mem, 131003bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131004bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131005bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131006bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131007bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131008bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131009bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131010bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131011bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131012bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131013bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131014bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131015bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131016bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131017bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131018bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131019bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131020bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131021bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131022bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131023bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131024bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131025bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131026bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131027bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131028bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131029bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131030bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131031bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131032bv64) == 236bv8);
  free requires (memory_load8_le(mem, 131033bv64) == 7bv8);
  free requires (memory_load8_le(mem, 131034bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131035bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131036bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131037bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131038bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131039bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131040bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131041bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131042bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131043bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131044bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131045bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131046bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131047bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131120bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131121bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131122bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131123bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131124bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131125bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131126bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131127bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131128bv64) == 56bv8);
  free requires (memory_load8_le(mem, 131129bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131130bv64) == 2bv8);
  free requires (memory_load8_le(mem, 131131bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131132bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131133bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131134bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131135bv64) == 0bv8);
  free ensures (Gamma_R29 == old(Gamma_R29));
  free ensures (Gamma_R31 == old(Gamma_R31));
  free ensures (R29 == old(R29));
  free ensures (R31 == old(R31));
{
  var #5: bv64;
  var Gamma_#5: bool;
  lmain:
    #5, Gamma_#5 := bvadd64(R31, 18446744073709551600bv64), Gamma_R31;
    stack, Gamma_stack := memory_store64_le(stack, #5, R29), gamma_store64(Gamma_stack, #5, Gamma_R29);
    stack, Gamma_stack := memory_store64_le(stack, bvadd64(#5, 8bv64), R30), gamma_store64(Gamma_stack, bvadd64(#5, 8bv64), Gamma_R30);
    R31, Gamma_R31 := #5, Gamma_#5;
    R29, Gamma_R29 := R31, Gamma_R31;
    R30, Gamma_R30 := 2040bv64, true;
    call get_secret();
    goto l0000035b;
  l0000035b:
    R1, Gamma_R1 := R0, Gamma_R0;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 72bv64), Gamma_R0;
    call rely();
    assert (L(mem, R0) ==> Gamma_R1);
    mem, Gamma_mem := memory_store64_le(mem, R0, R1), gamma_store64(Gamma_mem, R0, Gamma_R1);
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 72bv64), Gamma_R0;
    R0, Gamma_R0 := memory_load64_le(mem, R0), (gamma_load64(Gamma_mem, R0) || L(mem, R0));
    R30, Gamma_R30 := 2072bv64, true;
    call #free();
    goto l0000038e;
  l0000038e:
    R0, Gamma_R0 := 0bv64, true;
    R29, Gamma_R29 := memory_load64_le(mem, R31), (gamma_load64(Gamma_mem, R31) || L(mem, R31));
    R30, Gamma_R30 := memory_load64_le(mem, bvadd64(R31, 8bv64)), (gamma_load64(Gamma_mem, bvadd64(R31, 8bv64)) || L(mem, bvadd64(R31, 8bv64)));
    R31, Gamma_R31 := bvadd64(R31, 16bv64), Gamma_R31;
    return;
}

procedure malloc();
