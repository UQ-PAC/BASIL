var {:extern} Gamma_R0: bool;
var {:extern} Gamma_R1: bool;
var {:extern} Gamma_R16: bool;
var {:extern} Gamma_R17: bool;
var {:extern} Gamma_R2: bool;
var {:extern} Gamma_R29: bool;
var {:extern} Gamma_R30: bool;
var {:extern} Gamma_R31: bool;
var {:extern} Gamma_mem: [bv64]bool;
var {:extern} Gamma_mem_locked: [bv64]bool;
var {:extern} Gamma_stack: [bv64]bool;
var {:extern} R0: bv64;
var {:extern} R1: bv64;
var {:extern} R16: bv64;
var {:extern} R17: bv64;
var {:extern} R2: bv64;
var {:extern} R29: bv64;
var {:extern} R30: bv64;
var {:extern} R31: bv64;
var {:extern} mem: [bv64]bv8;
var {:extern} mem_locked: [bv64]bv8;
var {:extern} stack: [bv64]bv8;
const {:extern} $copiedtext_addr: bv64;
axiom ($copiedtext_addr == 131152bv64);
const {:extern} $text_addr: bv64;
axiom ($text_addr == 131128bv64);
function {:extern} L(memory: [bv64]bv8, index: bv64) returns (bool) {
  false
}

function {:extern} {:bvbuiltin "bvadd"} bvadd32(bv32, bv32) returns (bv32);
function {:extern} {:bvbuiltin "bvadd"} bvadd33(bv33, bv33) returns (bv33);
function {:extern} {:bvbuiltin "bvadd"} bvadd64(bv64, bv64) returns (bv64);
function {:extern} {:bvbuiltin "bvcomp"} bvcomp1(bv1, bv1) returns (bv1);
function {:extern} {:bvbuiltin "bvcomp"} bvcomp32(bv32, bv32) returns (bv1);
function {:extern} {:bvbuiltin "bvcomp"} bvcomp33(bv33, bv33) returns (bv1);
function {:extern} {:bvbuiltin "bvnot"} bvnot1(bv1) returns (bv1);
function {:extern} {:bvbuiltin "bvnot"} bvnot32(bv32) returns (bv32);
function {:extern} {:bvbuiltin "bvsub"} bvsub64(bv64, bv64) returns (bv64);
function {:extern} {:bvbuiltin "bvuge"} bvuge64(bv64, bv64) returns (bool);
function {:extern} {:bvbuiltin "bvugt"} bvugt64(bv64, bv64) returns (bool);
function {:extern} {:bvbuiltin "bvule"} bvule64(bv64, bv64) returns (bool);
function {:extern} {:bvbuiltin "bvult"} bvult64(bv64, bv64) returns (bool);
function {:extern} gamma_load32(gammaMap: [bv64]bool, index: bv64) returns (bool) {
  (gammaMap[bvadd64(index, 3bv64)] && (gammaMap[bvadd64(index, 2bv64)] && (gammaMap[bvadd64(index, 1bv64)] && gammaMap[index])))
}

function {:extern} gamma_load64(gammaMap: [bv64]bool, index: bv64) returns (bool) {
  (gammaMap[bvadd64(index, 7bv64)] && (gammaMap[bvadd64(index, 6bv64)] && (gammaMap[bvadd64(index, 5bv64)] && (gammaMap[bvadd64(index, 4bv64)] && (gammaMap[bvadd64(index, 3bv64)] && (gammaMap[bvadd64(index, 2bv64)] && (gammaMap[bvadd64(index, 1bv64)] && gammaMap[index])))))))
}

function {:extern} gamma_load8(gammaMap: [bv64]bool, index: bv64) returns (bool) {
  gammaMap[index]
}

function {:extern} gamma_store32(gammaMap: [bv64]bool, index: bv64, value: bool) returns ([bv64]bool) {
  gammaMap[index := value][bvadd64(index, 1bv64) := value][bvadd64(index, 2bv64) := value][bvadd64(index, 3bv64) := value]
}

function {:extern} gamma_store64(gammaMap: [bv64]bool, index: bv64, value: bool) returns ([bv64]bool) {
  gammaMap[index := value][bvadd64(index, 1bv64) := value][bvadd64(index, 2bv64) := value][bvadd64(index, 3bv64) := value][bvadd64(index, 4bv64) := value][bvadd64(index, 5bv64) := value][bvadd64(index, 6bv64) := value][bvadd64(index, 7bv64) := value]
}

function {:extern} gamma_store8(gammaMap: [bv64]bool, index: bv64, value: bool) returns ([bv64]bool) {
  gammaMap[index := value]
}

function {:extern} memory_load32_le(memory: [bv64]bv8, index: bv64) returns (bv32) {
  (memory[bvadd64(index, 3bv64)] ++ (memory[bvadd64(index, 2bv64)] ++ (memory[bvadd64(index, 1bv64)] ++ memory[index])))
}

function {:extern} memory_load64_le(memory: [bv64]bv8, index: bv64) returns (bv64) {
  (memory[bvadd64(index, 7bv64)] ++ (memory[bvadd64(index, 6bv64)] ++ (memory[bvadd64(index, 5bv64)] ++ (memory[bvadd64(index, 4bv64)] ++ (memory[bvadd64(index, 3bv64)] ++ (memory[bvadd64(index, 2bv64)] ++ (memory[bvadd64(index, 1bv64)] ++ memory[index])))))))
}

function {:extern} memory_load8_le(memory: [bv64]bv8, index: bv64) returns (bv8) {
  memory[index]
}

function {:extern} memory_store32_le(memory: [bv64]bv8, index: bv64, value: bv32) returns ([bv64]bv8) {
  memory[index := value[8:0]][bvadd64(index, 1bv64) := value[16:8]][bvadd64(index, 2bv64) := value[24:16]][bvadd64(index, 3bv64) := value[32:24]]
}

function {:extern} memory_store64_le(memory: [bv64]bv8, index: bv64, value: bv64) returns ([bv64]bv8) {
  memory[index := value[8:0]][bvadd64(index, 1bv64) := value[16:8]][bvadd64(index, 2bv64) := value[24:16]][bvadd64(index, 3bv64) := value[32:24]][bvadd64(index, 4bv64) := value[40:32]][bvadd64(index, 5bv64) := value[48:40]][bvadd64(index, 6bv64) := value[56:48]][bvadd64(index, 7bv64) := value[64:56]]
}

function {:extern} memory_store8_le(memory: [bv64]bv8, index: bv64, value: bv8) returns ([bv64]bv8) {
  memory[index := value[8:0]]
}

function {:extern} {:bvbuiltin "sign_extend 1"} sign_extend1_32(bv32) returns (bv33);
function {:extern} {:bvbuiltin "sign_extend 32"} sign_extend32_32(bv32) returns (bv64);
function {:extern} {:bvbuiltin "zero_extend 1"} zero_extend1_32(bv32) returns (bv33);
function {:extern} {:bvbuiltin "zero_extend 32"} zero_extend32_32(bv32) returns (bv64);
function {:extern} {:bvbuiltin "zero_extend 56"} zero_extend56_8(bv8) returns (bv64);
procedure mmemcpy$InlineInv();
  ensures ((forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i])))) || (forall i : bv64 :: mem_locked[i] == old(mem_locked[i])));

procedure mmemcpy$notRcimpliesRf();
  ensures (!((forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i])))) ==> (forall i: bv64 :: ((bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) || (bvule64(R1, i) && bvult64(i,bvadd64(R1, R2)))) ==> mem[i] == old(mem[i]))));

procedure mmemcpy$Gf();
  ensures (forall i : bv64 :: mem_locked[i] == old(mem_locked[i]));

procedure mmemcpy$GfimpliesGc();
  ensures (forall i: bv64 :: ((mem_locked[i] == 2) ==> (mem[i] == old(mem[i]))));

implementation mmemcpy$GfimpliesGc()
{
  call mmemcpy$Gf();
}

procedure mmemcpy$InlineInvTransitive();
  ensures ((forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i])))) || (forall i : bv64 :: mem_locked[i] == old(mem_locked[i])));

implementation mmemcpy$InlineInvTransitive()
{
  call mmemcpy$InlineInv();
  call mmemcpy$InlineInv();
}

procedure {:extern} rely();
  modifies Gamma_mem, mem;
  ensures (forall i: bv64 :: (((mem[i] == old(mem[i])) ==> (Gamma_mem[i] == old(Gamma_mem[i])))));
  ensures (forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i]))));
  free ensures (memory_load8_le(mem, 2136bv64) == 1bv8);
  free ensures (memory_load8_le(mem, 2137bv64) == 0bv8);
  free ensures (memory_load8_le(mem, 2138bv64) == 2bv8);
  free ensures (memory_load8_le(mem, 2139bv64) == 0bv8);
  free ensures (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free ensures (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free ensures (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free ensures (memory_load64_le(mem, 131120bv64) == 131120bv64);

procedure {:extern} rely_transitive();
  modifies Gamma_mem, mem;
  ensures (forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i]))));

implementation {:extern} rely_transitive()
{
  call rely();
  call rely();
}

procedure {:extern} rely_reflexive();

implementation {:extern} rely_reflexive()
{
  assert (forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i]))));
}

procedure {:extern} guarantee_reflexive();
  modifies Gamma_mem, mem;

implementation {:extern} guarantee_reflexive()
{
  assert (forall i: bv64 :: ((mem_locked[i] == 2) ==> (mem[i] == old(mem[i]))));
}

procedure acquire_lock();
  modifies Gamma_R0, Gamma_R31, Gamma_mem_locked, Gamma_stack, R0, R31, mem_locked, stack;
  requires bvugt64(R0, 0bv64);
  requires bvugt64(R1, R0);
  requires forall_interval(mem_locked, R0, R1, 0);
  free requires (memory_load8_le(mem, 2136bv64) == 1bv8);
  free requires (memory_load8_le(mem, 2137bv64) == 0bv8);
  free requires (memory_load8_le(mem, 2138bv64) == 2bv8);
  free requires (memory_load8_le(mem, 2139bv64) == 0bv8);
  free requires (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free requires (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free requires (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free requires (memory_load64_le(mem, 131120bv64) == 131120bv64);
  free ensures (forall i : bv64 :: mem_locked[i] == (if (bvuge64(i, old(R0)) && bvule64(i, old(R1))) then (1) else (old(mem_locked[i]))));
  free ensures (Gamma_R31 == old(Gamma_R31));
  free ensures (R31 == old(R31));
  free ensures (memory_load8_le(mem, 2136bv64) == 1bv8);
  free ensures (memory_load8_le(mem, 2137bv64) == 0bv8);
  free ensures (memory_load8_le(mem, 2138bv64) == 2bv8);
  free ensures (memory_load8_le(mem, 2139bv64) == 0bv8);
  free ensures (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free ensures (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free ensures (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free ensures (memory_load64_le(mem, 131120bv64) == 131120bv64);

implementation acquire_lock()
{
  lacquire_lock:
    assume {:captureState "lacquire_lock"} true;
    R31, Gamma_R31 := bvadd64(R31, 18446744073709551600bv64), Gamma_R31;
    stack, Gamma_stack := memory_store64_le(stack, bvadd64(R31, 8bv64), R0), gamma_store64(Gamma_stack, bvadd64(R31, 8bv64), Gamma_R0);
    assume {:captureState "%000003a6"} true;
    stack, Gamma_stack := memory_store64_le(stack, R31, R1), gamma_store64(Gamma_stack, R31, Gamma_R1);
    assume {:captureState "%000003ae"} true;
    R0, Gamma_R0 := memory_load64_le(stack, bvadd64(R31, 8bv64)), gamma_load64(Gamma_stack, bvadd64(R31, 8bv64));
    R31, Gamma_R31 := bvadd64(R31, 16bv64), Gamma_R31;
    goto acquire_lock_return;
  acquire_lock_return:
    assume {:captureState "acquire_lock_return"} true;
    return;
}

procedure main();
  modifies Gamma_R0, Gamma_R1, Gamma_R16, Gamma_R17, Gamma_R2, Gamma_R29, Gamma_R30, Gamma_R31, Gamma_mem, Gamma_mem_locked, Gamma_stack, R0, R1, R16, R17, R2, R29, R30, R31, mem, mem_locked, stack;
  requires (forall i : bv64 :: mem_locked[i] == 0);
  requires malloc_count == 0;
  requires R31 == 100000bv64;
  free requires (memory_load8_le(mem, 131112bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131113bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131114bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131115bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131116bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131117bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131118bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131119bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131120bv64) == 48bv8);
  free requires (memory_load8_le(mem, 131121bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131122bv64) == 2bv8);
  free requires (memory_load8_le(mem, 131123bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131124bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131125bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131126bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131127bv64) == 0bv8);
  free requires (memory_load8_le(mem, 131128bv64) == 66bv8);
  free requires (memory_load8_le(mem, 131129bv64) == 65bv8);
  free requires (memory_load8_le(mem, 131130bv64) == 83bv8);
  free requires (memory_load8_le(mem, 131131bv64) == 73bv8);
  free requires (memory_load8_le(mem, 131132bv64) == 76bv8);
  free requires (memory_load8_le(mem, 131133bv64) == 32bv8);
  free requires (memory_load8_le(mem, 131134bv64) == 86bv8);
  free requires (memory_load8_le(mem, 131135bv64) == 101bv8);
  free requires (memory_load8_le(mem, 131136bv64) == 114bv8);
  free requires (memory_load8_le(mem, 131137bv64) == 105bv8);
  free requires (memory_load8_le(mem, 131138bv64) == 102bv8);
  free requires (memory_load8_le(mem, 131139bv64) == 105bv8);
  free requires (memory_load8_le(mem, 131140bv64) == 101bv8);
  free requires (memory_load8_le(mem, 131141bv64) == 114bv8);
  free requires (memory_load8_le(mem, 2136bv64) == 1bv8);
  free requires (memory_load8_le(mem, 2137bv64) == 0bv8);
  free requires (memory_load8_le(mem, 2138bv64) == 2bv8);
  free requires (memory_load8_le(mem, 2139bv64) == 0bv8);
  free requires (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free requires (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free requires (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free requires (memory_load64_le(mem, 131120bv64) == 131120bv64);
  free ensures (Gamma_R29 == old(Gamma_R29));
  free ensures (Gamma_R31 == old(Gamma_R31));
  free ensures (R29 == old(R29));
  free ensures (R31 == old(R31));
  free ensures (memory_load8_le(mem, 2136bv64) == 1bv8);
  free ensures (memory_load8_le(mem, 2137bv64) == 0bv8);
  free ensures (memory_load8_le(mem, 2138bv64) == 2bv8);
  free ensures (memory_load8_le(mem, 2139bv64) == 0bv8);
  free ensures (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free ensures (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free ensures (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free ensures (memory_load64_le(mem, 131120bv64) == 131120bv64);

implementation main()
{
  var #6: bv64;
  var Gamma_#6: bool;
  lmain:
    assume {:captureState "lmain"} true;
    #6, Gamma_#6 := bvadd64(R31, 18446744073709551600bv64), Gamma_R31;
    stack, Gamma_stack := memory_store64_le(stack, #6, R29), gamma_store64(Gamma_stack, #6, Gamma_R29);
    assume {:captureState "%00000487"} true;
    stack, Gamma_stack := memory_store64_le(stack, bvadd64(#6, 8bv64), R30), gamma_store64(Gamma_stack, bvadd64(#6, 8bv64), Gamma_R30);
    assume {:captureState "%0000048d"} true;
    R31, Gamma_R31 := #6, Gamma_#6;
    R29, Gamma_R29 := R31, Gamma_R31;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 70bv64), Gamma_R0;
    R1, Gamma_R1 := R0, Gamma_R0;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 56bv64), Gamma_R0;
    R30, Gamma_R30 := 2032bv64, true;
    call acquire_lock();
    goto l000004bc;
  l000004bc:
    assume {:captureState "l000004bc"} true;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 94bv64), Gamma_R0;
    R1, Gamma_R1 := R0, Gamma_R0;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 80bv64), Gamma_R0;
    R30, Gamma_R30 := 2056bv64, true;
    call acquire_lock();
    goto l000004df;
  l000004df:
    assume {:captureState "l000004df"} true;
    R2, Gamma_R2 := 14bv64, true;
    R0, Gamma_R0 := 131072bv64, true;
    R1, Gamma_R1 := bvadd64(R0, 56bv64), Gamma_R0;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 80bv64), Gamma_R0;
    R30, Gamma_R30 := 2080bv64, true;
    if (*) {
      call mmemcpy$InlineInv();
      call mmemcpy$notRcimpliesRf();
      assert false;
    }
    call mmemcpy();
    goto l00000501;
  l00000501:
    assume {:captureState "l00000501"} true;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 80bv64), Gamma_R0;
    R30, Gamma_R30 := 2092bv64, true;
    call puts();
    goto l00000514;
  l00000514:
    assume {:captureState "l00000514"} true;
    R0, Gamma_R0 := 131072bv64, true;
    R0, Gamma_R0 := bvadd64(R0, 56bv64), Gamma_R0;
    R30, Gamma_R30 := 2104bv64, true;
    call puts();
    goto l00000526;
  l00000526:
    assume {:captureState "l00000526"} true;
    R0, Gamma_R0 := 0bv64, true;
    R29, Gamma_R29 := memory_load64_le(stack, R31), gamma_load64(Gamma_stack, R31);
    R30, Gamma_R30 := memory_load64_le(stack, bvadd64(R31, 8bv64)), gamma_load64(Gamma_stack, bvadd64(R31, 8bv64));
    R31, Gamma_R31 := bvadd64(R31, 16bv64), Gamma_R31;
    goto main_return;
  main_return:
    assume {:captureState "main_return"} true;
    return;
}

procedure mmemcpy();
  modifies Gamma_R0, Gamma_R1, Gamma_R2, Gamma_R31, Gamma_mem, Gamma_stack, R0, R1, R2, R31, mem, stack;
  free requires (memory_load8_le(mem, 2136bv64) == 1bv8);
  free requires (memory_load8_le(mem, 2137bv64) == 0bv8);
  free requires (memory_load8_le(mem, 2138bv64) == 2bv8);
  free requires (memory_load8_le(mem, 2139bv64) == 0bv8);
  free requires (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free requires (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free requires (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free requires (memory_load64_le(mem, 131120bv64) == 131120bv64);
  ensures (forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i, bvadd64(R0, R2))) then gamma_load8((Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))));
  ensures (forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le((mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));
  free ensures (Gamma_R31 == old(Gamma_R31));
  free ensures (R31 == old(R31));
  free ensures (memory_load8_le(mem, 2136bv64) == 1bv8);
  free ensures (memory_load8_le(mem, 2137bv64) == 0bv8);
  free ensures (memory_load8_le(mem, 2138bv64) == 2bv8);
  free ensures (memory_load8_le(mem, 2139bv64) == 0bv8);
  free ensures (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free ensures (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free ensures (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free ensures (memory_load64_le(mem, 131120bv64) == 131120bv64);


procedure puts();
  modifies Gamma_R16, Gamma_R17, R16, R17;
  free requires (memory_load8_le(mem, 2136bv64) == 1bv8);
  free requires (memory_load8_le(mem, 2137bv64) == 0bv8);
  free requires (memory_load8_le(mem, 2138bv64) == 2bv8);
  free requires (memory_load8_le(mem, 2139bv64) == 0bv8);
  free requires (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free requires (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free requires (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free requires (memory_load64_le(mem, 131120bv64) == 131120bv64);
  free ensures (memory_load8_le(mem, 2136bv64) == 1bv8);
  free ensures (memory_load8_le(mem, 2137bv64) == 0bv8);
  free ensures (memory_load8_le(mem, 2138bv64) == 2bv8);
  free ensures (memory_load8_le(mem, 2139bv64) == 0bv8);
  free ensures (memory_load64_le(mem, 130504bv64) == 1872bv64);
  free ensures (memory_load64_le(mem, 130512bv64) == 1792bv64);
  free ensures (memory_load64_le(mem, 131032bv64) == 2000bv64);
  free ensures (memory_load64_le(mem, 131120bv64) == 131120bv64);

