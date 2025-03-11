Globals: 
password: char
buf: long
stext: char[11]

DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32, bvuge64, bvugt64
  
Subroutine: #free
  Requires DIRECT: "(forall i : int, j: bv64 :: (malloc_base[i] == R0 && bvuge64(j, R0) && bvult64(j,  malloc_end[i])) ==> Gamma_mem[j])"

Subroutine: main
Requires DIRECT: "malloc_count == 0"
Requires: Gamma_password == false
Requires DIRECT: "gamma_load32(Gamma_mem, memory_load64_le(mem, $stext_addr))"
Requires DIRECT: "R31 == 100bv64"

Subroutine: malloc
Modifies: R0, malloc_base, malloc_count, malloc_end
Requires DIRECT: "bvugt64(R0, 0bv64)"
Requires DIRECT: "Gamma_R0 == true"
Ensures DIRECT: "Gamma_R0 == true"
Ensures DIRECT: "malloc_count == old(malloc_count) + 1"
Ensures DIRECT: "bvugt64(malloc_end[malloc_count], malloc_base[malloc_count])"
Ensures DIRECT: "R0 == malloc_base[malloc_count]"
Ensures DIRECT: "malloc_end[malloc_count] == bvadd64(R0, old(R0))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> bvugt64(malloc_base[malloc_count], malloc_end[i]) || bvult64(malloc_end[malloc_count], malloc_base[i]))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> malloc_base[i] == old(malloc_base[i]) && malloc_end[i] == old(malloc_end[i]))"
Ensures DIRECT: "bvuge64(R0, 100000000bv64)"
// uninitialised memory is low (free ensures since not part of modifies)
Ensures DIRECT: "(forall i : bv64 :: (bvuge64(i, R0) && bvult64(i, bvadd64(R0, old(R0)))) ==> (Gamma_mem[i] && gamma_load8(Gamma_mem, i)))"


Subroutine: memcpy
  Modifies: mem
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i, bvadd64(R0, R2))) then gamma_load8((Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le((mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"

Subroutine: __memcpy_chk
  Modifies: mem
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i, bvadd64(R0, R2))) then gamma_load8((Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le((mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"
  
// forall i <= n, Gamma_mem[R0] low
Subroutine: memset
  Modifies: mem
  Requires DIRECT: "Gamma_R1"
  Ensures: buf == old(buf) && password == old(password)
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then Gamma_R1 else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))))"



Subroutine: strlen
  Modifies: R0
  Ensures DIRECT: "Gamma_R0 == true"
  Ensures DIRECT: "(forall i: bv64 :: (bvule64(old(R0), i)) && (bvult64(i, bvadd64(old(R0), R0))) ==> mem[i] != 0bv8)"
  Ensures DIRECT: "(memory_load8_le(mem, bvadd64(old(R0), R0)) == 0bv8)"
  Ensures DIRECT: "(bvult64(old(R0), bvadd64(bvadd64(old(R0), R0), 1bv64)))"



