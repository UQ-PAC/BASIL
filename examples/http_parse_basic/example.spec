Globals: 
password: char
buf: long
stext: char[11]


DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32

  
Subroutine: #free
  Requires DIRECT: "(forall i : int, j: bv64 :: malloc_base[i] == R0 && (bvuge64(j, R0) && bvult64(j,  malloc_end[i])) ==> Gamma_mem[j])"
  Ensures DIRECT: "Gamma_R0 == true"


Subroutine: main
Requires: Gamma_password == false
Requires DIRECT: "gamma_load32(Gamma_mem, memory_load64_le(mem, $stext_addr))"
Requires DIRECT: "R31 == 100bv64"

Subroutine: malloc
Modifies: R0, malloc_base, malloc_count, malloc_end
Requires DIRECT: "Gamma_R0 == true"
Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
Ensures DIRECT: "Gamma_R0 == true"
Ensures DIRECT: "malloc_count == old(malloc_count) + 1"
Ensures DIRECT: "bvugt64(malloc_end[malloc_count], malloc_base[malloc_count])"
Ensures DIRECT: "R0 == malloc_base[malloc_count]"
Ensures DIRECT: "malloc_end[malloc_count] == bvadd64(R0, old(R0))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> bvugt64(malloc_base[malloc_count], malloc_end[i]) || bvult64(malloc_end[malloc_count], malloc_base[i]))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> malloc_base[i] == old(malloc_base[i]) && malloc_end[i] == old(malloc_end[i]))"
Ensures DIRECT: "bvuge64(R0, 100000000bv64)"


Subroutine: memcpy
  Modifies: mem
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then old(Gamma_mem[bvadd64(bvsub64(i, R0), R1)]) else old(Gamma_mem[i])))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then old(mem[bvadd64(bvsub64(i, R0), R1)]) else old(mem[i])))"

// forall i <= n, Gamma_mem[R0] low
Subroutine: memset
  Modifies: mem
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then Gamma_R1 else old(Gamma_mem[i])))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(mem[i])))"


Subroutine: strlen
  Modifies: R0
  // doesn't work with the below line removed
  Requires DIRECT: "(memory_load8_le(mem, R0) == 0bv8) || (memory_load8_le(mem, bvadd64(R0, 1bv64)) == 0bv8)|| (memory_load8_le(mem, bvadd64(R0, 2bv64)) == 0bv8)|| (memory_load8_le(mem, bvadd64(R0, 3bv64)) == 0bv8)"
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "Gamma_R0 == true"
  Ensures DIRECT: "(forall i: bv64 :: (bvule64(old(R0), i)) && (bvult64(i, bvadd64(old(R0), R0))) ==> mem[i] != 0bv8)"
  Ensures DIRECT: "(mem[bvadd64(old(R0), R0)] == 0bv8)"
  Ensures DIRECT: "(bvule64(old(R0), bvadd64(old(R0), R0)))"

